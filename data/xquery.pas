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
}
unit xquery;

{
Copyright (C) 2008 - 2019 Benito van der Zander (BeniBela)
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

{$I ../internettoolsconfig.inc}
{$DEFINE ALLOW_EXTERNAL_DOC_DOWNLOAD}


//{$define dumpFunctions}
interface


uses
   Classes, SysUtils,
   simplehtmltreeparser, math, bigdecimalmath, bbutils,
   {$ifdef ALLOW_EXTERNAL_DOC_DOWNLOAD}internetaccess{$endif},
   xquery.internals.common, xquery.internals.collations, xquery.namespaces, xquery__serialization_nodes,
   xquery__functions, xquery__parse //if this is in interface uses rather than implementation uses, fpc compiles xquery.pas first and other units can inline the functions here
   ;



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
  PIXQValue = ^IXQValue;
  TXQVArray = array of IXQValue;
  TXQValueMapLike = class;
  TXQValueFunction = class;
  TXQVariableChangeLog=class;
  TXQEvaluationStack=class;
  TXQTerm=class;
  TXQTermWithChildren=class;
  TXQTermArray = array of TXQTerm;
  TXQTermVariable = class;
  TXQTermSequenceType = class;
  TXQTerm_Visitor = class;
  TXQuery = class;
  IXQuery = interface;
  TXQNativeModule = class;
  TXQValueStandardPropertyEnumerator = class;
  TXQValueStringPropertyEnumerator = class;
  TXQValuePropertyValueEnumerator = class;
  TXQTermDefineVariable = class;
  TXQMapStringObject = class;

  float = record end;
  xqfloat = double;

  //** @abstract(Iterator over PIXQValue.) Faster version of TXQValueEnumerator
  TXQValueEnumeratorPtrUnsafe = record
  private
    fsingleelement: pointer{IXQValue};
    fcurrent, flast: PIXQValue;
    class procedure clear(out enum: TXQValueEnumeratorPtrUnsafe); static;
    class procedure makesingleelement(const v: ixqvalue; out enum: TXQValueEnumeratorPtrUnsafe); static;
    function MoveNextSingleElement: Boolean;
  public
    function MoveNext: Boolean; inline;
    function MoveMany(count: sizeint): Boolean;
    procedure CopyBlock(target: PIXQValue); //**< Copies all XQValue to the destination buffer and increases their ref count. For maximal performance, there is no check that the buffer is large enough
    procedure CopyBlock(target: PIXQValue; count: SizeInt); //**< Copies count XQValue to the destination buffer and increases their ref count.
    procedure CopyToList(list: TXQVList; count: SizeInt);
    property Current: PIXQValue read FCurrent;
    function GetEnumerator: TXQValueEnumeratorPtrUnsafe;
  end;
  //** @abstract(Iterator over an IXQValue.) Usually not used directly, but in a @code(for var in value) construction
  TXQValueEnumerator = record
  private
    fguardian: ixqvalue;
    ptr: TXQValueEnumeratorPtrUnsafe;
    function GetCurrent: IXQValue; inline;
    class procedure clear(out enum: TXQValueEnumerator); static;
  public
    function MoveNext: Boolean; inline;
    property Current: IXQValue read GetCurrent;
    function GetEnumerator: TXQValueEnumerator;
  end;

  //** @abstract(Iterator over an IXQValue.) Usually not used directly, but in a @code(for var in value) construction
  TXQValueEnumeratorArrayTransparentUnsafe = record
  private
    ptrs: array of TXQValueEnumeratorPtrUnsafe;
    currentEnumerator: integer;
    fcurrent: TXQValue;
    //class procedure clear(out enum: TXQValueEnumerator); static;
  public
    function MoveNext: Boolean;
    property Current: TXQValue read FCurrent;
    function GetEnumerator: TXQValueEnumeratorArrayTransparentUnsafe;
  end;

  //**Type of xqvalue (see TXQValue)
  TXQValueKind = (pvkUndefined, pvkBoolean, pvkInt64, pvkFloat, pvkBigDecimal, pvkString, pvkBinary, pvkQName, pvkDateTime, pvkSequence, pvkNode, pvkObject, pvkArray, pvkNull, pvkFunction);
  TXQMapPropertyKeyKind = (xqmpkkStringKeys, xqmpkkStandardKeys );

  TXQTermFlowerOrderEmpty = (xqeoStatic, xqeoEmptyLeast, xqeoEmptyGreatest);
  TXQDefaultNamespaceKind = (xqdnkUnknown, xqdnkAny, xqdnkElementType,  xqdnkType, xqdnkFunction);
  //** If it is possible to access object properties with @code(.property) @br
  //** xqpdnDisallowDotNotation: do not allow the dot operator anywhere @br
  //** xqpdnAllowUnambiguousDotNotation: allow the dot operator when disabling it would be a syntax error. E.g. cases like @code(($obj).property), @code(func(..).property), @code({"a": ..}.property), ...  @br
  //** xqpdnAllowFullDotNotation: allow the dot operator everywhere. Even in $obj.property  @br
  TXQPropertyDotNotation = (xqpdnDisallowDotNotation, xqpdnAllowUnambiguousDotNotation, xqpdnAllowFullDotNotation);

  TTreeNodeSerialization = (tnsText, tnsXML, tnsHTML);

  TXQParsingModel = (xqpmXPath2, xqpmXQuery1, xqpmXPath3_0, xqpmXQuery3_0, xqpmXPath3_1, xqpmXQuery3_1);
  TXQParsingModels = set of TXQParsingModel;
  TXQParsingModelsHelper = type helper for TXQParsingModels
    function requiredModelToString: string;
  end;

  TXQHashmapStrOwningTreeDocument = specialize TXQHashmapStrOwning<TTreeDocument, TTreeDocumentOwnershipTracker>;
  TXQTempTreeNodes = class(TTreeDocument)
  private
    tempnodes: TFPList;
    fdocumentCache: TXQHashmapStrOwningTreeDocument;
    function GetDocumentCache(const url: string): TTreeDocument;
    procedure SetDocumentCache(const url: string; AValue: TTreeDocument);
  protected
    function isHidden: boolean; override;
  public
    constructor create(); reintroduce;
    destructor destroy; override;
    function addClone(n: TTreeNode): TTreeNode;
    property documentCache[url: string]: TTreeDocument read GetdocumentCache write SetdocumentCache;
  end;


  //============================XQUERY CONTEXTS==========================

  //** Static context containing default values and values read during parsing and not changed during evaluation. Mostly corresponds to the "static context" in the XQuery spec
  //**
  //** If a query contains multiple queries (e.g. from importing modules), each query can have its own static context.
  TXQStaticContext = class
  private
    FNodeCollation: TXQCollation;  // default collation used for node name comparisons (extension, does not exist in XQuery)
    function getNodeCollation: TXQCollation;
  public
    sender: TXQueryEngine; //**< Engine this context belongs to

    //The following values map directly to XQuery options declarable in a prolog
    moduleNamespace: INamespace; //**< The namespace of this module or nil
    namespaces: TNamespaceList;  //**< All declared namespaces.
    moduleContextItemDeclarations: array of TXQTermDefineVariable;
    functions: array of TXQValueFunction;   //**< All declared functions. Each function contain a pointer to a TXQTerm and a dynamic context containing a pointer to this staticcontext
    associatedModules: TFPList;
    importedModules: TXQMapStringObject; //**< All imported modules as (prefix, module: TXQuery) tuples
    importedSchemas: TNamespaceList; //**< All imported schemas. Currently they are just treated as to be equivalent to xs: {TODO.}
    defaultFunctionNamespace: TNamespace; //**< Default function namespace (engine default is http://www.benibela.de/2012/pxp/extensions)
    defaultElementTypeNamespace: TNamespace; //**< Default element type namespace (default is empty)
    decimalNumberFormats: TFPList;

    baseURI: string;              //**< Static base uri
    collation: TXQCollation;      //**< Default collation for string comparisons


    stripBoundarySpace: boolean;  //**< If <a>  </a> is equivalent to <a/>. Only used during parsing of the query, ignored during evaluation
    emptyOrderSpec: TXQTermFlowerOrderEmpty;

    copyNamespacePreserve, copyNamespaceInherit: boolean;

    serializationOptions: IXQValue;

    //extensions
    defaultTypeNamespace: TNamespace; //**< Extension: default type namespace. Behaves like the default element type namespace, but does not change the namespace of constructed elements. (default is http://www.w3.org/2001/XMLSchema)
    stringEncoding: TSystemCodePage;    //**< Encoding of strings. Currently only affects the decoding of entities in direct element constructors
    strictTypeChecking: boolean;  //**< Activates strict type checking. If enabled, things like "2" + 3 raise an exception, otherwise it is evaluated to 5. Does not affect *correct* queries (and it makes it slower, so there is no reason to enable this option unless you need compatibility to other interpreters)
    useLocalNamespaces: boolean;  //**< When a statically unknown namespace is encountered in a matching expression it is resolved using the in-scope-namespaces of the possible matching elements
    jsonPXPExtensions: boolean; //**< Allows further json extensions, going beyond jsoniq (especially child and descendant axis test matching object properties) (for dot operator, see TXQParsingOptions) (default is true)
    AllowJSONiqOperations: boolean; //**< Allows implicit casting of objects/arrays to boolean/string.

    model: TXQParsingModel;

    //ignored
    ordering: boolean;  //**< unused
    constructionPreserve: boolean; //**< unused

    procedure assign(sc: TXQStaticContext);
    function clone(): TXQStaticContext; virtual;
    destructor Destroy; override;
    function findSchema(const namespace: string): TXSSchema;
    function findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): TNamespace;
    function findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
    function findNamespaceURLMandatory(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
    procedure splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
    procedure splitRawQName(out namespace: TNamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
    procedure splitRawQName(out namespace: string; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);

    function resolveDocURI(url: string): string;
    function retrieveFromURI(url: string; out contenttype: string; failErrCode: string): string;
    function retrieveFromFile(url: string; out contenttype: string; failErrCode: string): string;
  public
    //internally used
    temporaryNodes: TXQTempTreeNodes;
    function needTemporaryNodes: TXQTempTreeNodes;
    function ImplicitTimezoneInMinutes: integer; inline;
    function CurrentDateTime: TDateTime; inline;
    function findModule(const namespaceURL: string): TXQuery;
    function findModuleStaticContext(const namespaceURL: string): TXQStaticContext;
    function findFunction(const anamespace, alocalname: string; const argcount: integer): TXQValueFunction;
    function findVariableDeclaration( v: TXQTermVariable): TXQTermDefineVariable;
    function findVariableDeclaration(const namespace, varname: string): TXQTermDefineVariable;
    function isLibraryModule: boolean;
  protected
    function compareCommon(a, b: TXQValue; overrideCollation: TXQCollation; castUnknownToString: boolean): TXQCompareResult;
  public
    //**Compares two values atomically (eq,ne,..) and returns 0 if equal, -1 for a < b, and +1 for a > b; -2 for unknown
    function compareAtomic(a, b: TXQValue; overrideCollation: TXQCollation): TXQCompareResult;
    //**Compares two values atomically (eq,ne,..) and returns 0 if equal, -1 for a < b, and +1 for a > b; -2 for unknown
    function compareAtomic(const a, b: IXQValue; overrideCollation: TXQCollation = nil): TXQCompareResult; inline;
    procedure compareAtomic(const a, b: IXQValue; out result: IXQValue; accept1: TXQCompareResult; accept2: TXQCompareResult = xqcrReservedInvalid);
    function equalAtomic(a, b: TXQValue; overrideCollation: TXQCollation): boolean;
    function equalAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): boolean;
    function compareGeneral(const a, b: TXQValueEnumeratorPtrUnsafe; overrideCollation: TXQCollation; accept1: TXQCompareResult; accept2: TXQCompareResult = xqcrReservedInvalid): boolean;
    //**Compares two values (=,!=,...) and returns true if the compare value is \in [accept1,accept2]@br
    //**(Remember that these xpath comparison operators search for a matching pair in the product of the sequences)
    function compareGeneral(a, b: TXQValue; overrideCollation: TXQCollation; accept1: TXQCompareResult; accept2: TXQCompareResult = xqcrReservedInvalid): boolean;
    //**Compares two values (=,!=,...) and returns true if the compare value is \in [accept1,accept2]@br
    //**(Remember that these xpath comparison operators search for a matching pair in the product of the sequences)
    function compareGeneral(a, b: IXQValue; overrideCollation: TXQCollation; accept1: TXQCompareResult; accept2: TXQCompareResult = xqcrReservedInvalid): boolean;
    procedure compareGeneral(a, b: IXQValue; out result: IXQValue; accept1: TXQCompareResult; accept2: TXQCompareResult = xqcrReservedInvalid);
    //**Compares two atomic values and returns 0 as the deepEqual function would if equal, -1 for a < b, and +1 for a > b; -2 for unknown
    function compareDeepAtomic(a, b: TXQValue; overrideCollation: TXQCollation): TXQCompareResult;
    function compareDeepAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): TXQCompareResult; inline;
    function equalDeepAtomic(a, b: TXQValue; overrideCollation: TXQCollation): boolean;
    function equalDeepAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): boolean; inline;
    //**internally used (Returns if the eq operator is defined for the types of a and b)
    class function comparableTypes(const a, b: TXQValue): boolean; static;
  property
    NodeCollation: TXQCollation read getNodeCollation write FNodeCollation;
  end;


  (***
  @abstract(evaluation context, internal used)

  Stores information about the outside scope, needed for correct evaluation of an XQuery-expression
  *)
  TXQEvaluationContext = record
    //important maintaining note: when adding fields update getEvaluationContext
    RootElement: TTreeNode;   //**< associated tree (returned by @code( / ) within an expression)
    ParentElement: TTreeNode; //**< associated tree element (= context item @code( . ), if it is not overriden during the evaluation)
    TextNode: TTreeNode; //**< Use this to override the text node returned by text(). This is useful if you have an element <a>xx<b/>yy</a>. If TextNode is nil text() will return xx, but you can set it to yy. However, ./text() will always return xx.

    SeqValue: IXQValue; //**<Context item / value of @code( . ),  if a sequence is processed (nil otherwise)
    SeqIndex, SeqLength: SizeInt; //**<Position in the sequence, if there is one

    temporaryVariables: TXQEvaluationStack; //**< List of variables defined in the outside scope (e.g. for/same/every)
    globallyDeclaredVariables: TXQVariableChangeLog; //**< List of variables declared variables (e.g. declare variable $foo...) (might be nil)
    namespaces: TNamespaceList;               //**< Namespace declared in the outside scope (only changed by xmlns attributes of constructed nodes)

    staticContext: TXQStaticContext;

    function findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): TNamespace;
    function findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
    procedure splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
    procedure splitRawQName(out namespace: TNamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);

    function getRootHighest: TTreeNode;

    function hasGlobalVariable(const name: string; out value: IXQValue; const namespaceURL: string = ''): boolean;
    function hasGlobalVariable(const v: TXQTermVariable; out value: IXQValue): boolean;
    function getGlobalVariable(const name: string; const namespaceURL: string): IXQValue;
    function getGlobalVariable(const v: TXQTermVariable): IXQValue; inline;

    function parseDoc(const data, url, contenttype: string): TTreeDocument; //for internal use
    function parseCachedDocFromUrl(const url: string; const errorCode: string = 'FODC0002'): TTreeDocument; //for internal use

    function SeqValueAsString: string;
    function contextNode(mustExists: boolean = true): TTreeNode;
    procedure raiseXPDY0002ContextItemAbsent;
  private
    procedure setContextItem(const v: IXQValue);
    procedure setContextItem(const v: IXQValue; index, length: SizeInt);
    procedure getContextItem(out v: IXQValue; out index, length: SizeInt);
  end;
  PXQEvaluationContext = ^TXQEvaluationContext;


  //============================VALUE STORAGE==========================


  TXQValueClass = class of TXQValue;
  //**Record to store a datetime splitted in years/months/days/hours/minutes/secondes/secondfractions+timezone (because TDateTime is not sufficient to distinguish 1a vs. 12m for durations)
  TXQValueDateTimeData = record
    procedure initFromMicroSecondStamp(mics: int64; const tz: integer = high(integer));
    function initFromMicroSecondStampTimeOnly(mics: int64; const tz: integer = high(integer)): int64;
    function toMicroSecondStamp(subtractTimeZone: Boolean = true): int64; //does not include timezone
    function toMonths: integer; inline;
    function toDayTime: int64;
    function hasTimeZone: boolean; inline;
    case boolean of
      true: (values: array[1..7] of integer; );
      false: (year, month, day, hour, min, seconds, microsecs, timezone: integer;);
      //microsecs = fraction scaled by 1000000, timezone in minutes or high(integer) if absent
  end;
  PXQValueDateTimeData = ^TXQValueDateTimeData;

  TXQSerializer = object(TIndentingJSONXHTMLStrBuilder)
    nodeFormat: TTreeNodeSerialization;
    allowDuplicateNames: boolean;

    procedure init(abuffer:pstring; basecapacity: SizeInt = 64; aencoding: TSystemCodePage = {$ifdef HAS_CPSTRING}CP_ACP{$else}CP_UTF8{$endif});

    procedure appendJSONArrayStart;
    procedure appendJSONArrayComma;
    procedure appendJSONArrayEnd;
    procedure appendJSONObjectStart;
    procedure appendJSONObjectKeyColon(const key: string); inline;
    procedure appendJSONObjectComma;
    procedure appendJSONObjectEnd;
    procedure appendJSONString(const s: string);

    procedure appendQualifiedQName(const namespaceurl, local: string);
    procedure appendXQueryString(const s: string);
    procedure appendTypeNameFunctionStart(t: TXSType);

    procedure error(const code: string; value: TXQValue);
    procedure error(const code: string; message: string; value: TXQValue);
  protected
    sequenceTag: string;// = 'seq';
    elementTag: string;// = 'e';
    objectTag: string;// = 'object';
  end;

  type
    TXQMapDuplicateResolve = (xqmdrReject, xqmdrUseFirst, xqmdrUseLast, xqmdrCombine);
    TXQMapDuplicateResolveHelper = type helper for TXQMapDuplicateResolve
      procedure setFromString(const s: string);
    end;

    TXQJsonParser = object
      type
      //**Parsing options
      //**@value jpoAllowMultipleTopLevelItems allow @code([], [], {}) as input
      //**@value jpoLiberal             does not set joStrict for fpc's json scanner
      //**@value jpoAllowTrailingComma  sets joIgnoreTrailingComma for fpc's json scanner
      //**@value jpoEscapeCharacters    escapes characters for XPath/XQuery 3.1, e.g. return strings with \n or \uxxxx
      //**@value jpoJSONiq              change from XPath/XQuery 3.1 parsing mode to JSONiq:
      //**       @table(
      //**       @rowHead(@cell(input)   @cell(XPath/XQuery 3.1)    @cell(  JSONiq))
      //**       @row(@cell(number)      @cell(double  )            @cell(  int, decimal, double))
      //**       @row(@cell(null)        @cell(empty sequence, () ) @cell(  null          ))
      //**       @row(@cell(invalid)     @cell(err:FOJS0001)        @cell(  jerr:JNDY0021))
      //**       )
      TOption = (jpoAllowMultipleTopLevelItems, jpoLiberal, jpoAllowTrailingComma, jpoEscapeCharacters, jpoJSONiq);
      TOptions = set of TOption;
    public
      context: PXQEvaluationContext;
      options: TOptions;
      duplicateResolve: TXQMapDuplicateResolve;
      escapeFunction: TXQValueFunction;
      procedure init;
      procedure setConfigFromMap(const map: IXQValue);
      function parse(argc: SizeInt; args: PIXQValue): IXQValue;
      function parse(const data: string): IXQValue;
      class function parse(const data: string; someOptions: TOptions): IXQValue; static;
    end;
    TXQBatchFunctionCall = record
      tempcontext: TXQEvaluationContext;
      func: TXQValueFunction;
      stack: TXQEvaluationStack;
      stacksize: SizeInt;
      //prepares calls to f on outerContext.
      //It pushes a sufficient number of temp values on the stack (override with stack.topptr), so that the function can be called.
      //Note that pushing can change all stack addresses, invalidating all pointers to the stack (i.e., if you use this in a function called by the interpreter, the function arguments become inaccessible)
      procedure init(const outerContext: TXQEvaluationContext; const f: ixqvalue; const def: IXQValue = nil);
      procedure done;
      function call(): IXQValue; inline;
      function call1(const v: IXQValue): IXQValue;
      function call2(const v, w: IXQValue): IXQValue;
    end;

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
    function toBinaryBytes: TBytes;
    function toXQVList: TXQVList;  //**< Returns a TXQVList of all values contained in the implicit sequence. (if the type is not a sequence, it is considered to be a single element sequence). (this list is not an interface, don't forget to free it! This is the only interface method returning a non-auto-freed value.)
    function toXQuery: string; //**< Converts the value to an XQuery expression that evaluates to an equal value again (intended for debugging, not serialization, so no guarantees)
    function toValue: TXQValue;

    function getSequenceCount: SizeInt;  //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)
    function get(i: SizeInt): IXQValue; //**< Returns the i-th value in this sequence. (non-sequence values are considered to be sequences of length 1) (1-based index)
    function hasProperty(const name: string; value: PXQValue): boolean; //**< Checks if an object has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br The value is returned as TXQValue not IXQValue. (You can pass nil for value, if you don't need the value)
    function hasProperty(const name: IXQValue; value: PXQValue): boolean; //**< Checks if an object has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br The value is returned as TXQValue not IXQValue. (You can pass nil for value, if you don't need the value)
    function getProperty(const name: string): IXQValue; //**< Returns an object property. Returns empty sequence for non objects.
    function getProperty(const name: IXQValue): IXQValue; //**< Returns an object property. Returns empty sequence for non objects.
    function getPropertyKeyKind: TXQMapPropertyKeyKind;
    function getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator; //**< Returns an iterator over all object property values. Raises an exception for non-objects
    function getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator; //**< Returns an iterator over all object properties. Raises an exception for non-objects
    function getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator; //**< Returns an iterator over all object properties. Raises an exception for non-objects
    function getInternalDateTimeData: PXQValueDateTimeData;
    function Size: SizeInt;

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string; deprecated 'use toXQuery'; //**< Returns the value of this value, annotated with its type (e.g. string: abc)
    function jsonSerialize(nodeFormat: TTreeNodeSerialization; insertWhitespace: boolean = false): string; //**< Returns a json representation of this value. Converting sequences to arrays and objects to objects
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string; //**< Returns a xml representation of this value
    procedure jsonSerialize(var serializer: TXQSerializer);
    procedure xmlSerialize(var serializer: TXQSerializer);
    procedure adaptiveSerialize(var serializer: TXQSerializer);
    function serialize(const context: TXQEvaluationContext): string;
    function stringifyNodes: IXQValue; //preliminary
    function hasNodes: boolean;

    function clone: IXQValue; //**< Returns a clone of this value (deep copy). It is also an ref-counted interface, but can be safely be modified without affecting possible other references.
    function GetEnumerator: TXQValueEnumerator; //**< Implements the enumerator for for..in.@br Because it returns an IXQValue, it modifies the reference count of all objects in the sequence. For large sequences this is rather slow (e.g. it wastes 1 second to iterate over 10 million values in a simple benchmark.) and it is recommended to use GetEnumeratorPtrUnsafe. (it took 35ms for those 10 million values, comparable to the 30ms of a native loop not involving any enumerators)
    function GetEnumeratorPtrUnsafe: TXQValueEnumeratorPtrUnsafe; //**< Implements a faster version of GetEnumerator. It does not change any reference counts, not even of self, so it must not be used with values returned by functions!
    function GetEnumeratorMembersPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
    function GetEnumeratorArrayTransparentUnsafe: TXQValueEnumeratorArrayTransparentUnsafe;

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

    property Count: SizeInt read getSequenceCount;


    //**Same as toFloat, but throws an exception if the conversion is not invalid
    function toFloatChecked(scontext: TXQStaticContext): xqfloat;

    //internally used
    procedure enumeratePropertyKeys(var keyset: TXQHashsetStr);
    function enumeratePropertyKeys(): IXQValue;
    function enumeratePropertyValues: IXQValue;
    function setImmutable(const name, value: IXQValue): TXQValueMapLike;  //**< Creates a new object with the same values as the current one and changes a property of it
    function setImmutable(const name: string; const v: IXQValue): TXQValueMapLike;  //**< Creates a new object with the same values as the current one and changes a property of it
    function setImmutable(const name: string; const s: string): TXQValueMapLike;    //**< Creates a new object with the same values as the current one and changes a property of it (string wrapper)
    function hashCode: uint32;
  end;


  { TXQValue }

  (***
  Base class for XQuery-variants types, implementing the IXQValue interface. All other type classes are derived from it. (it correspondes to xs:anyType) @br@br

  See IXQValue for an actual description
  *)
  TXQValue = class(TFastInterfacedObject, IXQValue)
  public
    ftypeAnnotation: TXSType;
    constructor create(atypeAnnotation: TXSType); virtual;
    constructor create(atypeAnnotation: TXSType; const value: IXQValue); virtual;

    class function newinstance : tobject;override;
    procedure FreeInstance;override;

    function kind: TXQValueKind;  //**< Primary type of a value (actually just wraps classKind. Since you cannot define class functions in the interface, but we need to do calculations with types itself)
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
    function toBinaryBytes: TBytes; virtual;
    function toXQVList: TXQVList; virtual; //**< Converts the TXQValue dynamically to a TXQVList sequence (Beware: you have to free the list)
    function toXQuery: string; virtual; //**< Converts the value to an XQuery expression that evaluates to an equal value again (intended for debugging, not serialization, so no guarantees)
    function toValue: TXQValue;

    function getSequenceCount: SizeInt; virtual; //**< Returns the number of values actually contained in this value (0 for undefined, element count for sequences, and  1 for everything else)
    function get(i: SizeInt): IXQValue; virtual; //**< Returns the i-th value in this sequence. (non-sequence values are considered to be sequences of length 1)
    function hasProperty(const {%H-}name: string; {%H-}value: PXQValue): boolean; virtual; //**< Checks if an object has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br The value is returned as TXQValue not IXQValue. (You can pass nil for value, if you don't need the value)
    function hasProperty(const {%H-}name: ixqvalue; {%H-}value: PXQValue): boolean; virtual; //**< Checks if an object has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br The value is returned as TXQValue not IXQValue. (You can pass nil for value, if you don't need the value)
    function getProperty(const name: IXQValue): IXQValue; virtual; //**< Returns an object property. Returns empty sequence for non objects.
    function getProperty(const name: string): IXQValue; virtual; //**< Returns an object property. Returns empty sequence for non objects.
    function getPropertyKeyKind: TXQMapPropertyKeyKind; virtual;
    function getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator; virtual;
    function getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator; virtual;
    function getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator; virtual;
    function getInternalDateTimeData: PXQValueDateTimeData; virtual;
    function Size: SizeInt; virtual;

    function debugAsStringWithTypeAnnotation(textOnly: boolean = true): string; deprecated;
    function jsonSerialize(nodeFormat: TTreeNodeSerialization; insertWhitespace: boolean = false): string;
    function xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string;
    procedure jsonSerialize(var serializer: TXQSerializer); virtual;
    procedure xmlSerialize(var serializer: TXQSerializer); virtual;
    procedure adaptiveSerialize(var serializer: TXQSerializer); virtual;
    function serialize(const context: TXQEvaluationContext): string;
    function stringifyNodes: IXQValue; virtual;
    function hasNodes: boolean; virtual;

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

    function GetEnumerator: TXQValueEnumerator;virtual; //**< Implements the enumerator for for..in. (Only use with IXQValue references, not TXQValue)@br Because it returns an IXQValue, it modifies the reference count of all objects in the sequence. For large sequences this is rather slow (e.g. it wastes 1 second to iterate over 10 million values in a simple benchmark.) and it is recommended to use GetEnumeratorPtrUnsafe. (it took 35ms for those 10 million values, comparable to the 30ms of a native loop not involving any enumerators)
    function GetEnumeratorPtrUnsafe: TXQValueEnumeratorPtrUnsafe;virtual; //**< Implements a faster version of GetEnumerator. It does not change any reference counts, not even of self, so it must not be used with values returned by functions!
    function GetEnumeratorMembersPtrUnsafe: TXQValueEnumeratorPtrUnsafe; virtual;
    function GetEnumeratorArrayTransparentUnsafe: TXQValueEnumeratorArrayTransparentUnsafe; virtual;
  //for internal use:
  public
    class function classKind: TXQValueKind; virtual; //**< Primary type of a value
    function instanceOf(const typ: TXSType): boolean;  //**< If the XPath expression "self instance of typ" should return true
    procedure raiseInternalErrorObjectExpected(const functionname: string);
  public
    procedure enumeratePropertyKeys(var {%H-}keyset: TXQHashsetStr); virtual;
    function enumeratePropertyKeys: IXQValue; virtual;
    function enumeratePropertyValues: IXQValue;       virtual;
    procedure prepareInternetRequest(out method, url, post: string; internet: TInternetAccess);
    function setImmutable(const name, value: IXQValue): TXQValueMapLike; virtual; overload;
    function setImmutable(const {%H-}name: string; const {%H-}v: IXQValue): TXQValueMapLike; virtual;
    function setImmutable(const name: string; const s: string): TXQValueMapLike;   virtual;
    function setImmutable(const {%H-}props: PString; {%H-}len: SizeInt; const {%H-}v: IXQValue): TXQValue; virtual;
    function hashCode: uint32; virtual;
  end;

  TXQValueOwnershipTracker = record
    class procedure addRef(v: TXQValue); static;
    class procedure release(v: TXQValue); static;
    class function hash(const v: IXQValue): uint32; static;
    class function equal(const v, w: IXQValue): boolean; static;
    //assuming v is an atomic value
    class function isKeyStringLike(const v: TXQValue): boolean; static;
    class function isStringLikeAfterAtomize(const v: TXQValue): boolean; static;
    class function isAtomicKeyValue(const v: IXQValue): boolean; static; inline;
  end;

  TXQHashmapStrOwningXQValue = specialize TXQHashmapStrOwning<TXQValue, TXQValueOwnershipTracker>;
  TXQHashmapXQValue = specialize TXQBaseHashmapValuePointerLikeOwning<IXQValue, TXQValue, TXQValueOwnershipTracker, TXQValueOwnershipTracker>;

  { TXQValueUndefined }
  //**undefined/empty sequence
  TXQValueUndefined = class (TXQValue)
    class function classKind: TXQValueKind; override;
    function isUndefined: boolean; override;
    function toArray: TXQVArray; override;
    function toXQVList: TXQVList; override;
    function getSequenceCount: SizeInt; override;
    function clone: IXQValue; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure xmlSerialize(var serializer: TXQSerializer); override; overload;
    procedure adaptiveSerialize(var serializer: TXQSerializer); override;

    function map(const q: string): IXQValue; override;
    function map(const q: string; const vs: array of ixqvalue): IXQValue; override;
    function map(const q: string; const vs: array of string): IXQValue; override;
    function filter(const q: string): IXQValue; override;
    function filter(const q: string; const vs: array of ixqvalue): IXQValue; override;
    function filter(const q: string; const vs: array of string): IXQValue; override;

    function GetEnumeratorPtrUnsafe: TXQValueEnumeratorPtrUnsafe;override;
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
    function hashCode: uint32; override;

    function clone: IXQValue; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure adaptiveSerialize(var serializer: TXQSerializer); override;
  end;


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
    function hashCode: uint32; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure adaptiveSerialize(var serializer: TXQSerializer); override;

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

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt64: Int64; override; //**< Converts the TXQValue dynamically to integer
    function toFloat: xqfloat; override; //**< Converts the TXQValue dynamically to xqfloat
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function toDecimal: BigDecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function hashCode: uint32; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure adaptiveSerialize(var serializer: TXQSerializer); override;

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
    destructor Destroy; override;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toInt64: Int64; override; //**< Converts the TXQValue dynamically to integer
    function toDecimal: BigDecimal; override; //**< Converts the TXQValue dynamically to BigDecimal
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function hashCode: uint32; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure adaptiveSerialize(var serializer: TXQSerializer); override;

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
    destructor Destroy; override;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override;
    function toBooleanEffective: boolean; override;
    function toString: string; override;
    function toDateTime: TDateTime; override;
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function hashCode: uint32; override;

    function clone: IXQValue; override;
  end;


  //** string value
  TXQValueBinary = class (TXQValue)
    encoded:  string;

    constructor create(atypeAnnotation: TXSType; const astr: string); reintroduce;
    destructor Destroy; override;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override;
    function toBooleanEffective: boolean; override;
    function toString: string; override;
    function toDateTime: TDateTime; override;
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function hashCode: uint32; override;

    function toBinaryBytes: TBytes; override;

    class function compare(a, b: TXQValue): TXQCompareResult; static;

    function clone: IXQValue; override;
  end;


  //** QName value (namespace + local part)
  TXQValueQName = class (TXQValue)
    prefix, url, local: string;

    constructor create(atypeAnnotation: TXSType; const aurl, aprefix, alocal: string);
    constructor create(atypeAnnotation: TXSType; const ns: TNamespace; const alocal: string);
    constructor create(const aurl, aprefix, alocal: string);
    constructor create(const aurl, aprefixedLocal: string);
    constructor create(const ns: TNamespace; const alocal: string);
    constructor create(atypeAnnotation: TXSType; const value: IXQValue); override;
    destructor Destroy; override;

    class function classKind: TXQValueKind; override;

    function toString: string; override; //**< Converts the TXQValue dynamically to string (excludes namespace url)
    function toBooleanEffective: boolean; override;
    function hashCode: uint32; override;

    procedure adaptiveSerialize(var serializer: TXQSerializer); override;

    function clone: IXQValue; override;
  end;

  { TXQValueDateTime }



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
    function getInternalDateTimeData: PXQValueDateTimeData; override;
    function hashCode: uint32; override;

    procedure setDateTime(const dateTime: TDateTime);
    class procedure setDateTime(const dateTime: TDateTime; out v: TXQValueDateTimeData); static;

    function clone: IXQValue; override;


    //**A duration can be represented as an integer ("months" = 12 * year + months and "dayTime" = "dayTime" = time since midnight in microseconds)
    //**These set these values
    class procedure setMonths(var duration: TXQValueDateTimeData; m: integer; isDuration: boolean); static;
    class procedure setDayTime(var duration: TXQValueDateTimeData; dt: int64); static;
  protected
    class function tryCreateFromString(const s, format: string; data: PXQValueDateTimeData): TDateTimeParsingResult; static;

    class procedure addDurationDToDateS(const S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);


    procedure truncateRange();

    class function compare(const a,b: TXQValueDateTime; implicitTimezone: integer): TXQCompareResult; static;
//    class procedure subtract(S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);
  end;
  TXQValueDateTimeClass = class of TXQValueDateTime;

  { TXQValueSequence }

  //** Type for a sequence containing an arbitrary number (>= 0) of other IXQValue
  TXQValueSequence = class (TXQValue)
    seq: TXQVList;    //**< list of the contained sequence values.

    constructor create(capacity: SizeInt = 0);
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
    function hashCode: uint32; override;

    function toArray: TXQVArray; override; //**< Returns all elements as array
    function toXQVList: TXQVList; override; //**< Returns all elements as list (which must be freed by the caller)

    function getSequenceCount: SizeInt; override;
    function get(i: SizeInt): IXQValue; override;
    function GetEnumeratorPtrUnsafe: TXQValueEnumeratorPtrUnsafe; override;
    function map(const q: string): IXQValue; override;
    function order(const q: string): IXQValue; override;

    function clone: IXQValue; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure xmlSerialize(var serializer: TXQSerializer); override; overload;
    procedure adaptiveSerialize(var serializer: TXQSerializer); override;
    function stringifyNodes: IXQValue; override;
    function hasNodes: boolean; override;

    procedure add(const value: IXQValue); inline;  //**< Simply adds a value to the sequence (notice that an xpath sequence cannot contain another sequence, so they will be merged)
    procedure addOrdered(const node: IXQValue); inline; //**< Adds a value to a sequence of nodes sorted in document order(notice that an xpath sequence cannot contain another sequence, so they will be merged)

    destructor Destroy; override;
  end;

  //** Type for a node
  TXQValueNode = class (TXQValue)
  private
    fnode: TTreeNode;
    procedure setNode(AValue: TTreeNode);
  public
    property node: TTreeNode read fnode write setNode ;

    constructor create(); reintroduce; virtual;
    constructor create(anode: TTreeNode = nil); reintroduce; virtual;
    destructor destroy; override;

    class function classKind: TXQValueKind; override;

    function toBoolean: boolean; override; //**< Converts the TXQValue dynamically to boolean
    function toBooleanEffective: boolean; override; //**< Returns true
    function toString: string; override; //**< Converts the TXQValue dynamically to string
    function toDateTime: TDateTime; override; //**< Converts the TXQValue dynamically to TDateTime
    function toFloatChecked(scontext: TXQStaticContext): xqfloat; override;
    function toNode: TTreeNode; override; //**< Returns the node
    function hashCode: uint32; override;

    function clone: IXQValue; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure xmlSerialize(var serializer: TXQSerializer); override; overload;
    procedure adaptiveSerialize(var serializer: TXQSerializer); override;
    function stringifyNodes: IXQValue; override;
    function hasNodes: boolean; override;


    //for internal use
    class function nodeTypeAnnotation(tn: TTreeNode): TXSType; static;
    class function nodeTypeAnnotationForAtomic(tn: TTreeNode): TXSType; static;
  end;
                                                               {
  TXQPropertyEnumeratorInternal = class
    vars: TXQVariableChangeLog;
    idx: SizeInt;
    hashmapEnumerator: TXQHashmapStrOwningXQValue.TKeyPairEnumerator;
  end;                                                        }

  { TXQProperty }

  TXQProperty = TXQHashmapStrOwningXQValue.TKeyValuePairOption;
  TXQStandardProperty = TXQHashmapXQValue.TKeyValuePairOption;
  generic TXQValueGenericPropertyEnumerator<TProp> = class
  private
    fcurrent: TProp;
  public
    function MoveNext: Boolean; virtual; abstract;
    property Current: TProp read FCurrent;

    function GetEnumerator: TXQValueGenericPropertyEnumerator;
  end;

  TXQValueStringPropertyEnumerator = class(specialize TXQValueGenericPropertyEnumerator<TXQProperty>) end;
  TXQValueStandardPropertyEnumerator = class(specialize TXQValueGenericPropertyEnumerator<TXQStandardProperty>) end;
  TXQValuePropertyValueEnumerator = class(specialize TXQValueGenericPropertyEnumerator<TXQValue>) end;

  TXQValueMapLike = class (TXQValue)
    class function classKind: TXQValueKind; override;
    function toBooleanEffective: boolean; override;
    function toString: string; override;
    function toJoinedString(const sep: string=' '): string; override;
    //function cloneLinked: TXQValueObject; //**< Creates a weak clone (linked to the current object)
    function setImmutable(const props: PString; len: SizeInt; const v: IXQValue): TXQValueMapLike; overload; override;

    class function newinstance : tobject;override;
    procedure FreeInstance; override;
  end;

  //** String Hash map
  TXQValueStringMap = class (TXQValueMapLike)
    mapdata: TXQHashmapStrOwningXQValue;

    type
      TXQValueStringPropertyEnumeratorForStringMap = class(TXQValueStringPropertyEnumerator)
        enumerator: TXQHashmapStrOwningXQValue.TKeyPairEnumerator;
        constructor create(map: TXQValueStringMap);
        function MoveNext: Boolean; override;
      end;
      TXQValueStandardPropertyEnumeratorForStringMap = class(TXQValueStandardPropertyEnumerator)
        enumerator: TXQHashmapStrOwningXQValue.TKeyPairEnumerator;
        tempEntity: TXQHashmapXQValue.THashMapEntity;
        constructor create(map: TXQValueStringMap);
        function MoveNext: Boolean; override;
      end;
      TXQValuePropertyValueEnumeratorForStringMap = class(TXQValuePropertyValueEnumerator)
        enumerator: TXQHashmapStrOwningXQValue.TKeyPairEnumerator;
        constructor create(map: TXQValueStringMap);
        function MoveNext: Boolean; override;
      end;


    constructor create(); reintroduce; virtual;
    destructor Destroy; override;

    function Size: SizeInt; override;
    function getPropertyKeyKind: TXQMapPropertyKeyKind; override;
    function hasProperty(const name: string; value: PXQValue): boolean; override; //**< Checks if the object (or its prototype) has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br (You can pass nil for value, if you don't need the value)
    function hasProperty(const {%H-}name: ixqvalue; {%H-}value: PXQValue): boolean; override; //**< Checks if an object has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br The value is returned as TXQValue not IXQValue. (You can pass nil for value, if you don't need the value)
    function getProperty(const name: IXQValue): IXQValue; override; //**< Returns an object property. Returns empty sequence for non objects.
    function getProperty(const name: string): IXQValue; override; //**< Returns the value of a property

    function getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator; override;
    function getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator; override;
    function getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator; override;

    procedure setMutable(const name: string; const v: IXQValue); reintroduce; //**< Changes a property
    procedure setMutable(const name: string; const s: string); reintroduce; //**< Changes a property (string wrapper)

    function setImmutable(const name, value: IXQValue): TXQValueMapLike; overload; override;
    function setImmutable(const name: string; const v: IXQValue): TXQValueMapLike; overload; override; //**< Creates a new object with the same values as the current one and changes a property of it

    procedure enumeratePropertyKeys(var keyset: TXQHashsetStr); override;
    function enumeratePropertyValues: IXQValue;       override;

    function clone: IXQValue; override; //**< Creates a hard clone of the object (i.e. also clones all properties)
  end;

  TXQValueStringMapPendingUpdate = class(TXQValueStringMap)
    prototype: IXQValue; //txqvaluestringmap

    type
      TXQHashmapRawEnumerators = array[0..1] of TXQHashmapStrOwningXQValue.TKeyPairEnumerator;
      TXQValueStringPropertyEnumeratorForStringMapPendingUpdate = class(TXQValueStringPropertyEnumerator)
        enumerators: TXQHashmapRawEnumerators;
        constructor create(map: TXQValueStringMapPendingUpdate);
        function MoveNext: Boolean; override;
      end;
      TXQValueStandardPropertyEnumeratorForStringMapPendingUpdate = class(TXQValueStandardPropertyEnumerator)
        enumerators: TXQHashmapRawEnumerators;
        tempEntity: TXQHashmapXQValue.THashMapEntity;
        constructor create(map: TXQValueStringMapPendingUpdate);
        function MoveNext: Boolean; override;
      end;
      TXQValuePropertyValueEnumeratorForStringMapPendingUpdate = class(TXQValuePropertyValueEnumerator)
        enumerators: TXQHashmapRawEnumerators;
        constructor create(map: TXQValueStringMapPendingUpdate);
        function MoveNext: Boolean; override;
      end;

    constructor create(); reintroduce; virtual;
    constructor create(p: TXQValueStringMap); reintroduce; virtual;
    destructor Destroy; override;

    function Size: SizeInt; override;
    function hasProperty(const name: string; value: PXQValue): boolean; override; //**< Checks if the object (or its prototype) has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br (You can pass nil for value, if you don't need the value)
    function getProperty(const name: string): IXQValue; override; //**< Returns the value of a property
  private
    function getEnumerators: TXQHashmapRawEnumerators; inline;
    class function moveNextEnumerators(var enums: TXQHashmapRawEnumerators): boolean; inline; static;
  public
    function getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator; override;
    function getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator; override;
    function getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator; override;

    procedure setMutable(const name: string; const v: IXQValue); reintroduce; //**< Changes a property
    procedure setMutable(const name: string; const s: string); reintroduce; //**< Changes a property (string wrapper)

    function setImmutable(const name: string; const v: IXQValue): TXQValueMapLike; overload; override;

    procedure enumeratePropertyKeys(var keyset: TXQHashsetStr); override;
    function enumeratePropertyValues: IXQValue;       override;

    function clone: IXQValue; override;
  end;

  TXQValueStandardMap = class (TXQValueMapLike)
    mapdata: TXQHashmapXQValue;

    type
      TXQValueStringPropertyEnumeratorForStandardMap = class(TXQValueStringPropertyEnumerator)
        enumerator: TXQHashmapXQValue.TKeyPairEnumerator;
        tempEntity: TXQHashmapStrOwningXQValue.THashMapEntity;
        constructor create(map: TXQValueStandardMap);
        function MoveNext: Boolean; override;
      end;
      TXQValueStandardPropertyEnumeratorForStandardMap = class(TXQValueStandardPropertyEnumerator)
        enumerator: TXQHashmapXQValue.TKeyPairEnumerator;
        constructor create(map: TXQValueStandardMap);
        function MoveNext: Boolean; override;
      end;
      TXQValuePropertyValueEnumeratorForStandardMap = class(TXQValuePropertyValueEnumerator)
        enumerator: TXQHashmapXQValue.TKeyPairEnumerator;
        constructor create(map: TXQValueStandardMap);
        function MoveNext: Boolean; override;
      end;

    constructor create(); reintroduce; virtual;
    constructor create(const othermapdata: TXQHashmapXQValue); reintroduce; virtual;
    destructor Destroy; override;

    function Size: SizeInt; override;
    function getPropertyKeyKind: TXQMapPropertyKeyKind; override;
    function findKeyValuePair(const key: IXQValue): TXQHashmapXQValue.TKeyValuePairOption;
    function hasProperty(const name: string; value: PXQValue): boolean; override; //**< Checks if the object (or its prototype) has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br (You can pass nil for value, if you don't need the value)
    function hasProperty(const key: IXQValue; value: PXQValue): boolean; override; //**< Checks if the object (or its prototype) has a certain property, and returns the property value directly (i.e. changing value^ will change the value stored in the object). @br (You can pass nil for value, if you don't need the value)
    function getProperty(const name: string): IXQValue; override; //**< Returns the value of a property
    function getProperty(const key: IXQValue): IXQValue; override; //**< Returns the value of a property

    function getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator; override;
    function getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator; override;
    function getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator; override;

    procedure setMutable(const name: string; const v: IXQValue); reintroduce; //**< Changes a property
    procedure setMutable(const name: string; const s: string); reintroduce; //**< Changes a property (string wrapper)
    procedure setMutable(const key: IXQValue; const v: IXQValue); reintroduce; //**< Changes a property

    function setImmutable(const name, value: IXQValue): TXQValueMapLike; overload; override;
    function setImmutable(const name: string; const v: IXQValue): TXQValueMapLike; overload; override; //**< Creates a new object with the same values as the current one and changes a property of it

    function enumeratePropertyKeys: IXQValue; override;
    procedure enumeratePropertyKeys(var keyset: TXQHashsetStr); override;
    function enumeratePropertyValues: IXQValue;       override;

    function clone: IXQValue; override; //**< Creates a hard clone of the object (i.e. also clones all properties)
  end;


  //** JSON array of other IXQValue
  TXQValueJSONArray = class (TXQValue)
    seq: TXQVList;

    constructor create(takeList: TXQVList); reintroduce; virtual;
    constructor create(capacity: SizeInt = 0); reintroduce; virtual;

    class function classKind: TXQValueKind; override;

    function isUndefined: boolean; override;

    function GetEnumeratorMembers: TXQValueEnumerator;
    function GetEnumeratorMembersPtrUnsafe: TXQValueEnumeratorPtrUnsafe; override;
    function Size: SizeInt; override;

    function toBooleanEffective: boolean; override;
    function toString: string; override;
    function toJoinedString(const sep: string=' '): string; override;

    function clone: IXQValue; override;

    function setImmutable(const props: PString; len: SizeInt; const v: IXQValue): TXQValue; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure xmlSerialize(var serializer: TXQSerializer); override; overload;
    procedure adaptiveSerialize(var serializer: TXQSerializer); override;
    function stringifyNodes: IXQValue; override;
    function hasNodes: boolean; override;

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
    function hashCode: uint32; override;

    procedure jsonSerialize(var serializer: TXQSerializer); override; overload;
    procedure xmlSerialize(var serializer: TXQSerializer); override; overload;
  end;

  { TXQFunctionParameter }

  TXQFunctionParameter = record
    variable: TXQTermVariable;
    seqtype: TXQTermSequenceType;
    function toString(def: string = '?'): string;
  end;

  TXQEQName = class
    namespaceURL: string;
    localname: string;
    constructor create;
    constructor create(const ns, local: string);
    function clone: TXQEQName;
    function ToString: ansistring; override;
    function isEqual(const ns, local: string): boolean; inline;
  end;

  TXQEQNameWithPrefix = class(TXQEQName)
    namespacePrefix: string;
    constructor create;
    constructor create(const ns: INamespace; const local: string);
    constructor create(const nsurl, nsprf, local: string);
    function clone: TXQEQNameWithPrefix;
  end;

  TXQAnnotation = record
    name: TXQEQName;
    params: array of TXQTerm;
  end;
  TXQAnnotations = array of TXQAnnotation;

  //** A function. Anonymous or a named reference. Also used to store type information
  TXQValueFunction = class (TXQValue)
    name, namespaceURL, namespacePrefix: string;
    parameters: array of TXQFunctionParameter;
    resulttype: txqtermsequencetype;
    body: TXQTerm;
    ownsTerms: boolean;
    context: TXQEvaluationContext;
    annotations: TXQAnnotations;

    constructor create(aterm: TXQTerm = nil); reintroduce; virtual;
    procedure FreeInstance; override;
    destructor Destroy; override;

    class function classKind: TXQValueKind; override;

    function toBooleanEffective: boolean; override;

    //term is used for error handling, arguments are on the stack
    function evaluate(const outerContext: TXQEvaluationContext; const term: TXQTerm): IXQValue; //**< Calls the function with the given arguments. Evaluation context is the context the function was defined in.
    function evaluateInContext(var inContext: TXQEvaluationContext; const term: TXQTerm): IXQValue; //**< Calls the function with the given arguments. Evaluation context is the context the function was defined in.
    procedure contextOverrideParameterNames(const inContext: TXQEvaluationContext; count: SizeInt);

    function directClone: TXQValue;
    function clone: IXQValue; override;
    function toXQuery: string; override;
    function debugAsStringWithTypeAnnotation(textOnly: boolean=true): string;

    procedure assignCopiedTerms(const func: TXQValueFunction); //for internal use

    procedure adaptiveSerialize(var serializer: TXQSerializer); override;

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
  TXSCastingError = (xsceNoError, xsceXPTY0004, xsceFORG0001, xsceFOCA0002, xsceFODT0001, xsceFODT0002, xsceFOAR0002);
  //** General XML schema type
  TXSType = class
    name: string;
    schema: TXSSchema;
    base: TXSType;
    storage: TXQValueClass;

    whiteSpaceFacet: TXSConstrainingFacetWhitespace;
    whiteSpaceFixed: boolean;

    //types in baseSchema are numbered and descendantsIds is a bit field of all descendent types
    //imported types would have id 0
    id: integer;
    descendantsIds: int64;


    constructor Create(aname: string; aparent: TXSType = nil; astorage: TXQValueClass = nil; aschema: TXSSchema = nil);

    //function isAtomic: boolean; virtual;
    function derivedFrom(t: TXSType): boolean;
    function derivedFrom(t: array of TXSType): boolean;
    function containsTransitive(t: TXSType): boolean; virtual;

    class function commonType(a, b: TXSType): TXSType; static;
    class function commonType(const a, b: IXQValue): TXSType; static;

    function getIntegerType: TXSType; virtual;
    class function commonIntegerType(const a,b: TXSType): TXSNumericType; static;
    class function commonIntegerType(const a,b: IXQValue): TXSNumericType; inline; static;
    function getDecimalType: TXSType; virtual;
    class function commonDecimalType(a,b: TXSType; const failureType: TXSType): TXSType; //static;
    class function commonDecimalType(const a,b: IXQValue): TXSType; static;

  private
    procedure failCreateValue(err: TXSCastingError; v: int64);
  public

    //** Creates a new value from the argument array (directly maps to the xs:something constructors of XPath)
    function createValue(const v: IXQValue): IXQValue; inline;
    function createValue(const v: Int64): IXQValue; inline;
    function createValue(const v: xqfloat): IXQValue;
    function createValue(const v: BigDecimal): IXQValue;
    function createValue(const v: String): IXQValue; inline;

    function tryCreateValue(const v: IXQValue; outv: PXQValue = nil): TXSCastingError;
  protected

    function tryCreateValue(v: string; outv: PXQValue = nil): TXSCastingError;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError; virtual;
    function tryCreateValueInternal(const v: String; outv: PXQValue = nil): TXSCastingError; virtual;
    function tryCreateValue(const v: Int64; outv: PXQValue = nil): TXSCastingError; virtual;
    function tryCreateValue(const v: xqfloat; outv: PXQValue = nil): TXSCastingError; virtual;
    function tryCreateValue(const v: BigDecimal; outv: PXQValue = nil): TXSCastingError; virtual;

    function xsceXPTY0004ButTryCreatingFromAFakeSingleton(const v: IXQValue; outv: PXQValue): TXSCastingError;
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
    variety: (xsvAtomic, xsvAbsent, xsvList, xsvUnion);
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
    members: array of TXSType; //atomic types
    constructor Create(aname: string; aparent: TXSType=nil; astorage: TXQValueClass=nil; amembers: array of TXSType);
    function containsTransitive(t: TXSType): boolean; override;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue=nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: String; outv: PXQValue=nil): TXSCastingError; override;
  end;

  { TXSListType }

  //** XML Schema list type
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
    function constraintsSatisfied(const v: int64): boolean;
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
    lexicalSpaceRegex: TObject {TWrappedRegExpr};
    lexicalSpacePattern: string;
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
    function constraintsSatisfied(const v: TXQValueDateTimeData): boolean;
    function tryCreateValueInternal(const v: IXQValue; outv: PXQValue = nil): TXSCastingError; override;
    function tryCreateValueInternal(const v: string; outv: PXQValue = nil): TXSCastingError; override;
    constructor Create(aname: string; aparent: TXSType; apattern: string; atruncation: TXQDateTimeTruncation = xqdttNone );
  end;

  TXQMapStringOwningObject = class(TStringList)
    constructor Create;
    function DoCompareText(const s1, s2: string): PtrInt; override;
  end;
  TXQMapStringObject = class(TXQMapStringOwningObject)
    constructor Create;
  end;


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
    untypedOrNodeUnion: TXSUnionType;

    //3.1 only
    error: TXSSimpleType;
    numeric: TXSUnionType;

    constructor Create;
    destructor Destroy; override;
    function findType(const typeName: string): TXSType;

    //for internal use
    class function isValidNCName(const s: string): boolean; static; inline;
    class function isValidQName(s: string): boolean;  static;
    function isValidationOnlyType(t: TXSType): boolean;
    function isAbstractType(t: TXSType): boolean;


    procedure show(const s: string); //do not use
    procedure hide(const s: string); //do not use
  private
    typeList, hiddenTypeList: TXQMapStringOwningObject;
    procedure cacheDescendants;
    {$ifdef dumpFunctions}procedure logConstructorFunctions;{$endif}
  end;

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

  (*** @abstract(List of TXQValue-s). Can store any xqvalue, even nested sequences *)
  TXQVCustomList = class(specialize TFastInterfaceList<IXQValue>)
  protected
    procedure sortInDocumentOrderUnchecked; //**< Sorts the nodes in the list in document order. Does not check if they actually are nodes
    procedure insertSingle(i: SizeInt; const child: IXQValue); inline; //**< Inserts a IXQValue to the sequence. Does not perform sequence flattening
  public
    procedure addInArray(const value: IXQValue); inline;

    procedure revert; //**< Reverts the list
    procedure sort(cmp: TPointerCompareFunction; data: TObject = nil); //**< Sorts the list
  end;

  (*** @abstract(List of TXQValue-s). If a sequence is inserted/added, it is flattened, so only the contained items are added  *)
  TXQVList = class(TXQVCustomList)
    constructor create(acapacity: SizeInt = 0);
    constructor create(other: TXQVCustomList);
    procedure insert(i: SizeInt; value: IXQValue); reintroduce; //**< Adds an IXQValue to the sequence. (Remember that XPath sequences are not allowed to store other sequences, so if a sequence it passed, only the values of the other sequence are added, not the sequence itself)
    procedure add(const value: IXQValue); reintroduce; //**< Adds an IXQValue to the sequence. (Remember that XPath sequences are not allowed to store other sequences, so if a sequence it passed, only the values of the other sequence are added, not the sequence itself)
    procedure addOrdered(const node: IXQValue); //**< Adds an IXQValue to a node sequence. Nodes are sorted in document order and duplicates are skipped. (Remember that XPath sequences are not allowed to store other sequences, so if a sequence it passed, only the values of the other sequence are added, not the sequence itself)
    procedure add(node: TTreeNode);
    procedure add(list: TXQVCustomList);
    procedure addOrdered(list: TXQVList);
    function stringifyNodes: TXQVList;
    function hasNodes: boolean;
  end;

  {$define TRACK_STACK_VARIABLE_NAMES}
  TXQEvaluationStack = class(TXQVCustomList)
  private
    {$ifdef TRACK_STACK_VARIABLE_NAMES}debugNames: array of string;{$endif}
    procedure failTop(const name: TXQTermVariable; i: SizeInt = 0);
  public
    procedure push(const value: ixqvalue);
    procedure pop();
    procedure popTo(newCount: SizeInt);
    function top(i: SizeInt = 0): IXQValue;
    function topptr(i: SizeInt = 0): PIXQValue; inline;

    procedure push(const name: TXQTermVariable; const v: ixqvalue); inline;
    function top(const name: TXQTermVariable; i: SizeInt = 0): IXQValue; inline;
  end;

  (***
    @abstract(Basic/pure function, taking some TXQValue-arguments and returning a new IXQValue.)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQBasicFunction = function (argc: SizeInt; argv: PIXQValue): IXQValue;
  (***
    @abstract(Function, taking some TXQValue-arguments and returning a new TXQValue which can depend on the current context state)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQComplexFunction = function (const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
  (***
    @abstract(Binary operator of TXQValues)
    It should not modify the values passed in the args in case there are other references, but it may assign one of them to result.
  *)
  TXQBinaryOp = function (const cxt: TXQEvaluationContext; const a,b: IXQValue): IXQValue;



  TXQFunctionParameterTypes = record
    types: array of TXQTermSequenceType;
    returnType: TXQTermSequenceType;
    function serialize(separator: string = ''): string;
    function clone: TXQFunctionParameterTypes;
    procedure raiseErrorMessage(values: PIXQValue; count: SizeInt; const context: TXQEvaluationContext; term: TXQTerm; const addendum: string);
    procedure checkOrConvertTypes(values: PIXQValue; count: SizeInt; const context:TXQEvaluationContext; term: TXQTerm);
  end;
  PXQFunctionParameterTypes = ^TXQFunctionParameterTypes;

  //**The dynamic/static context values a query depends on (internal used for optimizations)
  //**xqcdFocusItem: context item/node (e.g. .)
  //**xqcdFocusPosition: index (e.g. position())
  //**xqcdFocusLast: size (e.g. last())
  //**xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther: context, obvious
  TXQContextDependency = (xqcdFocusItem,  xqcdFocusPosition, xqcdFocusLast, xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther);
  TXQContextDependencies = set of TXQContextDependency;

  { TXQAbstractFunctionInfo }

  TXQAbstractFunctionInfo = class
    name: string;
    minArgCount, maxArgCount: word;
    versions: array of TXQFunctionParameterTypes;
    sharedVersions: boolean;
    //used for user defined functions where the parameters must be promoted to the right type
    class procedure convertType(var result: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext; term: TXQTerm); static;
    //used for native functions (which should be robust enough to handle different types on the Pascal side)
    class function checkType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext): boolean; static;
    function getVersion(arity: integer): PXQFunctionParameterTypes;
    procedure setVersionsShared(const v: array of TXQFunctionParameterTypes);
    procedure setVersionsShared(const v: array of TXQTermSequenceType);
    procedure setVersionsShared(const v,w: array of TXQTermSequenceType);
    procedure setVersionsShared(i: integer; const v: array of TXQTermSequenceType);
    procedure setVersionsShared(count: integer);
    function checkOrConvertTypes(values: PIXQValue; count: integer; const context:TXQEvaluationContext; term: TXQTerm): integer;
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
  //**Information about an xquery binary operator
  TXQOperatorFlags = set of (xqofAssociativeSyntax, //if the syntax is associative. (not the semantic!). e.g. @code(1 + 2 + 3) is valid, but @code(1 eq 2 = true()) is a syntax error;
                             xqofCastUntypedToString,
                             xqofCastUntypedToDouble,
                             xqofSpecialParsing
                             );
  TXQOperatorInfo = class(TXQAbstractFunctionInfo)
    func: TXQBinaryOp;
    priority: integer;
    flags: TXQOperatorFlags;
    followedBy: string;
    contextDependencies: TXQContextDependencies;
    acceptedModels: TXQParsingModels;
  end;


  TXQPathMatchingKind = (qmValue, qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument,
                         qmSchemaFail {schema matching is not supported},
                         qmCheckNamespaceURL, qmCheckNamespacePrefix, qmCheckOnSingleChild);
  TXQPathMatchingKinds = set of TXQPathMatchingKind;
  TXQPathMatchingStepFilter = record
    filter: TXQTerm;
    dependencies: TXQContextDependencies;
  end;
  TXQPathMatchingStepFilters = array of TXQPathMatchingStepFilter;
  //***@abstract(Step of a query in a tree)
  //***You can use it to use queries, but it is intended for internal use
  TXQPathMatchingStep = record
    namespaceURLOrPrefix: string; //**< Namespace the matched node must be in (only used if qmCheckNamespace is set)
    value: string; //**< If @code(value <> ''), only nodes with the corresponding value are found (value = node-name for element node, value = text for text/comment nodes)
    valueHash: cardinal;
    filters: TXQPathMatchingStepFilters; //**< expressions a matched node must satisfy
    requiredType: TXQTermSequenceType;
    function serialize: string;
    function clone: TXQPathMatchingStep;
    function namespaceChecked: boolean;
    procedure destroy;
    case typ: TTreeNodeEnumeratorAxis of  //**< Axis, where it searchs for a matching tree node
    tneaSameNode: (matching: TXQPathMatchingKinds;); //**< Which nodes match the query command. If this is [], _nothing_ is found! The elements of the set [qmElement,qmText,qmComment,qmProcessingInstruction,qmAttribute] match nodes of a certain type, qmValue activates the value field.
    tneaFunctionSpecialCase: (specialCase: TXQTerm; );   //**< Term used for qcFunctionSpecialCase
  end;
  TXQPathMatching = array of TXQPathMatchingStep;

  TTreeElementTypes = set of TTreeNodeType;
  TXQPathNodeConditionIteration = (qcnciNext, qcnciPreceding, qcnciParent);

  TXQPathNodeConditionOption = (xqpncCheckValue, xqpncCheckNamespace, xqpncCheckOnSingleChild);
  TXQPathNodeConditionOptions = set of TXQPathNodeConditionOption;
  //** Record mapping
  TXQPathNodeCondition = object(TTreeNodeEnumeratorConditions)
    options: TXQPathNodeConditionOptions;
    searchedTypes: TTreeElementTypes; //**< Treeelement types matched by the query
    requiredValue: string; //**< Required node name (if checkValue)
    requiredValueHash: cardinal;
    requiredNamespaceURL: string; //**< Required namespace (if checkNamespace)
    requiredType: TXQTermSequenceType;
    equalFunction: TStringComparisonFunc; //**< Function used to compare node values with the required values
    documentElementSubCondition: ^TXQPathNodeCondition;

    function nodeMatchesExtendedConditions(node: TTreeNode): boolean;
    function getNextNode(prev: TTreeNode): TTreeNode; reintroduce;
    procedure init(const contextNode: TTreeNode; const command: TXQPathMatchingStep); reintroduce;
  end;








  //============================XQUERY AST TERMS==========================


  PXQTerm = ^TXQTerm;
  PXQTermVariable = ^TXQTermVariable;
  TXQTerm_VisitAction = (xqtvaContinue, xqtvaAbort, xqtvaNoRecursion);//xqtvaNothing, xqtvaDeleteWithChildren, xqtvaDeleteLonely);

  { TXQTerm_Visitor }

  TXQTerm_Visitor = class
    parent: TXQTerm;
    procedure declare({%H-}intentionallyUnusedParameter: PXQTermVariable); virtual;
    procedure undeclare({%H-}intentionallyUnusedParameter: PXQTermVariable); virtual;
    function visit ({%H-}intentionallyUnusedParameter: PXQTerm): TXQTerm_VisitAction; virtual;
    function leave ({%H-}intentionallyUnusedParameter: PXQTerm): TXQTerm_VisitAction; virtual;

    class function startVisiting(term: PXQTerm): TXQTerm_VisitAction;
    function simpleTermVisit (term: PXQTerm; theparent: TXQTerm): TXQTerm_VisitAction; //do not call, for internal use
  protected
    procedure replace(term: PXQTerm; newterm: TXQTerm); inline;
  private
    procedure declare(v: PXQTermVariable; theparent: TXQTerm); inline;
    procedure undeclare(v: PXQTermVariable; theparent: TXQTerm); inline;
    function simpleTermVisitNoRecurse (term: PXQTerm; theparent: TXQTerm): TXQTerm_VisitAction; //do not call, for internal use
  end;
  TXQTerm_VisitorClass = class of TXQTerm_Visitor;
  //**@abstract Internally used xpath term

  TXQTerm = class
    function evaluate(var context: TXQEvaluationContext): IXQValue; virtual;
    function getContextDependencies: TXQContextDependencies; virtual;
    function debugTermToString: string; virtual;
  public
    //for internal use
    procedure raiseParsingError(const errcode, s: string);
    procedure raiseEvaluationError(const errcode, s: string);
    procedure raiseTypeError0004(const s: string);
    procedure raiseTypeError0004(const s: string; const got: IXQValue);

    function visitchildren({%H-}intentionallyUnusedParameter: TXQTerm_Visitor): TXQTerm_VisitAction; virtual;
    function clone: TXQTerm; virtual;
  end;
  TXQTermClass = class of TXQTerm;

  TXQTermWithChildren = class(TXQTerm)
    children: array of TXQTerm;
    function getContextDependencies: TXQContextDependencies; override;
    function debugTermToString: string; override;
    destructor destroy; override;

    //for internal use
    procedure push(t: TXQTerm);
    function push(t: array of TXQTerm): TXQTerm;
  protected
    function getChildrenContextDependencies: TXQContextDependencies; virtual;
  public
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
  end;
  TXQTermWithChildrenOnStack = class(TXQTermWithChildren)
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
  end;

  { TXQTermNumber }

  { TXQTermConstant }

  TXQTermConstant = class sealed(TXQTerm)
    value: IXQValue;
    constructor createNumber(const avalue: string);
    constructor create(const avalue: string);
    constructor create(const avalue: IXQValue);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermSequence }

  TXQTermSequence = class(TXQTermWithChildrenOnStack)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermArray }

  TXQTermArrayBase = class(TXQTermWithChildren)
    function getContextDependencies: TXQContextDependencies; override;
  end;
  TXQTermJSONArray = class(TXQTermArrayBase)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
  end;
  TXQTermArray3_1 = class(TXQTermArrayBase)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
  end;
  TXQTermJSONiqArray = class(TXQTermArrayBase)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
  end;

  { TXQTermType }

  type
  TXQTypeInformationKind = (tikNone, tikAny, tikAtomic, tikFunctionTest, tikElementTest, tikArrayTest, tikMapTest, tikJSONiqTest, tikUnion);

  { TXQTermSequenceType }

  TXQTermSequenceType = class(TXQTermWithChildren)
    name: string;
    allowNone, allowMultiple: boolean; //?, *, and + modifiers. (note: tikNone/empty-sequence() does not have allowNone set)
    kind: TXQTypeInformationKind;
    atomicTypeInfo: TXSType; //used for tikAtomic (or tikFunctionTest during parsing) or tikJSONiqTest
    nodeMatching: TXQPathMatchingStep; //only for tikElementTest
    arguments: array of TXQTermSequenceType; //only for tikFunctionTest, last is return type

    constructor create();
    constructor create(akind: TXQTypeInformationKind);
    constructor create(atomic: TXSType; aallowNone: boolean = false);
    destructor destroy; override;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function serialize: string;
  public
    //for internal use
    function clone: TXQTerm; override;

    function isSingleType(): boolean; //test if ti is SingleType(XPATH) = AtomicType(XPATH) "?" ?
    function castableAsBase(v: IXQValue; staticContext: TXQStaticContext): boolean;
    function castAs(v: IXQValue; const context: TXQEvaluationContext): IXQValue;
    function castableAs(v: IXQValue; staticContext: TXQStaticContext): boolean;
    function instanceOf(const ta: IXQValue; const context: TXQEvaluationContext): boolean;
    //function instanceOf(const ta: IXQValue): boolean;
    function instanceOf(const node: TTreeNode): boolean;
    function subtypeOf(tb: TXQTermSequenceType): boolean;
  private
    class function subtypeItemTypeAtomic(xsa, xsb: TXSType): boolean; static;
    //if self is a subtype-itemtype of tb
    function subtypeItemTypeOf(tb: TXQTermSequenceType): boolean;
    function functionCoercion(const v: IXQValue): IXQValue;
    function isItemStar(): boolean;
    function isAtomicType(a: TXSType; shouldAllowNone: boolean = false;  shouldAllowMultiple: boolean = false): boolean;
  end;

  { TXQTermVariable }

  TXQNamespaceMode = (xqnmNone, xqnmURL, xqnmPrefix);

  TXQTermEQNameToken = class(TXQTerm)
    namespaceurl, namespaceprefix, localpart: string;
    constructor Create;
    constructor Create(const anamespaceurl, anamespaceprefix, alocalpart: string);
    function clone: TXQTerm; override;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function debugTermToString: string; override;
  end;

  TXQTermPendingEQNameTokenPending = (xqptUnknown, xqptVariable, xqptAttribute, xqptElement);
  TXQTermPendingEQNameToken = class(TXQTermEQNameToken )
    mode: TXQNamespaceMode;
    data: integer;
    pending: TXQTermPendingEQNameTokenPending;
    constructor create;
    constructor create(anamespaceurl, aprefix, alocalpart: string; amode: TXQNamespaceMode; somedata: integer = 0);
    constructor create(anamespaceurl, aprefix, alocalpart: string; amode: TXQNamespaceMode; realterm: TXQTermPendingEQNameTokenPending);
    function resolveURI(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind = xqdnkUnknown): string;
    function resolveAndFree(const staticContext: TXQStaticContext): TXQTerm;
    function clone: TXQTerm; override;
  end;


  TXQTermVariable = class(TXQTerm)
    namespace: string;
    value: string; //this is the name, not the value
    index: SizeInt;

//    constructor create(const avalue: string; staticContext: TXQStaticContext);
    constructor create(const avalue: string; const anamespace: TNamespace = nil);
    constructor create(const alocalname: string; const anamespace: string);
    function equalsVariable(v: TXQTermVariable): boolean;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    function ToString: ansistring; override;
  end;

  TXQTermVariableGlobal = class(TXQTerm)
    definition: TXQTermDefineVariable;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    function ToString: ansistring; override;
  private
    function evaluateInitial(var context: TXQEvaluationContext): IXQValue;
  end;
  TXQTermVariableGlobalImported = class(TXQTermVariableGlobal)
    staticContext: TXQStaticContext;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function clone: TXQTerm; override;
  end;

  TXQTermDefineVariable = class(TXQTermWithChildren)
    variable: TXQTerm;
    annotations: TXQAnnotations;
    constructor create(avarname: string; anamespace: TNamespace);
    constructor create(vari: TXQTerm; value: TXQTerm = nil);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getClassicValue(var context: TXQEvaluationContext): IXQValue;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;

    function getVariable: TXQTermVariable;
    function getExpression: TXQTerm;
    function getSequenceType: TXQTermSequenceType;
    function isExternal: boolean;

    destructor destroy; override;
  end;


  { TXQTermDefineFunction }
  TXQTermDefineFunctionKind = (xqtdfUserDefined, xqtdfNamedReference, xqtdfStaticPartialApplication, xqtdfDynamicPartialApplication);
  TXQTermNamedFunction = class;
  TXQTermDefineFunction = class(TXQTermWithChildren)
    name: TXQEQNameWithPrefix;
    parameterCount: integer;
    annotations: TXQAnnotations;
    kind: TXQTermDefineFunctionKind;
    constructor createReference(const fun: TXQTermNamedFunction; arity: integer);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function define(var context: TXQEvaluationContext; const clearFocus: boolean): TXQValueFunction;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  private
    initialized: boolean;
    function findNamedFunctionVersion(const context: TXQEvaluationContext): PXQFunctionParameterTypes;
    procedure initNamedFunctionReference(const context: TXQEvaluationContext);
    function defineStaticPartialApplication(var context: TXQEvaluationContext): TXQValueFunction;
    function defineDynamicPartialApplication(var context: TXQEvaluationContext; f: TXQValueFunction): TXQValueFunction;
  end;

  TXQTermPlaceholderVariable = class sealed(TXQTerm)
  end;

  TXQTermContextItem = class(TXQTerm)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function debugTermToString: string; override;
    function clone: TXQTerm; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  TXQTermNodeMatcherDirect = (xqnmdRoot, xqnmdParent);
  TXQTermNodeMatcher = class(TXQTerm)
    queryCommand: TXQPathMatchingStep;
    constructor Create();
    constructor Create(direct: TXQTermNodeMatcherDirect);
    constructor Create(const aaxis: string; const avalue: string);
    destructor Destroy; override;
    procedure setNamespace(namespaceCheck: TXQNamespaceMode; namespaceURLOrPrefix: string);
    procedure setAxis(const axis: string);

    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function debugTermToString: string; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermFilterSequence }

  TXQTermFilterSequence = class(TXQTermWithChildren)
    constructor create(seq: TXQTerm; filter: TXQTerm = nil);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;


  { TXQTermPatternMatcher }

  TXQTermPatternMatcher = class (TXQTerm) //a node temporarily used in a query (it cannot be kept the query, since it is destroyed with the query)
    node: TTreeNode;
    vars: array of TXQTermVariable;
    hasDefaultVariable: boolean;
    contextDependancies: TXQContextDependencies;
    constructor Create;
    constructor Create(adoc: TTreeNode);
    function clone: TXQTerm; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    destructor destroy; override;

    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  TXQTermNamedFunctionKind = (xqfkBasic, xqfkComplex, xqfkNativeInterpreted, xqfkUnknown, xqfkTypeConstructor); //"unknown" means unitialized or user-defined

  { TXQTermNamedFunction }

  TXQTermNamedFunction = class(TXQTermWithChildrenOnStack)
    name: TXQEQName;
    kind: TXQTermNamedFunctionKind;
    func: TXQAbstractFunctionInfo;
    version: PXQFunctionParameterTypes;
    constructor Create;
//    constructor create(const akind: TXQTermNamedFunctionKind; const afunc: TXQAbstractFunctionInfo);
    constructor create(const anamespace, alocalname: string; arity: integer; const staticContext: TXQStaticContext = nil);
    constructor create(const anamespace, alocalname: string; args: array of TXQTerm; const staticContext: TXQStaticContext = nil);
    destructor destroy; override;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    procedure assignWithoutChildren(source: TXQTermNamedFunction);
    function clone: TXQTerm; override;
    function ToString: ansistring; override;

    //for internal usage
    class function findKindIndex(const anamespace, alocalname: string; const argcount: integer; const staticContext: TXQStaticContext; out akind: TXQTermNamedFunctionKind; out afunc: TXQAbstractFunctionInfo): boolean;

    function convertToTypeConstructor: TXQTermNamedFunction;
  public
  //internally used
    interpretedFunction: TXQValueFunction;
    functionStaticContext: TXQStaticContext; //used for variable cycle detection
    procedure init(const context: TXQStaticContext);
  end;
  TXQTermNamedFunctionTypeConstructor = class(TXQTermNamedFunction)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;


  TXQTermDynamicFunctionCall = class (TXQTermWithChildren)
    constructor create(func: TXQTerm = nil; arg: TXQTerm = nil);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermBinaryOp }

  TXQTermBinaryOp = class(TXQTermWithChildrenOnStack)
    op: TXQOperatorInfo;
    constructor create(const aop: string; arg1: TXQTerm = nil; arg2: TXQTerm = nil);
    constructor create(arg1: TXQTerm; const aop: string; arg2: TXQTerm);
    constructor create(opinfo: TXQOperatorInfo);
    constructor createUnary(const aop: string; arg: TXQTerm);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function debugTermToString: string; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
  end;

  TXQTermSimpleMap = class(TXQTermWithChildren)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function debugTermToString: string; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  TXQTermPath = class(TXQTerm)
  private
    procedure addToPath(term: TXQTerm);
  public
    path: array of TXQPathMatchingStep;
    constructor create(b: TXQTerm);
    destructor Destroy; override;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function debugTermToString: string; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermFlower }
  TXQTermFlowerSubClauseKind = (xqtfcFor, xqtfcLet, xqtfcWindow, xqtfcForPattern, xqtfcLetPattern, xqtfcWhere, xqtfcOrder, xqtfcCount, xqtfcGroup);
  TXQTermFlowerSubClause = class(TXQTerm)
    class function kind: TXQTermFlowerSubClauseKind; virtual;

    function evaluate(var context: TXQEvaluationContext): IXQValue; override;

    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); virtual;
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

    //for internal use
    function findDuplicatedVariable: TXQTermVariable;
  private
    procedure visitlocalvariables(callback: TXQTermFlowerWindowVariableCallback; data: pointer);
    function variableCount: integer;
  end;
  TXQTermFlowerLetPattern = class(TXQTermFlowerSubClause)
    pattern: TXQTermPatternMatcher;
    expr: TXQTerm;
    sequenceTyp: TXQTermSequenceType;

    class function kind: TXQTermFlowerSubClauseKind; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); override;
    destructor destroy; override;
  end;
  TXQTermFlowerForPattern = class(TXQTermFlowerLetPattern)
    class function kind: TXQTermFlowerSubClauseKind; override;
  end;
  TXQTermFlowerWhere = class(TXQTermFlowerSubClause)
    test: TXQTerm;
    class function kind: TXQTermFlowerSubClauseKind; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;
  TXQTermFlowerOrder = class(TXQTermFlowerSubClause)
    //stableOrder: boolean; //always be stable
    expr: TXQTerm;
    descending: boolean; //ascending is default
    emptyOrder: TXQTermFlowerOrderEmpty;
    collation: TXQCollation;
    class function kind: TXQTermFlowerSubClauseKind; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;
  TXQTermFlowerCount = class(TXQTermFlowerSubClause)
    countvar: TXQTermVariable;

    class function kind: TXQTermFlowerSubClauseKind; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;
  TXQTermFlowerGroup = class(TXQTermFlowerSubClause)
    vars: array of TXQTermVariable;
    seqtypes: array of TXQTermSequenceType;
    collation: TXQCollation;
    class function kind: TXQTermFlowerSubClauseKind; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override; //This will not undeclare the variables!
    procedure visitchildrenToUndeclare(visitor: TXQTerm_Visitor); override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  end;

  TXQTermFlower = class sealed (TXQTermWithChildren)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    procedure precompute;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
    destructor destroy; override;
  private
    orders: array of TXQTermFlowerOrder;
    variables: array of TXQTermVariable;
    clauseIndex: array of integer; //how many clauses of a similar kind occur before that one. (three kinds: assignments, order, count)
    varCount, orderCount, countCount: Integer;
    needFullTuple: Boolean;
  end;


  { TXQTermSomeEvery }

  TXQTermSomeEvery = class(TXQTermWithChildren)
    isEvery: boolean;
    constructor create(every: boolean);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermIf }

  TXQTermIf = class(TXQTermWithChildren)
    constructor create;
    constructor createLogicOperation(const isOr: boolean; a, b: TXQTerm);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermTypeSwitch }

  TXQTermTypeSwitch = class(TXQTermWithChildren)
    type TXQTermTypeSwitchClause = class(TXQterm)
      pattern: TXQTermPatternMatcher;
      variable: TXQTermVariable;
      typ: TXQTermSequenceType;
      expr: TXQTerm;
      function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
      function getContextDependencies: TXQContextDependencies; override;
      function evaluate(var context: TXQEvaluationContext): IXQValue; override;
      function clone: TXQTerm; override;
      destructor Destroy; override;
      function matched(var context: TXQEvaluationContext; const v: IXQValue; out res: IXQValue): boolean;
    end;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
  end;

  { TXQTermSwitch }

  TXQTermSwitch = class(TXQTermWithChildren)
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;

  { TXQTermReadObjectProperty }

  TXQTermReadObjectProperty = class(TXQTermWithChildren)
    propname: string;
    constructor create(apropname: string);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
  end;

  TXQTermJSONLookupMode = (xqtjlmInteger, xqtjlmName, xqtjlmParenthesized, xqtjlmWild);
  TXQTermJSONLookup = class(TXQTermWithChildren)
    mode: TXQTermJSONLookupMode;
    unary: boolean;
    integerKey: SizeInt;
    propname: string;
    constructor create(aunary: boolean);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function clone: TXQTerm; override;
  end;

  TXQTermConstructorState = record
    document: TTreeDocument;
    root: TTreeNode;
    baseOffset: TTreeNodeIntOffset;
    olddocumentnamespacecount: SizeInt;
    namespaceOverrides: TNamespaceList;
  end;

  TXQTermConstructor = class(TXQTermWithChildren)
    typ: TTreeNodeType;
    nameValue: TXQTerm;
    nameHash: cardinal;
    implicitNamespaces: TNamespaceList;
    constructor create(atype: TTreeNodeType; aname: txqterm = nil);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function evaluate(var context: TXQEvaluationContext; var state: TXQTermConstructorState): TTreeNode;
    function getContextDependencies: TXQContextDependencies; override;
    function isNamespaceConstructor: boolean;
    function clone: TXQTerm; override;
    destructor destroy; override;

    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
  end;

  TXQTermConstructorComputed = class(TXQTermConstructor)  end; //exactly the same but used during parser for different error codes

  { TXQTermJSONObjectConstructor }

  TXQTermMapConstructor = class(TXQTermWithChildren)
    procedure duplicateError(const map: IXQValue; errStandardCode: boolean = true);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;
  TXQTermJSONiqObjectConstructor = class(TXQTermMapConstructor)
    optionals: array of boolean;
    objectsRestrictedToJSONTypes: boolean;
    allowDuplicates: boolean;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function clone: TXQTerm; override;
  end;

  { TXQTermTryCatch }

  TXQTermTryCatch = class(TXQTerm)
  private
    infoVars: array[0..6] of TXQTermVariable;
  public
    body: TXQTerm;
    catches: array of record
      tests: array of record
        name: TXQEQName;
        ignoreNamespace: boolean;
      end;
      expr: TXQTerm;
    end;

    constructor create(abody: TXQTerm);
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
    function visitchildren(visitor: TXQTerm_Visitor): TXQTerm_VisitAction; override;
    function clone: TXQTerm; override;
    destructor Destroy; override;
  end;

  { TXQTermModule }

  TXQTermModule = class(TXQTermWithChildren)
    allVariables: array of TXQTermDefineVariable;
    function evaluate(var context: TXQEvaluationContext): IXQValue; override;
    function getContextDependencies: TXQContextDependencies; override;
  end;


  TXQInternalPatternMatcherParse = function (const context: TXQStaticContext;  data: string): TXQTermPatternMatcher;
  TXQInternalPatternMatcherMatch = function (template, data: TTreeNode; var context: TXQEvaluationContext; throwExceptions: boolean = false): TXQVariableChangeLog;
  TXQInternalPatternMatcherVisit = function (const template: TXQTermPatternMatcher; visitor: TXQTerm_Visitor): TXQTerm_VisitAction;





  //============================XQUERY QUERY HOLDER==========================

  { IXQuery }
  //** @abstract Interface for an XPath/XQuery query.
  //** Reference counted. Call evaluate to evaluate it.
  IXQuery = interface
    function evaluate(const tree: TTreeNode = nil): IXQValue; //**< Evaluates the query with a certain root element (i.e. @code(/) will return tree)
    function evaluate(var context: TXQEvaluationContext): IXQValue; //**< Evaluates the query with a certain evaluation context
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
  TXQuery = class(TFastInterfacedObject, IXQuery)
    constructor Create(asStaticContext: TXQStaticContext; aterm: TXQTerm = nil);
    function evaluate(const tree: TTreeNode = nil): IXQValue;
    function evaluate(var context: TXQEvaluationContext): IXQValue;
    function evaluateInConstContext(const context: TXQEvaluationContext): IXQValue;
    function evaluate(const contextItem: IXQValue): IXQValue;

    function clone: IXQuery;
    function visit(visitor: TXQTerm_VisitorClass; parent: TXQTerm = nil): TXQTerm_VisitAction;
    function visit(visitor: TXQTerm_Visitor; parent: TXQTerm = nil): TXQTerm_VisitAction;

    function getStaticContext: TXQStaticContext; inline;

    destructor Destroy; override;
  protected
    staticContext: TXQStaticContext;
    fterm: txqterm;
    function getTerm: TXQTerm;
    procedure setTerm(aterm: TXQTerm);
  public
    staticContextShared: boolean;
    property Term: TXQTerm read getTerm write setTerm;
  end;

  TXQueryModuleList = specialize TFastInterfaceList<IXQuery>;

  //============================EXCEPTIONS/EVENTS==========================

  { EXQException }
  //**General XQuery exception, with an namespace, errorCode and message
  EXQException = class(Exception)
    errorCode: string;
    namespace: INamespace;
    constructor create(aerrcode, amessage: string; anamespace: INamespace = nil);
    class function searchClosestFunction(const addr: pointer): string;
  private
    rawMessageLength: SizeInt;
    function messagePrefix: string;
    function rawMessage: string;
  end;
  TXQExceptionEvent = procedure (exception: EXQException) of object;

  //**Exception raised during the parsing of an expression
  EXQParsingException = class(EXQException)
    next: EXQParsingException;
    constructor create(aerrcode, amessage: string; anamespace: INamespace = nil);
    destructor Destroy; override;
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
    @abstract(Event callback that is called to set the @code(value) of an XQuery variable declared as "declare variable ... external").

    The return value can be created with one of the xqvalue(..) functions.
  *)
  TXQDeclareExternalVariableEvent = procedure(const context: TXQStaticContext; sender: TObject; const namespaceUrl, variable: string; var value: IXQValue) of object;
  (***
  @abstract(Event callback that is called to set a function @code(value) of an XQuery function declared as "declare function ... external").

  The function in @code(result) has already been initialized with the parameters and result type, only the term in @code(result.body) has to be set.@br
  You can either create an syntax tree for the function with the respective TXQTerm classes or derive a class from TXQTerm and override the evaluate function to calculate it natively.
  *)
  TXQDeclareExternalFunctionEvent = procedure(sender: TObject; const context: TXQStaticContext; const namespaceURL, functionName: string; var result: TXQValueFunction) of object;

  TXQImportModuleEvent = procedure (sender: TObject; context: TXQStaticContext; const namespace: string; const at: array of string) of object;

  //** Event called by the fn:trace(value,info) function.
  TXQTraceEvent = procedure (sender: TXQueryEngine; value, info: IXQValue) of object;
  //** Event called by the fn:doc to parse a downloaded document.
  TXQParseDocEvent = procedure (sender: TXQueryEngine; data, url, contenttype: string; var node: TTreeNode) of object;
  TXQWarningEvent = procedure (sender: TXQueryEngine; warning: string) of object;

  //** Modifier for integer formatting, as described in fn:format-integer.
  //** The absence of xqfimOrdinal means cardinal    , the absence of traditional means alphabetic. (traditional is a traditional way of writing integers with letters, e.g. roman numbers)
  TXQFormatIntegerModifier = (xqfimOrdinal, xqfimTraditional);
  TXQFormatIntegerModifiers = set of TXQFormatIntegerModifier;

  TXQFormatIntegerEvent = procedure (sender: TObject; integerNumber: BigDecimal; primaryFormat, modifierVariant, language: string; modifiers: TXQFormatIntegerModifiers; var formatted: string) of object;

  //** Record grouping different parsing options
  TXQParsingOptionsStringEntities = (xqseDefault, xqseIgnoreLikeXPath, xqseResolveLikeXQuery);
  TXQJSONObjectMode = (xqjomForbidden, xqjomMapAlias, xqjomJSONiq );
  TXQJSONArrayMode = (xqjamStandard, xqjamArrayAlias, xqjamJSONiq );
  TXQParsingOptions = record
  private
    procedure SetAllowJSON(AValue: boolean);
  public
    AllowExtendedStrings: boolean; //**< If strings with x-prefixes are allowed, like x"foo{$variable}bar" to embed xquery expressions in strings
    AllowPropertyDotNotation: TXQPropertyDotNotation; //**< If it is possible to access (json) object properties with the @code(($obj).property) or even @code($obj.property) syntax (default is xqpdnAllowUnambiguousDotNotation, property syntax can be used, where a dot would be an invalid expression in standard xquery)
    AllowJSONLiterals: boolean; //**< If true/false/null literals are treated like true()/false()/jn:null()  (default true! However, this option is ignored and handled as false, if allowJSON is false).
    JSONArrayMode: TXQJSONArrayMode;
    JSONObjectMode: TXQJSONObjectMode;
    AllowJSONiqTests: boolean;
    StringEntities: TXQParsingOptionsStringEntities; //**< XQuery is almost a super set of XPath, except for the fact that they parse string entities differently. This option lets you change the parsing behaviour.
    LineEndingNormalization: (xqlenNone, xqlenXML1, xqlenXML11); //**< If all line breaks (#$D or #$D,#$85,#$2028) should be replaced by #$A
    AllowMutableVariables: boolean; //**< If $var := 123 without let should be allowed
    property AllowJSON: boolean write SetAllowJSON; deprecated;
  end;

  TXQDebugInfo = class
    function lineInfoMessage(t: TObject): string; virtual; abstract;
  end;


  //============================MAIN CLASS==========================

  { TXQueryEngine }

  (***
    @abstract(@bold(This is the XPath/XQuery-engine))

    You can use this class to evaluate an XPath/XQuery-expression on a certain document tree.@br
    For example, @code(TXQueryEngine.evaluateStaticXPath2('expression', nil)) returns the value of the evaluation of expression.

    A simple functional interface is provided by the function query.@br@br

    @bold(Syntax of an XQuery / XPath / Pseudo-XPath-Expression)

    This query engine currently supports XPath 3.1, XQuery 3.1, and JSONiq, with some extensions and minor deviations, as well as compatibility modes for older XPath and XQuery versions.
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

    A complete list of supported functions is given at http://www.benibela.de/documentation/internettools/xpath-functions.html

    Differences between this implementation and standard XPath/XQuery (most differences can be turned off with the respective option or the field in the default StaticContext):

    Extended syntax:@br

    @unorderedList(
    @item(@code(x"something{$var}{1+2+3}...") @br If a string is prefixed with an x, all expressions within {..}-parenthesis are evaluated and concatenated to the raw text, similarily to the value of an xquery direct attribute constructor. (option: extended-strings))
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

    You can look at the unit tests in the tests directory to see many (> 5000) examples.

    @bold(Using the class in FPC)

    The simplest way to use it with the function query and the defaultQueryEngine.

    You can evaluate XQuery/XPath-expressions by calling the class methods, e.g. @code(TXQueryEngine.evaluateStaticXPath3('expression', nil)) or @code(TXQueryEngine.evaluateStaticXPath2('expression', nil).toInt64) which returns the value of the expression, converted to the corresponding type.@br
    If you want to process an HTML/XML document, you have to pass the root TTreeNode (obtained by TTreeParser) instead of nil.@br@br@br
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
        @item(The XPath 3.1 syntax for JSON objects or arrays is used; previously it used the JSONiq syntax. There are various compatibility options to choose a custom syntax.)
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
      @item(IXQValue.toString on a sequence or array returns the concatenated strings of all contained values. Previously, it only returned the first.)
      @item(It assumes the default codepage is UTF-8. (before FreePascal 3.0 there was no default codepage) )
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
    ImplicitTimezoneInMinutes: Integer; //**< Local timezone (high(integer) = unknown, 0 = utc).

    StaticContext: TXQStaticContext;  //**< XQuery static context, defining various default values.


    VariableChangelog: TXQVariableChangeLog;  //**< All global variables that have been set (if a variable was overriden, it stores the old and new value)

    OnDeclareExternalVariable: TXQDeclareExternalVariableEvent;
    OnDeclareExternalFunction: TXQDeclareExternalFunctionEvent; //**< Event called to import a function that is declared as "declare function ... external" in an XQuery expression.
    OnImportModule: TXQImportModuleEvent;  //**< Event called to import an XQuery module that has not previously be defined

    OnTrace: TXQTraceEvent; //**< Event called by fn:trace
    OnCollection, OnUriCollection: TXQEvaluateVariableEvent; //**< Event called by fn:collection
    OnParseDoc: TXQParseDocEvent; //**< Event called by fn:doc (if nil, a default xml parser is used)

    OnWarningDeprecated: TXQWarningEvent; //**< Event called when a deprecated feature is used.

    ParsingOptions: TXQParsingOptions;

    GlobalNamespaces: TNamespaceList;  //**< Globally defined namespaces

    AutomaticallyRegisterParsedModules: boolean;

    procedure clear; //**< Clears all data.
    //** Parses a new XPath 2.0 expression and stores it in tokenized form.
    function parseXPath2(s:string; sharedContext: TXQStaticContext = nil): IXQuery; deprecated 'use parseQuery';
    //** Parses a new XQuery 1.0 expression and stores it in tokenized form.
    function parseXQuery1(s:string; sharedContext: TXQStaticContext = nil): IXQuery; deprecated 'use parseQuery';
    //** Parses a new XPath 3.0 expression and stores it in tokenized form.
    function parseXPath3(s:string; sharedContext: TXQStaticContext = nil): IXQuery; deprecated 'use parseQuery';
    //** Parses a new XQuery 3.0 expression and stores it in tokenized form.
    function parseXQuery3(s:string; sharedContext: TXQStaticContext = nil): IXQuery; deprecated 'use parseQuery';
    //** Parses a new CSS 3.0 Selector expression and stores it in tokenized form.
    function parseCSS3(s:string): IXQuery;
    //** Parses a new expression and stores it in tokenized form.
    function parseQuery(s:string; model: TXQParsingModel = xqpmXQuery3_1; sharedContext: TXQStaticContext = nil): IXQuery;

    function evaluate(var context: TXQEvaluationContext): IXQValue;
    function evaluate(const contextItem: IXQValue): IXQValue; //**< Evaluates a previously parsed query and returns its value as IXQValue
    function evaluate(tree:TTreeNode = nil): IXQValue; //**< Evaluates a previously parsed query and returns its value as IXQValue

    constructor create;
    destructor Destroy; override;

protected
    PatternMatcherTextStart, PatternMatcherTextEnd: TTreeNode;
public
    function evaluate(expression: string; model: TXQParsingModel; tree:TTreeNode = nil): IXQValue;
    function evaluate(expression: string; model: TXQParsingModel; const contextItem: IXQValue): IXQValue;

    //** Evaluates an XQuery 3.1 expression with a certain tree element as current node.
    function evaluateXQuery(expression: string; tree:TTreeNode = nil): IXQValue;
    //** Evaluates an XQuery 3.1 expression with a certain tree element as current node.
    function evaluateXQuery(expression: string; const contextItem: IXQValue): IXQValue;
    //** Evaluates an XPath 3.1 expression with a certain tree element as current node.
    function evaluateXPath(expression: string; tree:TTreeNode = nil): IXQValue;
    //** Evaluates an XPath 3.1 expression with a certain tree element as current node.
    function evaluateXPath(expression: string; const contextItem: IXQValue): IXQValue;

    //** Evaluates an XPath 2.0 expression with a certain tree element as current node.
    function evaluateXPath2(expression: string; tree:TTreeNode = nil): IXQValue; deprecated 'use evaluateXPath for the newest XPath version, or evaluate for an older';
    function evaluateXPath2(expression: string; const contextItem: IXQValue): IXQValue; deprecated 'use evaluateXPath for the newest XPath version, or evaluate for an older';
    //** Evaluates an XQuery 1.0 expression with a certain tree element as current node.
    function evaluateXQuery1(expression: string; tree:TTreeNode = nil): IXQValue; deprecated 'use evaluateXQuery for the newest XQuery version, or evaluate for an older';
    function evaluateXQuery1(expression: string; const contextItem: IXQValue): IXQValue; deprecated 'use evaluateXQuery for the newest XQuery version, or evaluate for an older';
    //** Evaluates an XPath 3.0 expression with a certain tree element as current node.
    function evaluateXPath3(expression: string; tree:TTreeNode = nil): IXQValue; deprecated 'use evaluateXPath for the newest XPath version, or evaluate for an older';
    function evaluateXPath3(expression: string; const contextItem: IXQValue): IXQValue; deprecated 'use evaluateXPath for the newest XPath version, or evaluate for an older';
    //** Evaluates an XQuery 3.0 expression with a certain tree element as current node.
    function evaluateXQuery3(expression: string; tree:TTreeNode = nil): IXQValue; deprecated 'use evaluateXQuery for the newest XQuery version, or evaluate for an older';
    function evaluateXQuery3(expression: string; const contextItem: IXQValue): IXQValue; deprecated 'use evaluateXQuery for the newest XQuery version, or evaluate for an older';
    //** Evaluates an CSS 3 Selector expression with a certain tree element as current node.
    function evaluateCSS3(expression: string; tree:TTreeNode = nil): IXQValue;
    function evaluateCSS3(expression: string; const contextItem: IXQValue): IXQValue;



    //** Evaluates an expression with a certain tree element as current node.
    class function evaluateStaticXPath2(expression: string; tree:TTreeNode = nil): IXQValue; overload; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';
    class function evaluateStaticXPath2(expression: string; const contextItem: IXQValue): IXQValue; overload; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';
    class function evaluateStaticXPath3(expression: string; tree:TTreeNode = nil): IXQValue; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';
    class function evaluateStaticXPath3(expression: string; const contextItem: IXQValue): IXQValue; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';
    class function evaluateStaticXQuery1(expression: string; tree:TTreeNode = nil): IXQValue; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';
    class function evaluateStaticXQuery1(expression: string; const contextItem: IXQValue): IXQValue; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';
    class function evaluateStaticXQuery3(expression: string; tree:TTreeNode = nil): IXQValue; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';
    class function evaluateStaticXQuery3(expression: string; const contextItem: IXQValue): IXQValue; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';
    //** Evaluates an expression with a certain tree element as current node.
    class function evaluateStaticCSS3(expression: string; tree:TTreeNode = nil): IXQValue; deprecated 'use global query() function for XQuery or defaultQueryEngine.evaluate* [afterwards call freeThreadVars]';

    procedure registerModule(module: IXQuery);  //**< Registers an XQuery module. An XQuery module is created by parsing (not evaluating) a XQuery expression that contains a "module" declaration
    function findModule(const namespaceURL: string): TXQuery; //**< Finds a certain registered XQuery module
    function findModule(context: TXQStaticContext; const namespace: string; const at: array of string): TXQuery; //**< Finds a certain registered XQuery module or tries to load one
    class function findNativeModule(const ns: string): TXQNativeModule; //**< Finds a native module.

    //** Registers a collation for custom string comparisons
    class procedure registerCollation(const collation: TXQCollation);

    //** Returns the collation for an url id
    class function getCollation(id:string; base: string; errCode: string = 'FOCH0002'): TXQCollation;

  private
    FLastQuery: IXQuery;
    FCreationThread: TThreadID;
  protected
    FModules, FPendingModules: TXQueryModuleList; //internal used
    FParserVariableVisitor: TObject;
    VariableChangelogUndefined, FDefaultVariableHeap: TXQVariableChangeLog;
    FDefaultVariableStack: TXQEvaluationStack;

    function GetNativeModules: TStringList;
    function isAWeirdGlobalVariable(const namespace, local: string): boolean;
    procedure addAWeirdGlobalVariable(const namespace, local: string);
    class procedure freeCommonCaches; static;
  protected
    function parseTerm(str:string; model: TXQParsingModel; context: TXQStaticContext = nil): TXQuery;
    function parseCSSTerm(css:string): TXQTerm;
    function parseXStringNullTerminated(str: string): TXQuery;

    //** Applies @code(filter) to all elements in the (sequence) and deletes all non-matching elements (implements []) (may convert result to nil!)
    class procedure filterSequence(const sequence: IXQValue; outList: TXQVList; const filter: TXQPathMatchingStepFilter; var context: TXQEvaluationContext);
    //** Applies @code(filter) to all elements in the (sequence) and deletes all non-matching elements (implements []) (may convert result to nil!)
    class procedure filterSequence(var result: IXQValue; const filter: TXQPathMatchingStepFilters; var context: TXQEvaluationContext);

    //** Performs a query step, given a (sequence) of parent nodes
    class function expandSequence(const previous: IXQValue; const command: TXQPathMatchingStep; var context: TXQEvaluationContext; lastExpansion: boolean): IXQValue; static;
    //** Initialize a query by performing the first step
    class function evaluateSingleStepQuery(const query: TXQPathMatchingStep;var context: TXQEvaluationContext; lastExpansion: boolean): IXQValue; static;

  public
    DefaultParser: TTreeParser; //used by fn:doc if no context node is there (internally used)
    DefaultJSONParser: TXQJsonParser;
    LastDebugInfo: TXQDebugInfo;

    class procedure registerNativeModule(const module: TXQNativeModule);
    {$ifdef DUMPFUNCTIONS}class procedure dumpFunctions;{$endif}

    function getEvaluationContext(staticContextOverride: TXQStaticContext = nil): TXQEvaluationContext;

    //** Last parsed query
    property LastQuery: IXQuery read FLastQuery;
  //for internal use
    function findNamespace(const nsprefix: string): TNamespace;
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
  function xqvalue():IXQValue; //**< Creates an undefined/empty-sequence IXQValue
  function xqvalue(const v: Boolean):IXQValue; inline; //**< Creates an boolean IXQValue
  function xqvalueTrue:IXQValue; inline; //**< Creates an boolean IXQValue
  function xqvalueFalse:IXQValue; inline; //**< Creates an boolean IXQValue
  function xqvalue(const v: Int64):IXQValue; inline; //**< Creates an integer IXQValue
  function xqvalue(v: Integer):IXQValue; inline; //**< Creates an integer IXQValue
  function xqvalue(v: xqfloat):IXQValue; inline; //**< Creates an BigDecimal IXQValue
  function xqvalue(const v: BigDecimal):IXQValue; inline; //**< Creates an BigDecimal IXQValue
  function xqvalue(const v: string):IXQValue; inline; //**< Creates a string IXQValue
  function xqvalue({%H-}intentionallyUnusedParameter: TDateTime):IXQValue; inline; //**< Raises an exception (to prevent xquery(TDateTime) from using xquery(float))
  function xqvalue(v: TTreeNode):IXQValue; inline; //**< Creates a node TXQValue
  function xqvalue(sl: TStringList): IXQValue; //**< Creates a sequence of strings (does *not* free the list)
  function xqvalue(const sl: array of string): IXQValue; //**< Creates a sequence of untyped strings
  function xqvalue(const sl: array of IXQValue): IXQValue; //**< Creates a sequence
  function xqvalue(const s: TXQHashsetStr): IXQValue; //**< Creates a sequence

  procedure xqvalueSeqSqueeze(var v: IXQValue); //**< Squeezes an IXQValue (single element seq => single element, empty seq => undefined)
  procedure xqvalueSeqSqueezed(out result: IXQValue; l: TXQVList); //**< Creates an IXQValue from a list sequence  (assume it FREEs the list)
  //** Adds a value to an implicit sequence list in result, i.e. you call it multiple times and the result becomes a sequence of all the add values.  @br
  //** For the first call result and seq must be nil. @br
  //** The point is that it only creates a sequence if there are multiple values, and it is especially fast, if you do not expect multiple values.
  procedure xqvalueSeqConstruct(var result: IXQValue; var seq: TXQValueSequence; const add: IXQValue);


  //**Assigns source to dest without updating ref counts @br
  //**This can be much faster, but will cause a crash, unless the reference count is corrected later, e.g. by xqvalueVaporize on source or dest.
  procedure xqvalueMoveNoRefCount(const source: IXQValue; var dest: IXQValue ); inline;
  procedure xqvalueVaporize(var dest: IXQValue); inline;

type
  (***
  @abstract(An XQuery variable)

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

    procedure setOverride(const n,v: string);
    procedure setOverride(const n: string; const v: ixqvalue);
    procedure setOverride(const n: string; const v: ixqvalue; const namespaceURL: string);

    function get(const name: string): IXQValue; //**< Returns the value of the variable @code(name) @br The returned interface points to the same instance as the interface in the internal variable storage
    function get(const name: string;  const namespaceURL: string): IXQValue; //**< Returns the value of the variable @code(name) @br The returned interface points to the same instance as the interface in the internal variable storage

    function get(i: SizeInt): IXQValue; inline; //**< Value of the variable at index @code(i)  @br The returned interface points to the same instance as the interface in the internal variable storage
    function indexOf(const name: string; const namespaceURL: string = ''): SizeInt; //**< Returns the last index of the variable @code(name) in the internal list. (Warning: doesn't support objects, yet??) It is recommended to use hasVariable instead, the index is an implementation detail

    function getName(i: SizeInt): string; //**< Name of the variable at index @code(i)
    function getNamespace(i: SizeInt): string; //**< Namespace of the variable at index @code(i)
    function getAll(const name: string; const namespaceURL: string = ''): IXQValue; //**< Returns all values of the variable with name @name(name) as sequence
    function getString(const name:string): string; //**< Returns a value as string. This is the same as get(name).toString.
    function isPropertyChange(i: SizeInt): boolean;

    //** Returns if a variable with name @param(variable) exists
    function hasVariable(const variable: TXQTermVariable): boolean;
    //** Returns if a variable with name @param(variable) exists
    function hasVariable(const variable: string; const namespaceURL: string = ''): boolean;
    //** Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value).
    function hasVariable(const variable: string; out value: IXQValue; const namespaceURL: string = ''): boolean;
    //** Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value).
    function hasVariable(const variable: TXQTermVariable; out value: IXQValue): boolean;

    property Values[name:string]:IXQValue read get write add; default;
    property ValuesString[name:string]:string read getString write add;
    property Names[i: SizeInt]: string read getName;

    constructor create();
    destructor destroy();override;

    procedure clear; //**< Clear everything
    procedure remove(const name: string; const namespace: string = '');
    function pushAll: SizeInt; //**< Marks the current state of the variables (in O(1))
    procedure popAll(level: SizeInt = -1); //**< Reverts all variables to the latest marked state


    procedure stringifyNodes;
    function hasNodes: boolean;
    //class function splitName(const variable: string; out base, varname: string): boolean; static;

    function debugTextRepresentation: string; //**< Dump of the log as list of name=value pairs

    function clone: TXQVariableChangeLog;
    procedure assign(from: TXQVariableChangeLog);
    function finalValues: TXQVariableChangeLog; //**< Remove all duplicates, so that only the last version of each variable remains
    procedure takeFrom(other: TXQVariableChangeLog); //**< Adds all variables from other to self, and clears other
    function condensed: TXQVariableChangeLog; //**< Removes all assignments to object properties and only keeps a final assignment to the object variable that contains all properties (i.e. @code(obj.a := 123, obj.b := 456) is condensed to a single assignment like in the pseudocode @code(obj := {a: 123, b:456})))
    function collected: TXQVariableChangeLog; //**< Collects multiple assignments to single sequence assignment. (i.e. @code(a := 123, a := 456, a := 789) collected is equivalent to @code(a := (123, 456, 789))) (creates a new variable log that has to be freed)
    function condensedCollected: TXQVariableChangeLog; //**< Same as condensed.collected

    //function evaluateVariable(sender: TObject; const variable: string; var value: IXQValue): boolean; //**< Sets @code(value) to the value of the variable @code(variable). @br This is used as callback by the XQuery-Engine
    //procedure defineVariable(sender: TObject; const variable: string; const value: IXQValue); //**< Sets @code(variable) to the @code(value)@br This is used as callback by the XQuery-Engine

    procedure addObjectModification(const variable: string; value: IXQValue; const namespaceURL: string; const props: PString; len: SizeInt);

    function toStringMap: TXQValueStringMap;
  protected
    shared: boolean;
    varCount, historyCount: SizeInt;
    varstorage: array of TXQVariable;
    histories: array of SizeInt;
    procedure removeLast;

    procedure pushOpenArray(const vs: array of IXQValue);
    procedure pushOpenArray(const untypedStrings: array of string);
    procedure reserve(newcap: SizeInt);
  public
    property count: SizeInt read varcount;
  end;


TXQDecimalFormatProperty = (xqdfpDecimalSeparator, xqdfpGroupingSeparator, xqdfpMinusSign, xqdfpPercent, xqdfpPerMille, xqdfpZeroDigit, xqdfpDigit, xqdfpPatternSeparator, xqdfpExponentSeparator);
TXQDecimalFormatPropertyData = record
   chars: array[TXQDecimalFormatProperty] of integer;
   infinity, nan: string;
end;
TXQDecimalFormat = class
  namespaceURL, localname: string;
  formats: TXQDecimalFormatPropertyData;
  constructor Create;
  constructor CreateEmpty;
  function clone: TXQDecimalFormat;
end;

//var curUnitTest: integer;

//**If XQGlobalTrimNodes is true, the result of every node->string conversion is trimmed. This trimming occurs after and not during the conversion.@br
//**E.g. If it is true, @code(text()) and @code(deep-text()) for @code(<a> a </a>) return 'a', and deep-text() for @code(<x><a> a </a><a> b </a></x>) returns 'a b'. @br
//**(This variable should actually be a property of TXQueryEngine, but that is not possible in the current design,
//**since the values convert themself, and don't know their corresponding parser)
var XQGlobalTrimNodes: boolean = true;


//**If XQGlobalUseIDfromDTD is false, the fn:*id* functions will thus assume every attribute with name id is an ID attribute@br
//**If XQGlobalUseIDfromDTD is true, only real ID attributes are used.  However, simplehtmltreeparser cannot handle DTDs, so it cannot detect ID attributes. So if this is true, only the wrapped fcl-xml parser from simplehtmltreeparserfpdom can be used
var XQGlobalUseIDfromDTD: boolean = false;


type TXQDebugTracingEvent = procedure (term: TXQTerm; const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue) of object;

//**Experimental event to trace the evaluation of a query
var XQOnGlobalDebugTracing: TXQDebugTracingEvent;

type

{ TXQValue_DatePart }

//** (Abstract) Class containing different parts of a date
TXQValue_DatePart = class (TXQValueDateTime)
end;

type


 {** A native XQuery module. Each native module has a certain namespace and declares functions, types and operators *}
 TXQNativeModule = class
  acceptedModels: TXQParsingModels;
  namespace: TNamespace;
  parents: array of TXQNativeModule;
  constructor create(const anamespace: TNamespace; const aparentModule: array of TXQNativeModule);
  constructor create(const anamespace: TNamespace);
  destructor Destroy; override;
protected
  procedure setTypeChecking(const name: string; info: TXQAbstractFunctionInfo; const typeChecking: array of string);
  procedure setTypeChecking(const name: string; info: TXQAbstractFunctionInfo; const typeChecking: array of TXQTermSequenceType);
public
  function registerFunction(const name: string; func: TXQComplexFunction; contextDependencies: TXQContextDependencies): TXQComplexFunctionInfo; overload;
  function registerFunction(const name: string; func: TXQBasicFunction): TXQBasicFunctionInfo; overload;
  //** Registers a function that does not depend on the context.
  //** TypeChecking contains a list of standard XQuery function declarations (without the function name) for strict type checking.
  function registerFunction(const name: string; minArgCount, maxArgCount: integer; func: TXQBasicFunction; const typeChecking: array of string): TXQBasicFunctionInfo; overload;
  function registerFunction(const name: string; func: TXQBasicFunction; const typeChecking: array of string): TXQBasicFunctionInfo; overload;
  function registerBasicFunction(const name: string; func: TXQBasicFunction; const typeChecking: array of TXQTermSequenceType): TXQBasicFunctionInfo; //this should also be called registerFunction, but then it does not find the overload
  //** Registers a function that does depend on the context.
  //**TypeChecking contains a list of standard XQuery function declarations (without the function name) for strict type checking.
  function registerFunction(const name: string; minArgCount, maxArgCount: integer; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]): TXQComplexFunctionInfo;
  function registerFunction(const name: string; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]): TXQComplexFunctionInfo;
  function registerFunction(const name: string; func: TXQComplexFunction; const typeChecking: array of TXQTermSequenceType; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]): TXQComplexFunctionInfo;
  //** Registers a function from an XQuery body
  //**TypeChecking must a standard XQuery function declarations (without the function name but WITH the variable names) (it uses a simplified parser, so only space whitespace is allowed)
  procedure registerInterpretedFunction(const name, typeDeclaration, func: string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]);
  //** Registers a binary operator
  //**TypeChecking contains a list of standard XQuery function declarations (with or without the function name) for strict type checking.
  function registerBinaryOp(const name:string; func: TXQBinaryOp;  priority: integer; flags: TXQOperatorFlags; const typeChecking: array of string; contextDependencies: TXQContextDependencies): TXQOperatorInfo;
  function registerBinaryOp(const name:string; func: TXQBinaryOp;  priority: integer; flags: TXQOperatorFlags; const typeChecking: array of TXQTermSequenceType; contextDependencies: TXQContextDependencies): TXQOperatorInfo;
  function registerBinaryOp(const name:string; func: TXQBinaryOp;  priority: integer; flags: TXQOperatorFlags; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]): TXQOperatorInfo;

  function findBasicFunction(const name: string; argCount: integer; model: TXQParsingModel = xqpmXQuery3_1): TXQBasicFunctionInfo;
  function findComplexFunction(const name: string; argCount: integer; model: TXQParsingModel = xqpmXQuery3_1): TXQComplexFunctionInfo;
  function findInterpretedFunction(const name: string; argCount: integer; model: TXQParsingModel = xqpmXQuery3_1): TXQInterpretedFunctionInfo;
  //function findBinaryOp(const name: string; model: TXQParsingModel = xqpmXQuery3_1): TXQOperatorInfo;

  function findSimilarFunctionsDebug(searched: TList; const localname: string): string;
  {$ifdef DUMPFUNCTIONS}procedure dumpFunctions;{$endif}
protected
  basicFunctions, complexFunctions, interpretedFunctions: TXQMapStringOwningObject;
  binaryOpLists: TXQMapStringOwningObject;
  class function findFunction(const sl: TStringList; const name: string; argCount: integer): TXQAbstractFunctionInfo;
end;


//**Escapes for an URL (internally used)
//function urlHexEncode(s: string; const safe: TCharSet = ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~']): string;
//**Checks the length of the args array (internally used)
procedure requiredArgCount(argc: sizeint; minc: sizeint; maxc: sizeint = -2);
procedure requiredArgType(const v: IXQValue; typ: TXSType);

//**Calculates starting position / length from a range definition (checks for things like NaN, INF, ...) (internally used)
procedure xpathRangeDefinition(argc: sizeint; args: PIXQValue; const maxLen: sizeint; out from, len: sizeint);
function xqvalueAtomize(const v: IXQValue): IXQValue;

function xqvalueDeep_equal(const context: TXQEvaluationContext; const a, b: IXQValue; collation: TXQCollation): boolean;  //needed for switch, tests

  const MY_NAMESPACE_PREFIX_URL = 'http://www.benibela.de/2012/pxp/';
  const XMLNamespaceURL_XPathFunctions = 'http://www.w3.org/2005/xpath-functions';
        XMLNamespaceURL_XPathFunctionsMath = XMLNamespaceURL_XPathFunctions + '/math';
        XMLNamespaceURL_XPathFunctionsArray = XMLNamespaceURL_XPathFunctions + '/array';
        XMLNamespaceURL_XPathFunctionsMap = XMLNamespaceURL_XPathFunctions + '/map';

        XMLNamespaceURL_XMLSchema = 'http://www.w3.org/2001/XMLSchema';
        XMLNamespaceURL_XMLSchemaInstance = 'http://www.w3.org/2001/XMLSchema-instance';
        XMLNamespaceURL_XQueryLocalFunctions = 'http://www.w3.org/2005/xquery-local-functions';
        XMLNamespaceURL_XQTErrors = 'http://www.w3.org/2005/xqt-errors';
        XMLNamespaceURL_MyExtensionsMerged = MY_NAMESPACE_PREFIX_URL + 'extensions';
        XMLNamespaceURL_MyExtensionsNew = 'http://pxp.benibela.de';
        XMLNamespaceURL_MyExtensionOperators = MY_NAMESPACE_PREFIX_URL + 'operators';
        XMLNamespaceURL_XQuery = 'http://www.w3.org/2012/xquery';


var GlobalStaticNamespaces: TNamespaceList; //**< List of namespaces which are known in all XPath/XQuery expressions, even if they are not declared there
    AllowJSONDefaultInternal: boolean = false; //**< Default setting for JSON (internally used).
    baseSchema: TJSONiqOverrideSchema;
    baseJSONiqSchema: TJSONiqAdditionSchema;

    patternMatcherParse: TXQInternalPatternMatcherParse;
    patternMatcherMatch: TXQInternalPatternMatcherMatch;
    patternMatcherVisit: TXQInternalPatternMatcherVisit;
    resolveHTMLCallback: TXQComplexFunction;


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


type

TXQAbstractParsingContext = class
protected
  engine: TXQueryEngine;

  options: TXQParsingOptions;
  parsingModel: TXQParsingModel;
  encoding: TSystemCodePage;
  staticContext: TXQStaticContext;

  str: string;
  pos: pchar;

  debugInfo: TXQDebugInfo;
  procedure parseQuery(aquery: TXQuery); virtual; abstract;
  procedure parseQueryXStringOnly(aquery: TXQuery); virtual; abstract;
  procedure parseFunctionTypeInfo(info: TXQAbstractFunctionInfo; const typeChecking: array of string; op: boolean); virtual; abstract;

end;

type TXQTerm_VisitorTrackKnownVariables = class(TXQTerm_Visitor)
  overridenVariables: {set of string} TXQVariableChangeLog;
  tempValue: IXQValue;
  constructor create;
  destructor Destroy; override;

  procedure declare(v: PXQTermVariable); override;
  procedure undeclare(v: PXQTermVariable); override;
end;

const
PARSING_MODEL3 = [xqpmXPath3_0, xqpmXQuery3_0, xqpmXPath3_1, xqpmXQuery3_1];
PARSING_MODEL3_1 = [xqpmXPath3_1, xqpmXQuery3_1];
PARSING_MODEL_XQUERY = [xqpmXQuery1, xqpmXQuery3_0, xqpmXQuery3_1];

var XMLNamespace_XPathFunctions, XMLnamespace_XPathFunctionsArray, XMLnamespace_XPathFunctionsMap, XMLNamespace_MyExtensionsNew, XMLNamespace_MyExtensionsMerged, XMLNamespace_MyExtensionOperators, XMLNamespace_XMLSchema: TNamespace;


function xqFunctionConcat(argc: SizeInt; args: PIXQValue): IXQValue;

function xqgetTypeInfo(wrapper: Ixqvalue): TXQTermSequenceType;
function xqvalueCastAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
function xqvalueCastableAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
function xqvalueInstanceOf(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
function xqvalueOrPlaceholder(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
function xqvalueAndPlaceholder(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;

procedure freeAnnotations(annotations: TXQAnnotations);

const xqpmXPath3 = xqpmXPath3_0;
      xqpmXQuery3 = xqpmXQuery3_0;

//for internal use
function charUnicodeZero(const cp: integer): integer;
procedure ignore(const intentionallyUnusedParameter: TXQEvaluationContext); overload; inline;
procedure ignore(const intentionallyUnusedParameter: IXQValue); overload;inline;
procedure ignore(const intentionallyUnusedParameter: TXQVArray); overload;inline;
const MicroSecsPerSec = int64(1000000);
function isValidXMLCharacter(const codepoint: integer): boolean; inline;
procedure raiseFORG0001InvalidConversion(const v: IXQValue; const convTo: string);
procedure raiseXPTY0004TypeError(const v: IXQValue; const convTo: string);
procedure raiseFOTY0013TypeError(const v: IXQValue);
//Raises an error with an value in the string. String manipulation slows down the entire function, even if the branch is not taking, we should avoid it in functions
procedure raiseXQEvaluationError(const code, s: string; const data: IXQValue);
var
  XQDefaultDecimalFormat: TXQDecimalFormatPropertyData = (
    chars: (ord('.'), ord(','), ord('-'), ord('%'), $2030, ord('0'), ord('#'), ord(';'), ord('e') );
    infinity: 'Infinity';
    nan: 'NaN');
const MATCH_ALL_NODES = [qmText,qmComment,qmElement,qmProcessingInstruction,qmAttribute,qmDocument];

type TXQueryInternals = object
//private
    class var commonValuesUndefined, commonValuesTrue, commonValuesFalse : IXQValue;
    class procedure raiseXSCEError(const err: TXSCastingError; const from, to_: string); static; noreturn;
    class function treeElementAsString(node: TTreeNode; deepSeparator: string = ''): string; static; inline;
end;


type TXQGlobalTypes = record
private
   procedure init;
   procedure free;
   function orEmpty(t: TXQTermSequenceType): TXQTermSequenceType;
   function star(t: TXQTermSequenceType): TXQTermSequenceType;
   function f(args: array of TXQTermSequenceType): TXQTermSequenceType;
public
   case boolean of
   true: (
    none,
    empty,
    atomic, atomicOrEmpty, atomicStar,
    item, itemPlus, itemStar, itemOrEmpty,

    boolean, booleanOrEmpty,

    integer, integerOrEmpty, integerStar,
    decimal, decimalOrEmpty,
    double, doubleOrEmpty,
    numeric, numericOrEmpty,



    stringt, stringOrEmpty, stringStar,
    untypedAtomic,
    anyURI, anyURIOrEmpty, anyURIStar,
    hexBinary, hexBinaryOrEmpty, base64Binary, base64BinaryOrEmpty, anyBinary,
    NOTATIONOrEmpty,
    NCNameOrEmpty,
    QName, QNameOrEmpty,
    language,

    time, timeOrEmpty,
    date, dateOrEmpty,
    dateTime, dateTimeOrEmpty,
    durationOrEmpty,
    dayTimeDuration, dayTimeDurationOrEmpty,
    yearMonthDuration, yearMonthDurationOrEmpty,
    gYearMonthOrEmpty,
    gYearOrEmpty,
    gMonthDayOrEmpty,
    gMonthOrEmpty,
    gDayOrEmpty,



    map, mapStar,
    arrayt, arrayOrEmpty, arrayStar,
    null, jsonItemOrEmpty, jsonItemStar,

    functiont, functiontOrEmpty,
    functionItemAtomicStar, functionAtomicItemStarItemStar, functionItemStarAtomicStar,
    functionItemStarItemItemStar, functionItemItemStarItemStar, functionItemItemItemStar,
    functionItemStarItemStar, functionItemStarBoolean, functionItemStarItemStarItemStar,

    node, nodeOrEmpty, nodeStar,
    element, elementStar,
    documentNodeOrEmpty, documentElementNodeOrEmpty,
    elementSerializationParams, elementSerializationParamsOrEmpty
    : TXQTermSequenceType;
  );
  false: (cachedTypes: array[1..81] of TXQTermSequenceType);
end;
var globalTypes: TXQGlobalTypes;

type TXQGlobalSerializationCallback = function (const context: TXQEvaluationContext; const value: IXQValue): string;
var globalSerializationCallback: TXQGlobalSerializationCallback;

implementation
uses base64, jsonscanner, strutils, xquery__regex, bbutilsbeta, xquery.internals.floathelpers;

var
  interpretedFunctionSynchronization: TRTLCriticalSection;

  const ALL_CONTEXT_DEPENDENCIES = [xqcdFocusItem, xqcdFocusPosition, xqcdFocusLast, xqcdContextCollation, xqcdContextTime, xqcdContextVariables, xqcdContextOther];
  const ALL_CONTEXT_DEPENDENCIES_FOCUS = [xqcdFocusItem, xqcdFocusPosition, xqcdFocusLast];

type TGlobalCurrentQueryInfo = record
  AllowJSONiqOperations: boolean;
  property CastStringMapKeysToString: boolean read AllowJSONiqOperations;
end;
threadvar globalCurrentQueryInfo: TGlobalCurrentQueryInfo;



function namespaceReverseLookup(const url: string): TNamespace; forward;

{$PUSH}{$HINTS OFF}
procedure ignore(const intentionallyUnusedParameter: TXQEvaluationContext); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: IXQValue); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: TXQVArray); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: xqfloat); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: bigDecimal); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: TStringArray); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: array of string); {inline; }begin end;
procedure ignore(const intentionallyUnusedParameter: TTreeNodeSerialization); inline; begin end;
procedure ignore(const intentionallyUnusedParameter: array of IXQValue); { inline; } begin end;
{$POP}


{$ImplicitExceptions off}
procedure xqvalueMoveNoRefCount(const source: IXQValue; var dest: IXQValue ); inline;
begin
  PPointer(@dest)^ := PPointer(@source)^;
end;

procedure xqvalueVaporize(var dest: IXQValue); inline;
begin
  PPointer(@dest)^ := nil;
end;

procedure xqswap(var a, b: IXQValue); inline;
var
  t: Pointer;
begin
  t := PPointer(@a)^ ;
  PPointer(@a)^ := PPointer(@b)^;
  PPointer(@b)^ := t;
end;



function TXQValueDateTimeData.hasTimeZone: boolean;
begin
  result := timezone = high(integer);
end;





procedure TXQParsingOptions.SetAllowJSON(AValue: boolean);
begin
  if avalue then begin
    JSONArrayMode := xqjamArrayAlias;
    JSONObjectMode := xqjomMapAlias;
    AllowJSONiqTests := true;
  end else begin
    JSONArrayMode := xqjamStandard;
    JSONObjectMode := xqjomForbidden;
    AllowJSONiqTests := false;
  end;
end;

procedure TXQSerializer.init(abuffer: pstring; basecapacity: SizeInt; aencoding: TSystemCodePage);
begin
  inherited init(abuffer, basecapacity, aencoding);
  nodeFormat := tnsText;
  allowDuplicateNames := true;
  standard := false;
  sequenceTag :=  'seq';
  elementTag := 'e';
  objectTag := 'object';
end;


procedure TXQSerializer.appendJSONArrayStart;
begin
  inherited appendJSONArrayStart;
  if insertWhitespace = xqsiwIndent then begin
    indent;
    append(LineEnding);
    appendIndent;
  end;
end;

procedure TXQSerializer.appendJSONArrayComma;
begin
  case insertWhitespace of
    xqsiwConservative: append(', ');
    xqsiwIndent: begin
      append(',');
      append(LineEnding);
      appendIndent;
    end;
    xqsiwNever: append(',');
  end;
end;

procedure TXQSerializer.appendJSONArrayEnd;
begin
  if insertWhitespace = xqsiwIndent then begin
    unindent;
    append(LineEnding);
    appendIndent;
  end;
  inherited appendJSONArrayEnd;
end;

procedure TXQSerializer.appendJSONObjectStart;
begin
  inherited appendJSONObjectStart;
  if insertWhitespace = xqsiwIndent then begin
    indent;
    append(LineEnding);
    appendIndent;
  end;
end;

procedure TXQSerializer.appendJSONObjectKeyColon(const key: string);
begin
  appendJSONString(key);
  if insertWhitespace = xqsiwNever then append(':')
  else append(': ');
end;

procedure TXQSerializer.appendJSONObjectComma;
begin
  appendJSONArrayComma
end;

procedure TXQSerializer.appendJSONObjectEnd;
begin
  if insertWhitespace = xqsiwIndent then begin
    unindent;
    append(LineEnding);
    appendIndent;
  end;
  inherited appendJSONObjectEnd;
end;

procedure TXQSerializer.appendJSONString(const s: string);
begin
  append('"');
  onInterceptAppendJSONString(s);
  append('"');
end;

procedure TXQSerializer.appendQualifiedQName(const namespaceurl, local: string);
begin
  append('Q{');
  append(namespaceurl);
  append('}');
  append(local);
end;

procedure TXQSerializer.appendXQueryString(const s: string);
var
  temp: String;
  quot: Char;
begin
  temp := StringReplace(s, '&', '&amp;', [rfReplaceAll]);
  if pos('"', temp) = 0 then quot := '"'
  else if pos('''', temp) = 0 then quot := ''''
  else begin
    quot := '"';
    temp := StringReplace(temp, '"', '""', [rfReplaceAll]);
  end;
  append(quot);
  append(temp);
  append(quot);
end;

procedure TXQSerializer.appendTypeNameFunctionStart(t: TXSType);
begin
  if t is TXSSimpleType then t := TXSSimpleType(t).primitive;
  {if (t.schema <> baseSchema) and (t.schema.url <> XMLNamespaceURL_XMLSchema) then begin
    appendQualifiedQName(t.schema.url, t.name);
  end else begin}
  append('xs:');
  append(t.name);
  append('(');
end;

procedure TXQSerializer.error(const code: string; value: TXQValue);
begin
  error(code, 'Serialization error', value);
end;

procedure TXQSerializer.error(const code: string; message: string; value: TXQValue);
begin
  raise EXQEvaluationException.create(code, message + ', when serializing ' + value.toXQuery);
end;

function TXQTempTreeNodes.GetDocumentCache(const url: string): TTreeDocument;
begin
  result := fdocumentCache[url];
end;

procedure TXQTempTreeNodes.SetDocumentCache(const url: string; AValue: TTreeDocument);
begin
  fdocumentCache[url] := AValue;
end;

function TXQTempTreeNodes.isHidden: boolean;
begin
  Result:=true;
end;

constructor TXQTempTreeNodes.create();
begin
  inherited create(nil);
  tempnodes := TFPList.Create;
  fdocumentCache.init;
end;

destructor TXQTempTreeNodes.destroy;
var
  i: SizeInt;
begin
  for i:= 0 to tempnodes.count - 1 do
    TTreeNode(tempnodes[i]).freeAll();
  tempnodes.free;
  fdocumentCache.done;
  inherited destroy;
end;

function TXQTempTreeNodes.addClone(n: TTreeNode): TTreeNode;
begin
  result := n.clone(self);
  tempnodes.add(result);
end;

function TXQParsingModelsHelper.requiredModelToString: string;
begin
  if not (xqpmXPath3_0 in self) then result := ' requires XPath/XQuery 3.1'
  else if not (xqpmXPath2 in self) then result := ' requires XPath/XQuery 3.0'
  else result := '';
end;

{$ImplicitExceptions on}




class procedure TXQValueEnumeratorPtrUnsafe.clear(out enum: TXQValueEnumeratorPtrUnsafe);
begin
  enum.fcurrent := nil;
  enum.fsingleelement := nil;
  enum.flast := nil;
end;

class procedure TXQValueEnumeratorPtrUnsafe.makesingleelement(const v: ixqvalue; out enum: TXQValueEnumeratorPtrUnsafe);
begin
  enum.fsingleelement := v;
  enum.flast :=  nil;
  enum.fcurrent := nil;
end;

function TXQValueEnumeratorPtrUnsafe.MoveNextSingleElement: Boolean;
begin
  //this must only be called if fcurrent = flast.
  //i.e. fsingleelement should only used if fcurrent = flast = nil.
  result := (fcurrent = nil) and (fsingleelement <> nil);
  if result then begin
    fcurrent := @fsingleelement; //if this was moved to a virtual function of IXQValue, it could be used for staged lazy evaluation; each stage having its own current and last
    flast := @fsingleelement;
  end;
end;

function TXQValueEnumeratorPtrUnsafe.MoveNext: Boolean;
begin
  result := fcurrent < flast; //the multivalue loop case comes first, because if it occurs many times
  if result then
    inc(fcurrent)
  else
    result := MoveNextSingleElement; //not inlined since it only occurs once
end;

function TXQValueEnumeratorPtrUnsafe.MoveMany(count: sizeint): Boolean;
begin
  result := true;
  while count > 0 do begin
    result := MoveNext;
    if not result then exit;
    dec(count);
    if fcurrent + count > flast then begin
      count -= (flast - fcurrent) div sizeof(IXQValue);
      fcurrent := flast;
    end else begin
      fcurrent += count;
      count := 0;
    end;
  end;
end;

procedure TXQValueEnumeratorPtrUnsafe.CopyBlock(target: PIXQValue);
var
  size: SizeInt;
  endtarget: Pointer;
begin
  while MoveNext do begin
    size := PtrInt(pointer(flast) - pointer(fcurrent)) + sizeof(IXQValue);
    move(fcurrent^, target^, size  );
    endtarget := pointer(target) + size;
    while target < endtarget do begin
      target^._AddRef;
      inc(target);
    end;
    fcurrent := flast;
  end;
end;

//The iterator will copy from the element after current
//After the copying current will be the last copied element
procedure TXQValueEnumeratorPtrUnsafe.CopyBlock(target: PIXQValue; count: SizeInt);
var
  size, maxsize: SizeInt;
  endtarget: Pointer;
begin
  maxsize := count * sizeof(IXQValue);
  while (maxsize > 0) and MoveNext  do begin
    size := PtrInt(pointer(flast) - pointer(fcurrent)) + sizeof(IXQValue);
    if size > maxsize then size := maxsize;
    move(fcurrent^, target^, size  );
    endtarget := pointer(target) + size;
    while target < endtarget do begin
      target^._AddRef;
      inc(target);
    end;
    maxsize -= size;
    fcurrent := pointer(fcurrent) + size - sizeof(IXQValue) ;
  end;
end;

procedure TXQValueEnumeratorPtrUnsafe.CopyToList(list: TXQVList; count: SizeInt);
begin
  list.reserve(list.Count + count);
  CopyBlock(@list.fbuffer[list.Count], count);
  list.fcount += count;
end;

function TXQValueEnumeratorPtrUnsafe.GetEnumerator: TXQValueEnumeratorPtrUnsafe;
begin
  result := self;
end;

function TXQValueEnumerator.GetCurrent: IXQValue;
begin
  result := ptr.current^;
end;

class procedure TXQValueEnumerator.clear(out enum: TXQValueEnumerator);
begin
  TXQValueEnumeratorPtrUnsafe.clear(enum.ptr);
end;

function TXQValueEnumerator.MoveNext: Boolean;
begin
  result := ptr.MoveNext;
end;

function TXQValueEnumerator.GetEnumerator: TXQValueEnumerator;
begin
  result := self;
end;

function TXQValueEnumeratorArrayTransparentUnsafe.MoveNext: Boolean;
label enter, leaveArray, currentEnumeratorMoveNext;
begin
  currentEnumeratorMoveNext:
    result := ptrs[currentEnumerator].MoveNext;
    if result then begin
      fcurrent := Ptrs[currentEnumerator].Current^.toValue;
      case fcurrent.kind of
        pvkArray, pvkSequence, pvkUndefined: goto enter;
        else exit;
      end;
    end else goto leaveArray;

  enter: //fcurrent = ptrs[currentEnumerator].current^ is an array or sequence
    if currentEnumerator = high(ptrs) then
      SetLength(ptrs, length(ptrs) * 2);
    inc(currentEnumerator);
    if fcurrent.kind = pvkArray then
      ptrs[currentEnumerator] := fcurrent.GetEnumeratorMembersPtrUnsafe
     else
      ptrs[currentEnumerator] := fcurrent.GetEnumeratorPtrUnsafe;
    goto currentEnumeratorMoveNext;

  leaveArray: //ptrs[currentEnumerator].current^ is nil
    dec(currentEnumerator);
    if currentEnumerator < 0 then exit(false);
    goto currentEnumeratorMoveNext;
end;


function TXQValueEnumeratorArrayTransparentUnsafe.GetEnumerator: TXQValueEnumeratorArrayTransparentUnsafe;
begin
  result := self
end;






function compareXQInDocumentOrder(temp: tobject; p1,p2: pointer): integer;
type PIXQValue = ^IXQValue;
begin
  ignore(temp);
  result:=TTreeNode.compareInDocumentOrder(PIXQValue(p1)^.toNode,PIXQValue(p2)^.toNode);
end;

procedure TXQVCustomList.sortInDocumentOrderUnchecked;
begin
  if fcount < 2 then exit;
  stableSort(@fbuffer[0], @fbuffer[fcount-1], sizeof(IXQValue), @compareXQInDocumentOrder);
end;

procedure TXQVCustomList.insertSingle(i: SizeInt; const child: IXQValue);
begin
  insert(i, child);
end;

procedure TXQVCustomList.addInArray(const value: IXQValue);
begin
  add(value);
end;



procedure TXQVCustomList.revert;
var
 h: SizeInt;
 i: SizeInt;
begin
  if count=0 then exit;
  h :=count-1;
  for i:=0 to count div 2 - 1 do //carefully here. xqswap(a,a) causes a memory leak
    xqswap(fbuffer[i], fbuffer[h-i]);
end;

procedure TXQVCustomList.sort(cmp: TPointerCompareFunction; data: TObject);
begin
  if count <= 1 then exit;
  stableSort(@fbuffer[0], @fbuffer[count-1], sizeof(fbuffer[0]), cmp, data);
end;


constructor TXQVList.create(acapacity: SizeInt);
begin
  reserve(acapacity);
  fcount := 0;
end;

constructor TXQVList.create(other: TXQVCustomList);
begin
  add(other);
end;

procedure TXQVList.insert(i: SizeInt; value: IXQValue);
var
 v: PIXQValue;
begin
  assert(value <> nil);
  case value.kind of
    pvkSequence: begin
      for v in value.GetEnumeratorPtrUnsafe do begin
        insertSingle(i, v^);
        i+=1;
      end;
    end;
    pvkUndefined: ;
    else insertSingle(i, value);
  end;
end;

procedure TXQVList.add(const value: IXQValue);
var
 othercount: SizeInt;
 enumerator: TXQValueEnumeratorPtrUnsafe;
begin
  assert(value <> nil);
  case value.kind of
    pvkSequence: begin
      othercount := value.getSequenceCount;
      if othercount > 0 then begin
        enumerator := value.GetEnumeratorPtrUnsafe;
        reserve(fcount + othercount);
        enumerator.copyBlock(@fbuffer[fcount]);
        inc(fcount, othercount);
      end;
    end;
    pvkUndefined: ;
    else begin
      if fcount = fcapacity then
        reserve(fcount + 1);
      PPointer(fbuffer)[fcount] := value;
      value._AddRef;
      fcount += 1;
    end;
  end;
end;

procedure TXQVList.addOrdered(const node: IXQValue);
var
 a,b,m, cmp: SizeInt;
 s: PIXQValue;
 childnode: TTreeNode;
begin
  case node.kind of
    pvkNode: begin
      childnode:=node.toNode;
      if (Count = 0) or (TTreeNode.compareInDocumentOrder(childnode, Items[count-1].toNode) > 0) then
        add(node)
      else if (TTreeNode.compareInDocumentOrder(childnode, Items[0].toNode) < 0) then
        insertSingle(0, node)
      else begin
        a := 0;
        b := count-1;
        while a < b do begin
          m := (a+b) div 2;
          cmp:=TTreeNode.compareInDocumentOrder(childnode, Items[m].toNode);
          if cmp <> 0 then begin
            if cmp < 0 then b := m-1
            else a := m + 1;
          end else begin
            a := m;
            b := m;
            break;
          end;
        end;
        for m := b to a do begin
          cmp:=TTreeNode.compareInDocumentOrder(childnode, Items[m].toNode);
          if cmp <> 0 then begin
            if cmp < 0 then begin insertSingle(m, node); exit; end
            else begin insertSingle(m + 1, node); exit; end;
          end else begin
            if childnode <> Items[m].toNode then insertSingle(m, node); //safety check. cmp only returns 0 for identical nodes
            exit;
          end;
        end;
        raise EXQEvaluationException.Create('pxp:INTERNAL', 'binary insert failed');
      end;
    end;
    pvkUndefined: ;
    pvkSequence:
      for s in node.GetEnumeratorPtrUnsafe do
        addOrdered(s^); //TODO: optimize
    else raise EXQEvaluationException.Create('pxp:INTERNAL', 'invalid merging');
  end;
end;

procedure TXQVList.add(node: TTreeNode);
var
  temp: TXQValueNode; //faster than ixqvalue
begin
  if fcount = fcapacity then
    reserve(fcount + 1);
  temp := TXQValueNode.create(node);
  PPointer(fbuffer)[fcount] := IXQValue(temp); //the cast on the left side avoids the fpc_assign call and implicit ref counting; the cast on the right side ensures we get the correct pointer without a temporary variable.
  temp._AddRef;
  fcount += 1;
end;

procedure TXQVList.add(list: TXQVCustomList);
var
  i: SizeInt;
begin
  for i := 0 to list.Count - 1 do list.fbuffer[i]._AddRef;
  reserve(count + list.Count);
  move(list.fbuffer[0], fbuffer[count], list.Count * sizeof(IXQValue));
  fcount += list.Count;
end;

procedure TXQVList.addOrdered(list: TXQVList);
var
  i: SizeInt;
begin
  for i := 0 to list.Count - 1 do addOrdered(list.fbuffer[i]);
end;

function TXQVList.stringifyNodes: TXQVList;
var
  i: SizeInt;
begin
  result := TXQVList.create(Count);
  for i := 0 to Count - 1 do
    result.add(fbuffer[i].stringifyNodes);
end;

function TXQVList.hasNodes: boolean;
var
  i: SizeInt;
begin
  for i := 0 to Count - 1 do
    if fbuffer[i].hasNodes then exit(true);
  result := false;
end;





{$IMPLICITEXCEPTIONS OFF}
function xqvalue: IXQValue;
begin
  result := TXQueryInternals.commonValuesUndefined;
  //result := TXQValueUndefined.Create;
end;

function xqvalue(const v: Boolean): IXQValue;
begin
  case v of
    true:  result := TXQueryInternals.commonValuesTrue;
    false: result := TXQueryInternals.commonValuesFalse;
  end;

  //result := TXQValueBoolean.Create(v);
end;

function xqvalueTrue: IXQValue;
begin
  result := TXQueryInternals.commonValuesTrue;
end;

function xqvalueFalse: IXQValue;
begin
  result := TXQueryInternals.commonValuesFalse;
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

{function xqvalue(v: TDateTime): IXQValue;
begin
  result := TXQValueDateTime.Create(v);
end;}

function xqvalue(intentionallyUnusedParameter: TDateTime): IXQValue;
begin
  result := nil;
  raise EXQEvaluationException.Create('', 'Directly converting a date time is not supported. (the respective function prevents an implicit datetime => float conversion)');
end;


function xqvalue(const v: BigDecimal): IXQValue;
begin
  result := TXQValueDecimal.Create(v);
end;

function xqvalue(const v: string): IXQValue; inline;
begin
  {if v = '' then
    result := commonValues[cvkEmptyString]
   else}
  result := TXQValueString.Create(v);
end;

function xqvalue(sl: TStringList): IXQValue;
var
  i: SizeInt;
  list: TXQVList;
begin
  if sl.Count = 0 then exit(xqvalue());
  if sl.Count = 1 then exit(xqvalue(sl[0]));

  list := TXQVList.create(sl.Count);
  for i:=0 to sl.Count - 1 do
    list.add(xqvalue(sl[i]));
  xqvalueSeqSqueezed(result, list)
end;

function TXQValue.typeAnnotation: TXSType;
begin
  result := ftypeAnnotation;
end;

procedure TXQValueSequence.add(const value: IXQValue);
begin
  seq.add(value);
end;


function xqvalue(const sl: array of string): IXQValue;
var
  resseq: TXQValueSequence;
  i: SizeInt;
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
  i: SizeInt;
begin
  resseq := TXQValueSequence.create(length(sl));
  for i := 0 to high(sl) do
    resseq.add(sl[i]);
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqvalue(const s: TXQHashsetStr): IXQValue;
var
  resseq: TXQValueSequence;
  k: string;
begin
  resseq := TXQValueSequence.create(s.Count);
  for k in s do
    resseq.add(xqvalue(k));
  result := resseq;
  xqvalueSeqSqueeze(result);
end;

function xqvalue(v: TTreeNode): IXQValue;
begin
  if v = nil then exit(xqvalue());
  result := TXQValueNode.Create(v);
end;

procedure xqvalueSeqSqueeze(var v: IXQValue);
begin
  if (v.getSequenceCount > 1) or (v.kind <> pvkSequence) then exit;
  v := v.get(1);
end;

procedure xqvalueSeqSqueezed(out result: IXQValue; l: TXQVList);
begin
  case l.Count of
    0: result := xqvalue();
    1: result := l[0];
    else begin
      result := TXQValueSequence.create(l);
      exit;
    end;
  end;
  l.free;
end;

procedure xqvalueSeqConstruct(var result: IXQValue; var seq: TXQValueSequence; const add: IXQValue);
begin
  if result = nil then result := add
  else begin
    if seq = nil then begin
      seq := TXQValueSequence.create(result);
      result := seq;
    end;
    seq.add(add);
  end;
end;




// If the value is a sequence of exactly one element. That should not happen and this function should not be used.
function xqvalueIsFakeSingleton(const v: IXQValue): boolean;
begin
  result := (v.kind = pvkSequence) and (v.getSequenceCount = 1) and (v.get(1) <> v)
end;



function xqvalueDecimalEqualInt(const d: IXQValue; const index: SizeInt): boolean;
begin
  result := d.toDecimal = index;
end;



{$IMPLICITEXCEPTIONS ON}




constructor TXQMapStringObject.Create;
begin
  inherited;
  OwnsObjects := false;
end;


function TXQFunctionParameterTypes.serialize(separator: string = ''): string;
var
  j: SizeInt;
  humanReadable: Boolean;
begin
  result := '';
  humanReadable := separator = '';
  if humanReadable then separator := ', ';
  if humanReadable then result := '(';
  for j := 0 to high(types) do begin
    if j <> 0 then result += separator;
    result += types[j].serialize;
  end;
  if humanReadable then result += ')';
  if returnType <> nil then begin
    if humanReadable then result += ' as '
    else result += separator;
    result += returnType.serialize
  end;
end;

function TXQFunctionParameterTypes.clone: TXQFunctionParameterTypes;
var
  i: SizeInt;
begin
  result := self;
  SetLength(result.types, length(types));
  for i := 0 to high(types) do
    result.types[i] := TXQTermSequenceType(types[i].clone);
  if returnType <> nil then
    result.returnType := TXQTermSequenceType(returnType.clone);
end;

procedure TXQFunctionParameterTypes.raiseErrorMessage(values: PIXQValue; count: sizeint; const context: TXQEvaluationContext;
  term: TXQTerm; const addendum: string);
var errCode: string = 'XPTY0004';
  function needsSpecialErrorCode(st: TXQTermSequenceType; const v: IXQValue): boolean;
  begin
    result := false;
    if not (st.kind in [tikFunctionTest, tikElementTest, tikAny, tikArrayTest, tikMapTest, tikJSONiqTest]) then begin
      case v.kind of
        pvkFunction, pvkObject: begin
          errCode := 'FOTY0013';
          exit(true);
        end;
        pvkArray: if v.Size = 1 then exit(needsSpecialErrorCode(st, (v as TXQValueJSONArray).seq[0]));
        else;
      end;
    end;
    if (context.staticContext.model in PARSING_MODEL3)
              and (st.kind = tikAtomic) and (st.atomicTypeInfo.storage = TXQValueQName)
              and ((v.kind = pvkNode) or v.instanceOf(baseSchema.untypedAtomic)) then begin
     errCode := 'XPTY0117';
     exit(true);
    end;
  end;

var
  errMessage, name: String;
  i: SizeInt;
begin
  for i := 0 to high(types) do
    if needsSpecialErrorCode(types[i], values[i]) then
      break;
  name := '';
  if (term is TXQTermNamedFunction) and (TXQTermNamedFunction(term).func is TXQAbstractFunctionInfo) then
    name := 'function ' + TXQAbstractFunctionInfo(TXQTermNamedFunction(term).func).name
  else if term is TXQTermBinaryOp then
    name := 'operator ' + TXQTermBinaryOp(term).op.name
  ;
  errMessage := 'Invalid types for '+name+'#'+IntToStr(count)+'.'+LineEnding;
  errMessage += 'Got: ';
  for i := 0 to high(types) do begin
    if i <> 0 then errMessage += ', ';
    errMessage += values[i].toXQuery();
  end;
  errMessage += LineEnding;
  errMessage += 'Expected: ' + serialize;
  errMessage += addendum;
  term.raiseEvaluationError(errCode, errMessage);
end;

procedure TXQFunctionParameterTypes.checkOrConvertTypes(values: PIXQValue; count: sizeint; const context: TXQEvaluationContext;
  term: TXQTerm);
var
  j: SizeInt;
begin
  for j := 0 to count -1 do
    if types[j].kind <> tikFunctionTest then begin
      if not TXQAbstractFunctionInfo.checkType(values[j], types[j], context) then
        raiseErrorMessage(values, count, context, term, '');
    end else
      TXQAbstractFunctionInfo.convertType(values[j], types[j], context, term);
end;




function TXQTermContextItem.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  if context.SeqValue <> nil then result := context.SeqValue
  else if context.ParentElement <> nil then result := xqvalue(context.ParentElement)
  else if context.RootElement <> nil then result := xqvalue(context.RootElement)
  else begin context.raiseXPDY0002ContextItemAbsent(); result := nil; end
end;

function TXQTermContextItem.debugTermToString: string;
begin
  Result:='.';
end;

function TXQTermContextItem.clone: TXQTerm;
begin
  Result:=TXQTermContextItem.Create;
end;

function TXQTermContextItem.getContextDependencies: TXQContextDependencies;
begin
  result := ALL_CONTEXT_DEPENDENCIES - [xqcdFocusPosition, xqcdFocusLast, xqcdContextTime, xqcdContextVariables, xqcdContextOther];
end;



constructor TXQMapStringOwningObject.Create;
begin
  inherited create;
  Sorted := true;
  OwnsObjects:=true;
  Duplicates := dupAccept;
  CaseSensitive := true;
end;

function TXQMapStringOwningObject.DoCompareText(const s1, s2: string): PtrInt;
begin
  Result:=CompareStr(s1, s2);
end;

function TXQPathMatchingStep.serialize: string;
var
  i: SizeInt;
begin
  result := '';
  if typ <> tneaFunctionSpecialCase then begin
   if not (qmAttribute in matching) then
     case typ of
       tneaSameNode: result := 'self::';
       tneaDirectParent: result := 'parent::';
       tneaDirectChild: result := '';//child::';
       tneaDirectChildImplicit: result := '';
       tneaSameOrDescendant: result := 'same-or-descendant::';
       tneaDescendant: result := 'descendant::';
       tneaFollowing: result := 'following::';
       tneaFollowingSibling: result := 'following-sibling::';
       tneaAncestor: result := 'ancestor::';
       tneaPrecedingSibling: result := 'preceding-sibling::';
       tneaPreceding: result := 'preceding::';
       tneaSameOrAncestor: result := 'same-or-ancestor::';
       tneaDocumentRoot: result := 'root::';
       tneaFunctionSpecialCase: result := '(:special-case::)';
       else result := '???';
     end;
   if (matching = [qmElement,qmText,qmComment,qmProcessingInstruction,qmAttribute,qmDocument]) and (requiredType = nil)  then result += 'node()'
   else if (matching = [qmElement,qmAttribute]) and (requiredType = nil) then result += '@*'
   else if (matching = [qmValue, qmElement, qmCheckNamespaceURL]) and (requiredType = nil) then result := 'Q{}' + value
   else begin
     if qmElement in matching then result += 'element';
     if qmText in matching then result += 'text';
     if qmComment in matching then result += 'comment';
     if qmProcessingInstruction in matching then result += 'processing-instruction';
     if qmAttribute in matching then result += 'attribute';
     if qmDocument in matching then result += 'document-node';
     result += '(';
     if qmCheckNamespaceURL in matching then
       if namespaceURLOrPrefix = 'http://www.w3.org/2010/xslt-xquery-serialization' then result += 'output:'
       else result += 'Q{'+namespaceURLOrPrefix+'}';
     if qmCheckNamespacePrefix in matching then result += namespaceURLOrPrefix + ':';
     if qmValue in matching then result += value;
     if qmCheckOnSingleChild in matching then result += 'element(*)';

     if requiredType <> nil then result += ', '+requiredType.serialize;
     result += ')';
   end;
  end;
  for i := 0 to high(filters) do
    result += '[' + filters[i].filter.ToString + ']';
end;

function TXQPathMatchingStep.clone: TXQPathMatchingStep;
var
  i: SizeInt;
begin
  result.typ := typ;
  result.namespaceURLOrPrefix := namespaceURLOrPrefix;
  result.value:=value;
  result.valueHash:=valueHash;
  result.filters:=filters;
  SetLength(result.filters, length(result.filters));
  for i := 0 to high(result.filters) do result.filters[i].filter:= result.filters[i].filter.clone;
  if requiredType = nil then result.requiredType:=nil
  else result.requiredType:=TXQTermSequenceType(requiredType.clone);
  if typ = tneaFunctionSpecialCase then result.specialCase :=specialCase.clone
  else result.matching:=matching;
end;

function TXQPathMatchingStep.namespaceChecked: boolean;
begin
  result := [qmCheckNamespaceURL,qmCheckNamespacePrefix] * matching <> [];
end;

procedure TXQPathMatchingStep.destroy;
var
  i: SizeInt;
begin
  for i := 0 to high(filters) do filters[i].filter.free;
  requiredType.Free;
  requiredType := nil;
  if typ = tneaFunctionSpecialCase then specialCase.free;
end;

constructor TXQDecimalFormat.Create;
begin
  formats := XQDefaultDecimalFormat;
end;

constructor TXQDecimalFormat.CreateEmpty;
begin

end;

function TXQDecimalFormat.clone: TXQDecimalFormat;
begin
  result := TXQDecimalFormat.CreateEmpty;
  result.namespaceURL := namespaceURL;
  result.localname := localname;
  result.formats := formats;
end;

constructor TXQTermEQNameToken.Create;
begin

end;

constructor TXQTermEQNameToken.Create(const anamespaceurl, anamespaceprefix, alocalpart: string);
begin
  namespaceurl := anamespaceurl;
  namespaceprefix := anamespaceprefix;
  localpart := alocalpart;
end;

function TXQTermEQNameToken.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermEQNameToken(result).namespaceurl := namespaceurl;
  TXQTermEQNameToken(result).namespaceprefix := namespaceprefix;
  TXQTermEQNameToken(result).localpart := localpart;
end;

function TXQTermEQNameToken.evaluate(var context: TXQEvaluationContext): IXQValue;
begin
  ignore(context);
  raiseEvaluationError('pxp:internal','Internal error 160117');
  result := nil;
end;

function TXQTermEQNameToken.getContextDependencies: TXQContextDependencies;
begin
  raiseEvaluationError('pxp:internal','Internal error 160117');
  result := [];
end;

function TXQTermEQNameToken.debugTermToString: string;
begin
  result := '';
  if namespaceurl <> '' then result += 'Q{'+namespaceurl+'}';
  if namespaceprefix <> '' then result += namespaceprefix + ':';
  result += localpart;
end;


constructor TXQTermPendingEQNameToken.create;
begin

end;

constructor TXQTermPendingEQNameToken.create(anamespaceurl, aprefix, alocalpart: string; amode: TXQNamespaceMode; somedata: integer);
begin
  inherited Create(anamespaceurl, aprefix, alocalpart);
  mode := amode;
  data := somedata;
end;

constructor TXQTermPendingEQNameToken.create(anamespaceurl, aprefix, alocalpart: string; amode: TXQNamespaceMode;
  realterm: TXQTermPendingEQNameTokenPending);
begin
  inherited Create(anamespaceurl, aprefix, alocalpart);
  pending := realterm;
  mode := amode;
end;

function TXQTermPendingEQNameToken.resolveURI(const staticContext: TXQStaticContext; kind: TXQDefaultNamespaceKind): string;
begin
  if mode = xqnmPrefix then begin
    namespaceurl := staticContext.findNamespaceURLMandatory(namespaceprefix, kind);
  end;
  result := TNamespace.uniqueUrl(namespaceurl);
end;

function TXQTermPendingEQNameToken.resolveAndFree(const staticContext: TXQStaticContext): TXQTerm;
begin
  case pending of
    xqptVariable: begin
      result := TXQTermVariable.create(localpart, resolveURI(staticContext, xqdnkUnknown));
    end;
    xqptAttribute: begin
      result := TXQTermEQNameToken.create(resolveURI(staticContext, xqdnkUnknown), namespaceprefix, localpart);
    end;
    xqptElement: begin
      result := TXQTermEQNameToken.create(resolveURI(staticContext, xqdnkElementType), namespaceprefix, localpart);
    end;
    else{xqptUnknown: }begin raise EXQParsingException.create('XPST0003', 'Internal error 20160101181238'); result := nil; end;
  end;
  free;
end;

function TXQTermPendingEQNameToken.clone: TXQTerm;
begin
  Result:=inherited clone;
  TXQTermPendingEQNameToken(result).mode := mode;
  TXQTermPendingEQNameToken(result).data := data;
  TXQTermPendingEQNameToken(result).pending := pending;
end;


constructor TXQEQNameWithPrefix.create;
begin

end;

constructor TXQEQNameWithPrefix.create(const ns: INamespace; const local: string);
begin
  namespaceURL := ns.getURL;
  namespacePrefix := ns.getPrefix;
  localname := local;
end;

constructor TXQEQNameWithPrefix.create(const nsurl, nsprf, local: string);
begin
  namespaceURL := nsurl;
  namespacePrefix := nsprf;
  localname := local;
end;

function TXQEQNameWithPrefix.clone: TXQEQNameWithPrefix;
begin
  result := TXQEQNameWithPrefix.create(namespaceURL, namespacePrefix, localname);
end;

constructor TXQEQName.create;
begin

end;

constructor TXQEQName.create(const ns, local: string);
begin
  namespaceURL := ns;
  localname := local;
end;

function TXQEQName.clone: TXQEQName;
begin
  result := TXQEQName.create(namespaceURL, localname);
end;

function TXQEQName.ToString: ansistring;
begin
  result := 'Q{' + namespaceURL + '}' + localname;
end;

function TXQEQName.isEqual(const ns, local: string): boolean;
begin
  result := (namespaceURL = ns) and (localname = local);
end;

procedure TXQValueDateTimeData.initFromMicroSecondStamp(mics: int64; const tz: integer);
begin
  mics := initFromMicroSecondStampTimeOnly(mics, tz);
  dateDecode(mics, @year, @month, @day);
end;

function TXQValueDateTimeData.initFromMicroSecondStampTimeOnly(mics: int64; const tz: integer): int64;
begin
  result := mics div (MicroSecsPerSec * 60 * 60 * 24);
  mics := mics - result * (MicroSecsPerSec * 60 * 60 * 24);
  if mics < 0 then mics += (MicroSecsPerSec * 60 * 60 * 24);

  microsecs := mics mod MicroSecsPerSec; mics := mics div MicroSecsPerSec;
  seconds   := mics mod 60; mics := mics div 60;
  min       := mics mod 60; mics := mics div 60;
  hour      := mics mod 24; mics := mics div 24;
  timezone := tz;
end;

function TXQValueDateTimeData.toMicroSecondStamp(subtractTimeZone: Boolean = true): int64;
var
  tempmin: int64;
  dayStamp: Int64;
  timeStamp: Int64;
begin
  tempmin := min;
  if subtractTimeZone and (timezone <> high(Integer)) then tempmin -= timezone;
  dayStamp := trunc(dateEncode(year, month, day));
  timeStamp := hour * 3600  + tempmin *60 + seconds;
  result := (dayStamp * int64(SecsPerDay) + timeStamp) * MicroSecsPerSec + microsecs;
end;

function TXQValueDateTimeData.toMonths: integer;
begin
  result := 12 * year + month;
end;

function TXQValueDateTimeData.toDayTime: int64;
begin
  result := microsecs + MicroSecsPerSec * (seconds + 60 * (min + 60 * (hour + 24 * int64(day))));
end;





{ TXQFunctionParameter }

function TXQFunctionParameter.toString(def: string): string;
begin
  if variable = nil then exit(def);
  result := variable.ToString;
  if seqtype <> nil then
    result += ' as ' + seqtype.serialize;
end;


{ TXQInterpretedFunctionInfo }
var GlobalInterpretedNativeFunctionStaticContext: TXQStaticContext;
    CurrentInitializations: array of TXQInterpretedFunctionInfo;

procedure TXQInterpretedFunctionInfo.initialize();
var
  temp: TXQueryEngine;
  tempQuery: TXQuery;
  i: SizeInt;
  needNewEngine: Boolean;
begin
  if definition <> nil then exit;
  if definition = nil then begin
   EnterCriticalsection(interpretedFunctionSynchronization);
   try
     if definition <> nil then exit;
     if length(CurrentInitializations) > 0 then begin
       for i := 0 to high(CurrentInitializations) do
         if CurrentInitializations[i] = self then exit;
       if length(CurrentInitializations) > 100 then raise EXQEvaluationException.create('PXP', 'Internal recursion 1601161646');
     end;
     setlength(CurrentInitializations, length(CurrentInitializations) + 1);
     CurrentInitializations[high(CurrentInitializations)] := self;
     try
       needNewEngine := length(CurrentInitializations) = 1;
       if needNewEngine then begin
         temp := TXQueryEngine.create;
         temp.StaticContext.free;
       end else temp := GlobalInterpretedNativeFunctionStaticContext.sender;
       try
         if namespace <> nil then temp.GlobalNamespaces.add(namespace);
         GlobalInterpretedNativeFunctionStaticContext.sender := temp;
         temp.StaticContext := GlobalInterpretedNativeFunctionStaticContext;
         tempQuery := temp.parseTerm(source, xqpmXQuery3_1, temp.StaticContext);
         definition := tempQuery.fterm as TXQTermDefineFunction;
         func := tempQuery.evaluate() as TXQValueFunction;
         func._AddRef;
         tempQuery.fTerm := nil;
         tempQuery.Free;
       finally
         if needNewEngine then begin
            temp.StaticContext := nil;
            GlobalInterpretedNativeFunctionStaticContext.sender := nil;
            temp.free;
         end;
       end;
     finally
       SetLength(CurrentInitializations, high(CurrentInitializations));
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
    message := message + ':'+LineEnding+value.toXQuery();
  term := aterm;
  if term <> nil then
    message := message + LineEnding+'in '+term.ToString;
end;


{ EXQParsingException }

constructor EXQParsingException.create(aerrcode, amessage: string; anamespace: INamespace);
begin
  inherited;
end;

destructor EXQParsingException.Destroy;
begin
  next.free;
  inherited Destroy;
end;



{ EXQException }

constructor EXQException.create(aerrcode, amessage: string; anamespace: INamespace = nil);
begin
  rawMessageLength := length(amessage);
  if strBeginsWith(aerrcode, 'pxp:') then begin
    delete(aerrcode, 1, 4);
    namespace := TNamespace.make(XMLNamespaceURL_MyExtensionsMerged, 'pxp');
  end else if strBeginsWith(aerrcode, 'x:') then begin
    delete(aerrcode, 1, 4);
    namespace := TNamespace.make(XMLNamespaceURL_MyExtensionsNew, 'x');
  end else if strBeginsWith(aerrcode, 'err:') then begin
    delete(aerrcode, 1, 4);
    namespace := TNamespace.make(XMLNamespaceURL_XQTErrors, 'err');
  end else if strBeginsWith(aerrcode, 'jerr:') then begin
    delete(aerrcode, 1, 5);
    namespace := TNamespace.make('http://jsoniq.org/errors', 'jerr');
  end else if anamespace = nil then namespace := TNamespace.make(XMLNamespaceURL_XQTErrors, 'err')
  else namespace := anamespace;
  errorCode:=aerrcode;
  inherited create(messagePrefix + amessage);
end;

var nativeModules: TStringList;


class function EXQException.searchClosestFunction(const addr: pointer): string;
const terms: array[1..40] of TXQTermClass = (TXQTermWithChildren, TXQTermVariable, TXQTermSequenceType, TXQTermDefineFunction, TXQTerm, TXQTermWithChildren, TXQTermConstant, TXQTermSequence, TXQTermJSONArray, TXQTermSequenceType, TXQTermVariable, TXQTermDefineVariable, TXQTermDefineFunction, TXQTermNodeMatcher, TXQTermFilterSequence, TXQTermPatternMatcher, TXQTermNamedFunction, TXQTermDynamicFunctionCall, TXQTermBinaryOp  , TXQTermFlowerSubClause, TXQTermFlowerLet, TXQTermFlowerFor, TXQTermFlowerWindow, TXQTermFlowerLetPattern, TXQTermFlowerForPattern, TXQTermFlowerWhere, TXQTermFlowerOrder, TXQTermFlowerCount, TXQTermFlowerGroup, TXQTermFlower, TXQTermSomeEvery, TXQTermIf, TXQTermTypeSwitch, TXQTermSwitch, TXQTermReadObjectProperty, TXQTermConstructor, TXQTermMapConstructor, TXQTermJSONiqObjectConstructor, TXQTermTryCatch, TXQTermModule);
var
  i, j: SizeInt;
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
  delta := {%H-}PtrUInt(addr);
  for i := low(terms) to high(terms) do
    checkAddr( TMethod(@terms[i].evaluate).Code,  terms[i].ClassName);

  for i := 0 to nativeModules.Count - 1 do begin
    module := TXQNativeModule(nativeModules.Objects[i]);
    for j := 0 to module.basicFunctions.Count - 1 do
      checkAddr( TXQBasicFunctionInfo(module.basicFunctions.Objects[j]).func, 'Q{' +nativeModules[i] + '}'+ module.basicFunctions[j]);
    for j := 0 to module.complexFunctions.Count - 1 do
    checkAddr( TXQComplexFunctionInfo(module.complexFunctions.Objects[j]).func, 'Q{' +nativeModules[i] + '}'+ module.complexFunctions[j]);
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





var   XMLNamespace_XMLSchemaInstance, XMLNamespace_XQueryLocalFunctions, XMLNamespace_XQuery: TNamespace;


function namespaceReverseLookup(const url: string): TNamespace;
begin
  if url = XMLNamespaceURL_XPathFunctions then result := XMLNamespace_XPathFunctions
  else if url = XMLNamespaceURL_MyExtensionsNew then result := XMLNamespace_MyExtensionsNew
  else result := TNamespace.make(url, 'prefix');
end;


function charUnicodeZero(const cp: integer): integer;
const UNICODE_ZEROS: array[1..55] of integer = ($0030,$0660,$06F0,$07C0,$0966,$09E6,$0A66,$0AE6,$0B66,$0BE6,$0C66,$0CE6,$0D66,$0DE6,$0E50,$0ED0,$0F20,$1040,$1090,$17E0,$1810,$1946,$19D0,$1A80,$1A90,$1B50,$1BB0,$1C40,$1C50,$A620,$A8D0,$A900,$A9D0,$A9F0,$AA50,$ABF0,$FF10,$104A0,$11066,$110F0,$11136,$111D0,$112F0,$114D0,$11650,$116C0,$11730,$118E0,$16A60,$16B50,$1D7CE,$1D7D8,$1D7E2,$1D7EC,$1D7F6);
var
  i: Integer;
begin
  for i := low(UNICODE_ZEROS) to high(UNICODE_ZEROS) do
    if (cp >= UNICODE_ZEROS[i]) and (cp < UNICODE_ZEROS[i] + 10) then
      exit(UNICODE_ZEROS[i]);
  result := -11;
end;

{$I disableRangeOverflowChecks.inc}





{$I restoreRangeOverflowChecks.inc}

function isValidXMLCharacter(const codepoint: integer): boolean; inline;
begin
  case codepoint of
    $20..$D7FF, $E000..$FFFD, $10000..$10FFFF: result := true;
    $1..$1F: result := (codepoint in [$9,$A,$D]) or (baseSchema.version = xsd11);
    else result := false;
  end;
end;

class function TXQueryInternals.treeElementAsString(node: TTreeNode; deepSeparator: string = ''): string; inline;
begin
  if (node = nil) then exit('');
  case node.typ of
    tetText, tetComment: result:=node.value;
    tetAttribute: result := TTreeAttribute(node).realvalue;
    tetOpen, tetDocument: result := node.deepNodeText(deepSeparator);
    tetProcessingInstruction: exit(node.getAttribute(''));
    else exit('');
  end;
  if XQGlobalTrimNodes then result := strTrim(Result);
end;









procedure requiredArgCount(argc: sizeint; minc: sizeint; maxc: sizeint);
begin
  if maxc = -2 then maxc := minc;
  if (argc >= minc) and (argc <= maxc) then exit;
  if minc = maxc then raise EXQEvaluationException.Create('XPST0017', IntToStr(argc) + ' arguments passed, need exactly '+IntToStr(minc))
  else raise EXQEvaluationException.Create('XPST0017', IntToStr(argc) + ' arguments passed, need between '+IntToStr(minc)+ ' and ' + inttostr(maxc));
end;
procedure requiredArgType(const v: IXQValue; typ: TXSType);
begin
  if not (v.instanceOf(typ)) then
    raise EXQEvaluationException.create('XPTY0004', 'Expected '+typ.name+', got: '+v.toXQuery());
end;

function TXSType.createValue(const v: IXQValue): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then TXQueryInternals.raiseXSCEError(err, v.toXQuery(), name);
  result := temp;
end;

procedure TXSType.failCreateValue(err: TXSCastingError; v: int64);
begin
  TXQueryInternals.raiseXSCEError(err, IntToStr(v), name)
end;
function TXSType.createValue(const v: Int64): IXQValue;
var err: TXSCastingError;
    temp: TXQValue;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then failCreateValue(err, v);
  result := temp;
end;

function TXSType.createValue(const v: xqfloat): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;

procedure fail;
begin
  TXQueryInternals.raiseXSCEError(err, FloatToStr(v), name);
end;

begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then fail;
  result := temp;
end;

function TXSType.createValue(const v: BigDecimal): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then TXQueryInternals.raiseXSCEError(err, BigDecimalToStr(v), name);
  result := temp;
end;

function TXSType.createValue(const v: String): IXQValue;
var temp: TXQValue;
  err: TXSCastingError;
begin
  err := tryCreateValue(v, @temp);
  if err <> xsceNoError then TXQueryInternals.raiseXSCEError(err, v, name);
  result := temp;
end;


function xqvalueAtomize(const v: IXQValue): IXQValue;
var x: PIXQValue;
  isAlreadyAtomized: Boolean;
  seqResult: TXQValueSequence;
  t: TXSType;
begin
  if v.getSequenceCount = 0 then exit(v);
  case v.kind of
    pvkSequence: begin
      isAlreadyAtomized := true;
      for x in v.GetEnumeratorPtrUnsafe do
        if not x^.instanceOf(baseSchema.AnyAtomicType) then begin
          isAlreadyAtomized := false;
          break;
        end;
      if isAlreadyAtomized then exit(v);
      seqResult := TXQValueSequence.create(v.getSequenceCount);
      for x in v.GetEnumeratorPtrUnsafe do seqResult.seq.add(xqvalueAtomize(x^));
      result := seqResult;
      exit
    end;
    pvkNode: begin
      t := TXQValueNode.nodeTypeAnnotationForAtomic(v.toNode);
      result := t.createValue(v.toString);
    end;
    pvkArray: begin
      seqResult := TXQValueSequence.create(v.getSequenceCount);
      for x in v.GetEnumeratorMembersPtrUnsafe do seqResult.seq.add(xqvalueAtomize(x^));
      result := seqResult;
      exit;
    end;
    pvkFunction, pvkObject: raise EXQEvaluationException.create('FOTY0013', 'Maps and function values cannot be atomized.' + v.toXQuery);
    else if v.instanceOf(baseSchema.AnyAtomicType) then exit(v)
    else raise EXQEvaluationException.Create('XPTY0004','Invalid value for atomization: '+v.toXQuery());
  end;
end;


function TXQVariableChangeLog.get(i: SizeInt): IXQValue; inline;
begin
  result := varstorage[i].value;
end;


function TXQStaticContext.equalDeepAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): boolean;
begin
  result := equalDeepAtomic(a.toValue, b.toValue, overrideCollation);
end;


{$ImplicitExceptions off}
class procedure TXQValueOwnershipTracker.addRef(v: TXQValue);
begin
  v._AddRef;
end;

class procedure TXQValueOwnershipTracker.release(v: TXQValue);
begin
  v._Release;
end;

class function TXQValueOwnershipTracker.hash(const v: IXQValue): uint32;
begin
  result := v.hashCode;
end;

class function TXQValueOwnershipTracker.equal(const v, w: IXQValue): boolean;
  function compareIntToFloat(i: int64; f: xqfloat): boolean;
  begin
    result := i = f;
  end;
  function compareIntToBCD(i: int64; const bcd: BigDecimal): boolean;
  begin
    result := i = bcd;
  end;
  function compareFloatToBCD(f: xqfloat; const bcd: BigDecimal): boolean;
  begin
    result := FloatToBigDecimal(f, bdffExact) = bcd;
  end;

var
  vk, wk: TXQValueKind;
begin
  vk := v.kind;
  wk := w.kind;
  if vk = wk then begin
    case vk of
      pvkString: exit(v.toString = w.toString);
      pvkBinary: exit( (v.typeAnnotation.base = w.typeAnnotation.base) and (v.toString = w.toString)) ;
      pvkFloat:
        exit(   (v.toFloat = w.toFloat)
            or  (v.toFloat.IsNan() and w.toFloat.IsNan())
        );
      pvkInt64: exit(v.toInt64 = w.toInt64);
      pvkBigDecimal: exit(v.toDecimal = w.toDecimal);
      pvkDateTime:
        if v.instanceOf(baseSchema.duration) <> w.instanceOf(baseSchema.duration) then exit(false)
        else if not v.instanceOf(baseSchema.duration) and (v.getInternalDateTimeData.hasTimeZone <> w.getInternalDateTimeData.hasTimeZone) then
          exit(false);
      pvkBoolean, pvkQName: {fallthrough};
      else exit(false);
    end;
  end else begin
    if v.hashCode <> w.hashCode then exit(false);
    case vk of
      pvkFloat: if not (v.toFloat.isFinite) then exit(false);
      pvkInt64, pvkBigDecimal: {fallthrough};
      else exit(false)
    end;
    case wk of
      pvkFloat: if not (w.toFloat.isFinite) then exit(false);
      pvkInt64, pvkBigDecimal: {fallthrough};
      else exit(false)
    end;
    case vk of
      pvkInt64: case wk of
        pvkFloat: exit(compareIntToFloat(v.toInt64, w.toFloat));
        pvkBigDecimal: exit(compareIntToBCD(v.toInt64, w.toDecimal));
        else ;
      end;
      pvkBigDecimal: case wk of
        pvkFloat: exit(compareFloatToBCD(w.toFloat, v.toDecimal));
        pvkInt64: exit(compareIntToBCD(w.toInt64, v.toDecimal));
        else ;
      end;
      pvkFloat: case wk of
        pvkInt64: exit(compareIntToFloat(w.toInt64, v.toFloat));
        pvkBigDecimal: exit(compareFloatToBCD(v.toFloat, w.toDecimal));
        else ;
      end;
      else ;
    end;
    exit(false);
  end;
  result := GlobalInterpretedNativeFunctionStaticContext.equalDeepAtomic(v,w,nil);
end;

class function TXQValueOwnershipTracker.isKeyStringLike(const v: TXQValue): boolean;
begin
  result := (v.kind = pvkString) and (v.instanceOf(baseSchema.string_) or v.instanceOf(baseSchema.untypedAtomic) or v.instanceOf(baseSchema.anyURI))
end;

class function TXQValueOwnershipTracker.isStringLikeAfterAtomize(const v: TXQValue): boolean;
var
  w: TXQValue;
begin
  case v.kind of
    pvkString: result := (v.instanceOf(baseSchema.string_) or v.instanceOf(baseSchema.untypedAtomic) or v.instanceOf(baseSchema.anyURI));
    pvkNode: result := true;
    pvkSequence, pvkArray: begin
      result := false;
      for w in v.GetEnumeratorArrayTransparentUnsafe do
        if result or not isStringLikeAfterAtomize(w) then exit(false)
        else result := true;
    end
    else result := false;
  end;
end;

class function TXQValueOwnershipTracker.isAtomicKeyValue(const v: IXQValue): boolean;
begin
  case v.kind of
    pvkSequence, pvkNode, pvkArray, pvkFunction, pvkObject, pvkUndefined: result := false;
    else result := true;
  end;
end;

{$ImplicitExceptions on}



function xqvalueDeep_equal(const context: TXQEvaluationContext; const a, b: IXQValue; collation: TXQCollation): boolean;
  procedure raiseFOTY0015(const v: IXQValue);
  begin
    raise EXQEvaluationException.create('FOTY0015', 'Function item ' + v.toXQuery() + ' passed to deep-equal')
  end;

  function compareStandardMapToStringMap(standardMap: PIXQValue; stringMap: PIXQValue): boolean;
  var
    pp2: TXQStandardProperty;
    k, tempv: TXQValue;
  begin
    result := true;
    for pp2 in standardMap.getEnumeratorPropertiesUnsafe do begin
      k := pp2.key.toValue;
      if not TXQValueOwnershipTracker.isKeyStringLike(k) then
        exit(false);
      if not stringMap.hasProperty(k.toString, @tempv) then exit(false)
      else if not xqvalueDeep_equal(context, pp2.Value, tempv, collation) then exit(false);
    end;
  end;

var i, j:SizeInt;
    enum1, enum2, enum1array, enum2array: TXQValueEnumeratorPtrUnsafe;
    pa, pb: PIXQValue;
    tempv: TXQValue;
    pp: TXQProperty;
    pp2: TXQStandardProperty;
begin
  if a.getSequenceCount <> b.getSequenceCount then
    exit(false);

  enum1 := a.GetEnumeratorPtrUnsafe;
  enum2 := b.GetEnumeratorPtrUnsafe;
  for i := 0 to a.getSequenceCount - 1 do begin
    enum1.MoveNext;
    enum2.MoveNext;
    pa := enum1.Current;
    pb := enum2.Current;
    case pb.kind of
      pvkFunction: raiseFOTY0015(pb^);
      pvkArray: begin
        if pa.kind <> pvkArray then exit(false);
        if pa.Size <> pb.Size then exit(false);
        enum1array := pa.GetEnumeratorMembersPtrUnsafe;
        enum2array := pb.GetEnumeratorMembersPtrUnsafe;
        for j := 0 to pa.Size - 1 do begin
          enum1array.MoveNext;
          enum2array.MoveNext;
          result := xqvalueDeep_equal(context, enum1array.Current^, enum2array.Current^, collation);
          if not result then exit;
        end;
      end;
      pvkObject: begin
        if pa.kind <> pvkObject then exit(false);
        if pa.Size <> pb.Size then exit(false);
        case pa.getPropertyKeyKind of
          xqmpkkStringKeys:
            case pb.getPropertyKeyKind of
              xqmpkkStringKeys:
                for pp in pa.getEnumeratorStringPropertiesUnsafe do
                  if not pb.hasProperty(pp.key, @tempv) then exit(false)
                  else if not xqvalueDeep_equal(context, pp.Value, tempv, collation) then exit(false);
              xqmpkkStandardKeys:
                result := compareStandardMapToStringMap(pb, pa);
            end;
          xqmpkkStandardKeys:
            case pb.getPropertyKeyKind of
              xqmpkkStringKeys:
                result := compareStandardMapToStringMap(pa, pb);
              xqmpkkStandardKeys:
                for pp2 in pa.getEnumeratorPropertiesUnsafe do
                  if not pb.hasProperty(pp2.key, @tempv) then exit(false)
                  else if not xqvalueDeep_equal(context, pp2.Value, tempv, collation) then exit(false);
            end;
        end;
      end
      else begin
        if pa.instanceOf(baseSchema.AnyAtomicType) then begin
          if not (pb.instanceOf(baseSchema.anyAtomicType))
             or not context.staticContext.equalDeepAtomic(pa^, pb^, collation) then
            exit(false);
        end else begin
          case pa.kind of
            pvkFunction: raiseFOTY0015(pa^);
            pvkObject, pvkArray: exit(false);
            else ;
          end;
          if collation = nil then
            collation := context.staticContext.nodeCollation;
          if (pb.instanceOf(baseSchema.anyAtomicType)) or
             (((pa.kind = pvkNode) or (pb.kind = pvkNode))
                and not pa.toNode.isDeepEqual(pb.toNode, [tetProcessingInstruction, tetComment], @collation.equal)) then
            exit(false);
        end;
      end;
    end;
  end;
  result := true;
end;

type TRangeSpecial = (rsNormal, rsInfinity, rsBigDecimal);
procedure xpathRangeDefinition(argc: sizeint; args: PIXQValue; const maxLen: sizeint; out from, len: SizeInt);
var
  from64, len64, until64: SizeInt64;
  fromF, lenF, untilF: xqfloat;
label exitWithZeroLength, exitFromFromTillInfinity;
begin
  if args[1].kind = pvkInt64 then begin
    //sane fast path
    //todo: check if int64 fit in double, because the standard is double only?
    from64 := args[1].toInt64;
    if from64 > maxLen then goto exitWithZeroLength;
    if argc = 2 then begin
      if from64 >= 1 then from := from64
      else from := 1;
      goto exitFromFromTillInfinity;
    end else if args[2].kind = pvkInt64 then begin
      len64 := args[2].toInt64;
      //intersect the [from64,from64+len64[ range with the allowed [1,maxLen] range
      if len64 <= 0 then goto exitWithZeroLength; //this is written so weirdly to avoid overflows. Like now knowing len64 is positive, we can add negative numbers, but not positive ones
      if from64 >= 1 then begin
        from := from64;
        if len64 >= maxLen - from + 1 then goto exitFromFromTillInfinity;
        len := len64
      end else begin
        until64 := from64 + len64;
        from := 1;
        if until64 <= 0 then goto exitWithZeroLength;
        if until64 > maxLen then goto exitFromFromTillInfinity;
        len := until64 - {from}1;
      end;
      exit
    end;
  end;


  //standard double path
  fromF := args[1].toFloat;
  if fromF.isFinite then begin
    fromF := fromF.round();
    if fromF > maxLen then goto exitWithZeroLength;
  end else if fromF.IsNegativeInfinity then begin
    from := 1;
    if argc <= 2 then len := maxLen
    else len := 0;
    exit;
  end else goto exitWithZeroLength;

  if argc <= 2 then begin
    if fromF >= 1 then from := trunc(fromF)
    else from := 1;
    goto exitFromFromTillInfinity;
  end;

  lenF := args[2].toFloat;
  if lenF.isFinite() then begin
    lenF := lenF.round;
    if lenF <= 0 then goto exitWithZeroLength;
    if fromF >= 1 then begin
      from := trunc(fromF);
      if lenF >= maxLen - from + 1 then goto exitFromFromTillInfinity;
      len := trunc(lenF);
    end else begin
      untilF := fromF + lenF;
      from := 1;
      if untilF <= 0 then goto exitWithZeroLength;
      if untilF > maxLen then goto exitFromFromTillInfinity;
      len := trunc(untilF) - {from}1;
    end;
    exit;
  end else if lenF.IsPositiveInfinity then begin
    if fromF >= 1 then from := trunc(fromF)
    else from := 1;
    goto exitFromFromTillInfinity;
  end else len := 0;

  exit;

exitWithZeroLength:
  from := 1;
  len := 0;
  exit;

exitFromFromTillInfinity:
  len := maxLen - from + 1;
  exit;
end;

function xqFunctionConcat(argc: SizeInt; args: PIXQValue): IXQValue;
var temp:string;
 i: SizeInt;
begin
  temp:='';
  for i:=0 to argc-1 do begin
    case args[i].kind of
      pvkSequence: if args[i].getSequenceCount > 1 then raiseXPTY0004TypeError(args[i], 'singleton');
      pvkFunction: raiseFOTY0013TypeError(args[i]);
      else;
    end;
    temp+=args[i].toString;
  end;
  result := xqvalue(temp);
end;


{ TXQVariableStorage }

procedure TXQVariableChangeLog.add(name: string; const value: IXQValue; const namespaceURL: string);
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Readonly variable changelog modified');

  reserve(count + 1);

  varstorage[count].namespaceURL := namespaceURL;
  varstorage[count].name:=name;
  varstorage[count].value:=value;
  varstorage[count].propertyChange := false;

  inc(varcount);
end;

procedure TXQVariableChangeLog.addObjectModification(const variable: string; value: IXQValue; const namespaceURL: string; const props: PString; len: SizeInt);
var
  oldObj, newValue: IXQValue;
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Readonly variable changelog modified');
  if len <= 0 then begin
   add(variable, value, namespaceURL);
   exit;
  end;

  if not hasVariable(variable, oldObj, namespaceURL) then
    raise EXQEvaluationException.Create('pxp:OBJECT', 'Failed to find object variable '+variable+LineEnding+'(when changing properties: '+strJoin(props, len, '.')+')');

  case oldObj.kind of
    pvkArray, pvkObject: newValue := oldObj.toValue.setImmutable(props, len, value);
    else raise EXQEvaluationException.Create('pxp:OBJECT', 'Variable '+variable+' is not an object or array, but '+oldObj.toXQuery()+LineEnding+'(when changing properites: '+strJoin(props, len, '.')+')');
  end;

  reserve(count + 1);

  varstorage[count].namespaceURL := namespaceURL;
  varstorage[count].name:=variable;
  varstorage[count].value:=newValue;
  varstorage[count].propertyChange := true;

  inc(varcount);
end;

function TXQVariableChangeLog.toStringMap: TXQValueStringMap;
var
  i: SizeInt;
begin
  result := TXQValueStringMap.create();
  for i := 0 to count - 1 do
    result.setMutable(getName(i), get(i));
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
  add(variable.value, value, variable.namespace);
end;

procedure TXQVariableChangeLog.setOverride(const n,v: string);
begin
  setOverride(n, xqvalue(v), '');
end;

procedure TXQVariableChangeLog.setOverride(const n: string; const v: ixqvalue);
begin
  setOverride(n, v, '');
end;

procedure TXQVariableChangeLog.setOverride(const n: string; const v: ixqvalue; const namespaceURL: string);
var
  i: SizeInt;
begin
  i := indexOf(n, namespaceURL);
  if i >= 0 then varstorage[i].value := v
  else if (parentLog = nil) or parentLog.readonly then add(n, v, namespaceURL)
  else parentLog.setOverride(n, v, namespaceURL)
end;

function TXQVariableChangeLog.get(const name: string): IXQValue;
begin
  result := get(name, '');
end;

function TXQVariableChangeLog.get(const name: string; const namespaceURL: string): IXQValue;
var i:SizeInt;
begin
  i := indexOf(name, namespaceUrl);
  if i = -1 then
    if parentLog <> nil then exit(parentLog.get(name, namespaceURL))
    else exit(xqvalue());
  result := varstorage[i].value;
end;


function TXQVariableChangeLog.getString(const name: string): string;
begin
  result := get(name, '').toString;
end;

function TXQVariableChangeLog.isPropertyChange(i: SizeInt): boolean;
begin
  result := varstorage[i].propertyChange;
end;

function TXQVariableChangeLog.indexOf(const name: string;  const namespaceURL: string): sizeint;
var i:sizeint;
begin
  for i:=varCount - 1 downto 0 do
    if (varstorage[i].name = name) and (varstorage[i].namespaceURL = namespaceURL) then exit(i);
  exit(-1);
end;

function TXQVariableChangeLog.getName(i: SizeInt): string;
begin
  assert(i>=0); assert(i< count);
  result := varstorage[i].name;
end;

function TXQVariableChangeLog.getNamespace(i: SizeInt): string;
begin
  assert(i>=0); assert(i< count);
  result := varstorage[i].namespaceURL;
end;



function TXQVariableChangeLog.getAll(const name: string;  const namespaceURL: string): IXQValue;
var
  i: SizeInt;
  list: TXQVList;
begin
  result := xqvalue();
  list := TXQVList.create();
  for i:=0 to count - 1 do
    if (varstorage[i].name = name) and (varstorage[i].namespaceURL = namespaceURL) then
      list.add(varstorage[i].value);
  xqvalueSeqSqueezed(result, list)
end;

procedure TXQVariableChangeLog.clear;
var
  i: SizeInt;
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'readonly variable change log modified');
  if varCount > 256 then setlength(varstorage, 0)
  else for i := 0 to varCount - 1 do
    varstorage[i].value := nil; //free value to reduce memory usage
  if historyCount > 256 then setlength(histories, 0);
  varCount := 0;
  historyCount := 0;
end;

procedure TXQVariableChangeLog.remove(const name: string; const namespace: string);
var
  i: SizeInt;
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'Readonly variable changelog modified');
  for i := count - 1 downto 0 do begin
    if not strEqual(varstorage[i].name, name) or not strEqual(varstorage[i].namespaceURL, namespace) then continue;
    varstorage[i].name := '';
    varstorage[i].namespaceURL := '';
    varstorage[i].value := nil;
    dec(varcount);
    if i < varcount then begin
      move(varstorage[i + 1], varstorage[i], sizeof(varstorage[i]) * (varcount - i));
      FillChar(varstorage[varcount], sizeof(varstorage[count]), 0);
    end;
  end;
end;

function TXQVariableChangeLog.pushAll: SizeInt;
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'readonly variable change log modified');
  if historyCount = length(histories) then
    if length(histories) < 2 then SetLength(histories, 2)
    else SetLength(histories, length(histories) * 2);
  histories[historyCount] := varCount;
  result := historyCount;
  inc(historyCount);
end;

procedure TXQVariableChangeLog.popAll(level: SizeInt = -1);
var targetCount, targetHistoryCount, i: SizeInt;
begin
  if readonly then raise EXQEvaluationException.Create('pxp:INTERNAL', 'readonly variable change log modified');
  if level < 0 then targetHistoryCount := historyCount + level
  else targetHistoryCount := level;

  if targetHistoryCount >= historyCount then exit;
  if targetHistoryCount < 0 then
      raise EXQEvaluationException.Create('pxp:INTERNAL', 'Nothing to pop');

  targetCount := histories[targetHistoryCount];
  historyCount := targetHistoryCount;
  if targetCount < varCount then begin
    for i := targetCount to varCount - 1 do varstorage[i].value := nil;
    varCount := targetCount;
  end;
  if varCount < length(varstorage) shr 2 then SetLength(varstorage, varCount); //shrink
end;

procedure TXQVariableChangeLog.stringifyNodes;
var
  i: SizeInt;
  pv: PIXQValue;
  xhasNodes: Boolean;
  list: TXQVList;
begin
  for i:=0 to count-1 do
    case varstorage[i].value.kind of
      pvkNode: varstorage[i].value := xqvalue(varstorage[i].value.toString);
      pvkSequence: begin
        xhasNodes := false;
        for pv in varstorage[i].value.GetEnumeratorPtrUnsafe do
          if pv^.kind = pvkNode then begin
            xhasNodes := true;
            break;
          end;
        if xhasNodes then begin
          list := txqvlist.create(varstorage[i].value.getSequenceCount);
          for pv in varstorage[i].value.GetEnumeratorPtrUnsafe do
            if pv^.kind = pvkNode then list.add(xqvalue(pv^.toString))
            else list.add(pv^);
          varstorage[i].value := TXQValueSequence.create(list);
        end;
      end;
    end
end;

function TXQVariableChangeLog.hasNodes: boolean;
var i: SizeInt;
begin
  for i:=varCount - 1 downto 0 do
    if varstorage[i].value.hasNodes then exit(true);
  result := false;
end;

procedure TXQVariableChangeLog.removeLast;
begin
  dec(varCount);
  varstorage[varCount].value := nil;
end;

procedure TXQVariableChangeLog.pushOpenArray(const vs: array of IXQValue);
var
  i: SizeInt;
begin
  pushAll;
  for i := 0 to high(vs) do
    add('_'+IntToStr(i+1), vs[i]);
end;

procedure TXQVariableChangeLog.pushOpenArray(const untypedStrings: array of string);
var
  i: SizeInt;
begin
  pushAll;
  for i := 0 to high(untypedStrings) do
    add('_'+IntToStr(i+1), TXQValueString.create(baseSchema.untypedAtomic, untypedStrings[i]));
end;

procedure TXQVariableChangeLog.reserve(newcap: SizeInt);
begin
  if newcap <= length(varstorage) then exit;
  if newcap > 2 * length(varstorage) then SetLength(varstorage, newcap)
  else SetLength(varstorage, 2* length(varstorage));
end;

constructor TXQVariableChangeLog.create();
begin
end;

destructor TXQVariableChangeLog.destroy();
begin
  inherited destroy();
end;

function TXQVariableChangeLog.debugTextRepresentation: string;
var i:sizeint;
begin
  if count = 0 then exit('');
  result:=getName(0)+'='+get(0).debugAsStringWithTypeAnnotation();
  for i:=1 to count - 1 do
    result+=LineEnding+getName(i)+'='+get(i).debugAsStringWithTypeAnnotation(){%H-};
end;

function TXQVariableChangeLog.clone: TXQVariableChangeLog;
begin
  result := TXQVariableChangeLog.create();
  result.assign(self);
end;

procedure TXQVariableChangeLog.assign(from: TXQVariableChangeLog);
begin
  varstorage := from.varstorage;
  setlength(varstorage, length(varstorage)); //detach
  histories := from.histories;
  setlength(histories, length(histories)); //detach
  varCount := from.varCount;
  historyCount := from.historyCount;
end;

function TXQVariableChangeLog.finalValues: TXQVariableChangeLog;
var final: boolean;
    i,j: SizeInt;
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
end;

function TXQVariableChangeLog.collected: TXQVariableChangeLog;
var i: SizeInt;
  oldid, j: SizeInt;
  templist: TFPList; //list of sequences, so we do not need to cast ixqvalue -> txqvaluesequence
begin
  result := TXQVariableChangeLog.create();
  templist := nil;
  for i := 0 to count - 1 do begin
    oldid := result.indexOf(varstorage[i].name, varstorage[i].namespaceURL);
    if oldid < 0 then begin
      result.add(varstorage[i].name, varstorage[i].value, varstorage[i].namespaceURL);
      if templist <> nil then templist.Add(nil);
    end else begin
      if templist = nil then begin
        templist := TFPList.Create;
        templist.Capacity := result.count;
        for j := 0 to result.count - 1 do templist.Add(nil);
      end;
      if templist[oldid] = nil then begin
        templist[oldid] := TXQValueSequence.create(result.varstorage[oldid].value);
        result.varstorage[oldid].value := TXQValueSequence(templist[oldid]);
      end;
      TXQValueSequence(templist[oldid]).add(varstorage[i].value);
    end;
  end;
  templist.free;
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
var
  i: SizeInt;
begin
  reserve(varCount + other.count);
  for i:=0 to other.count-1 do
    varstorage[varCount+i] := other.varstorage[i];
  inc(varCount, other.count);
  other.clear;
end;

procedure deleteVar(self: TXQVariableChangeLog; i: SizeInt);
begin
  with self do begin
    varstorage[i].namespaceURL := ''; //free mem
    varstorage[i].name := '';
    varstorage[i].value := nil;
    move(varstorage[i+1],varstorage[i], (varCount - 1 - i) * sizeof(varstorage[i]) );
    FillChar(varstorage[varCount - 1], sizeof(varstorage[i]), 0);
    dec(varCount);
  end;
end;

function TXQVariableChangeLog.condensed: TXQVariableChangeLog;

var
  temp: IXQValue;
  i, last: SizeInt;
begin
  result := TXQVariableChangeLog.create();
  result.shared:=true;
  SetLength(result.varstorage, varCount);
  for i:=0 to varCount - 1 do begin
    if varstorage[i].propertyChange then begin
      last := result.indexOf(varstorage[i].name, varstorage[i].namespaceURL);
      if last >= 0 then begin
        deleteVar(result, last);

        result.varstorage[result.varCount] := varstorage[i];
        //result.varstorage[result.varCount].name := varstorage[i].name; ???
        result.varstorage[result.varCount].propertyChange:=false;
        result.varCount += 1;
        continue;
      end;

      if not parentLog.hasVariable(varstorage[i].name, temp, varstorage[i].namespaceURL) then
        raise EXQEvaluationException.Create('pxp:OBJECT', 'Assignment to property of object '+varstorage[i].name+', but no variable of that name exists');
      if temp.kind <> pvkObject then
        raise EXQEvaluationException.Create('pxp:OBJECT', 'Assignment to property of object '+varstorage[i].name+', but '+varstorage[i].name+'='+temp.toXQuery()+' is not an object ');
    end;
    result.varstorage[result.varCount] := varstorage[i];
    result.varCount += 1;
  end;
end;

function TXQVariableChangeLog.hasVariable(const variable: TXQTermVariable): boolean;
begin
  result := hasVariable(variable.value, variable.namespace);
end;

function TXQVariableChangeLog.hasVariable(const variable: string; const namespaceURL: string = ''): boolean;
begin
  if indexOf(variable, namespaceURL) >= 0 then result := true
  else if parentLog <> nil then result := parentLog.hasVariable(variable, namespaceURL)
  else result := false;
end;

function TXQVariableChangeLog.hasVariable(const variable: string; out value: IXQValue; const namespaceURL: string): boolean;
var
  i: SizeInt;
begin
  i := indexOf(variable, namespaceURL);
  if i = -1 then
    if parentLog <> nil then exit(parentLog.hasVariable(variable, value, namespaceURL))
    else exit(false);
  value := varstorage[i].value;
  result := true;
end;

function TXQVariableChangeLog.hasVariable(const variable: TXQTermVariable; out value: IXQValue): boolean; //**< Returns if a variable with name @param(variable) exists, and if it does, returns its value in @param(value). @param(value) might be nil, and it returns the value directly, not a cloned value. Supports objects. (notice that the pointer points to an TXQValue, not an IXQValue, since latter could cause problems with uninitialized values. If you pass a pointer to a IXQValue, it will compile, but randomly crash)
begin
  result := hasVariable(variable.value, value, variable.namespace);
end;


procedure TXQEvaluationStack.push(const value: ixqvalue);
begin
  if fcount >= fcapacity then
    reserve(fcount + 1);
  PPointer(fbuffer)[fcount] := value;
  value._AddRef;
  fcount += 1;
end;

procedure TXQEvaluationStack.pop();
begin
  assert(fcount > 0);
  fbuffer[fcount-1] := nil;
  dec(fcount);
end;

procedure TXQEvaluationStack.popTo(newCount: SizeInt);
var
  i: SizeInt;
begin
  assert(newCount <= fcount);
  for i := newCount to fcount - 1 do
    fbuffer[i]._Release;
  FillChar(fbuffer[newCount], sizeof(fbuffer[newCount]) * (fcount - newCount), 0);
  fcount := newCount;
end;

function TXQEvaluationStack.top(i: SizeInt): IXQValue;
begin
  assert((i >= 0) and (count - i - 1 >= 0));
  result := fbuffer[count - i - 1];
end;

function TXQEvaluationStack.topptr(i: SizeInt): PIXQValue;
begin
  result := @fbuffer[count - i - 1];
end;


procedure TXQEvaluationStack.push(const name: TXQTermVariable; const v: ixqvalue);
begin
  {$ifdef TRACK_STACK_VARIABLE_NAMES}
  if fcount >= length(debugNames) then
   setlength(debugNames, length(debugNames) + 128);
  debugNames[fcount] := name.value;
  {$endif}
  push(v);
end;

procedure TXQEvaluationStack.failTop(const name: TXQTermVariable; i: SizeInt);
begin
  raise EXQEvaluationException.create('pxp:INTERNAL','Stack name mismatch: '+debugNames[count - i - 1]+' <> '+name.value);
end;
function TXQEvaluationStack.top(const name: TXQTermVariable; i: SizeInt): IXQValue;
begin
  result := top(i);
  {$ifdef TRACK_STACK_VARIABLE_NAMES}
  if not strEqual(debugNames[count - i - 1], name.value)
  //   and (debugNames[count - i - 1][1] <> '0') {and (v.namespace = XMLNamespaceURL_MyExtensionOperators}
     then failTop(name, i);

  {$endif}
end;


procedure TXQBatchFunctionCall.init(const outerContext: TXQEvaluationContext; const f: ixqvalue; const def: IXQValue);
var
  i: SizeInt;
begin
  func := f.toValue as TXQValueFunction;
  stack := outerContext.temporaryVariables;
  stacksize := stack.Count;
  tempcontext := func.context;
  tempcontext.temporaryVariables := outerContext.temporaryVariables;
  tempcontext.globallyDeclaredVariables := outerContext.globallyDeclaredVariables;
  if def = nil then begin
    for i := 0 to high(func.parameters) do
      stack.push(f);
  end else for i := 0 to high(func.parameters) do stack.push(def);
  func.contextOverrideParameterNames(tempcontext, length(func.parameters));
end;

procedure TXQBatchFunctionCall.done;
begin
  stack.popTo(stackSize);
end;

function TXQBatchFunctionCall.call(): IXQValue; inline;
begin
  result := func.evaluateInContext(tempcontext, nil);
end;

function TXQBatchFunctionCall.call1(const v: IXQValue): IXQValue;
begin
  stack.topptr(0)^ := v;
  result := func.evaluateInContext(tempcontext, nil);
end;

function TXQBatchFunctionCall.call2(const v, w: IXQValue): IXQValue;
begin
  stack.topptr(1)^ := v;
  stack.topptr(0)^ := w;
  result := func.evaluateInContext(tempcontext, nil);
end;


procedure TXQMapDuplicateResolveHelper.setFromString(const s: string);
begin
  case s of
    'reject': self := xqmdrReject;
    'use-any', 'use-first': self := xqmdrUseFirst;
    'use-last': self := xqmdrUseLast;
    'combine': self := xqmdrCombine;
    else raise EXQEvaluationException.create('FOJS0005', 'Invalid duplicates option: ' + s, nil, nil);
  end;
end;

procedure TXQJsonParser.init;
begin
  context:=nil;
  options:=[];
  duplicateResolve:=xqmdrUseFirst;
  escapeFunction:=nil;
end;

procedure TXQJsonParser.setConfigFromMap(const map: IXQValue);
  procedure raiseInvalidParam(code: string = 'XPTY0004');
  begin
    raise EXQEvaluationException.create(code, 'Invalid parameter', nil, map);
  end;

  procedure checkType(const v: TXQValue; t: TXSType);
  begin
    if (v.getSequenceCount <> 1) or (not v.instanceOf(t) and not (v.instanceOf(baseSchema.untypedAtomic)) and (v.kind <> pvkNode)) then
      raiseInvalidParam;
  end;
var vo: TXQValue;
begin
  if map.hasProperty('liberal', @vo) then begin
    checkType(vo, baseSchema.boolean);
    if vo.toBoolean then begin
      include(options, jpoLiberal);
      include(options, jpoAllowTrailingComma);
    end else Exclude(options, jpoLiberal);
  end;
  if map.hasProperty('duplicates', @vo) then begin
    checkType(vo, baseSchema.string_);
    duplicateResolve.setFromString(vo.toString);
    if duplicateResolve = xqmdrCombine then raiseInvalidParam;
  end;
  if map.hasProperty('escape', @vo) then begin
    checkType(vo, baseSchema.boolean);
    if vo.toBoolean then include(options, jpoEscapeCharacters)
    else exclude(options, jpoEscapeCharacters)
  end;
  if map.hasProperty('fallback', @vo) then begin
    checkType(vo, baseSchema.function_);
    if (jpoEscapeCharacters in options) then
      raise EXQEvaluationException.create('FOJS0005', 'fallback can''t be used with escape', nil, map);
    escapeFunction := vo as TXQValueFunction;
    if length(escapeFunction.parameters) <> 1 then raiseInvalidParam();
  end;
  if jpoJSONiq in options then begin
    if map.hasProperty('jsoniq-multiple-top-level-items', @vo) then begin
      if (vo.getSequenceCount < 1) or vo.instanceOf(baseJSONiqSchema.jsNull) then include(options, jpoAllowMultipleTopLevelItems)
      else  if (vo.getSequenceCount = 1) and (vo.instanceOf(baseSchema.boolean) ) then begin
        if vo.toBoolean then include(options, jpoAllowMultipleTopLevelItems)
        else exclude(options, jpoAllowMultipleTopLevelItems)
      end else
        raiseInvalidParam('jerr:JNTY0020');
    end;
  end;
  //setOption('trailing-comma', pjoAllowTrailingComma);
end;

function TXQJsonParser.parse(argc: SizeInt; args: PIXQValue): IXQValue;
begin
  if (argc = 2) then
    setConfigFromMap(args[1]);
  if args[0].isUndefined then exit(xqvalue);
  result := parse(args[0].toString);
end;




function TXQJsonParser.parse(const data: string): IXQValue;
var
  scanner: TJSONScanner;
  jsoniqMode: boolean;

  procedure raiseError(message: string = 'error'; code: string = '');
  var token: string;
  begin
    Str(scanner.CurToken, token);
    if code = '' then begin
      if jsoniqMode then code := 'jerr:JNDY0021'
      else if isInvalidUTF8Guess(scanner.CurLine, length(scanner.CurLine)) then code := 'FOUT1200'
      else code := 'FOJS0001';
    end;
    raise EXQEvaluationException.create(code, message+' at ' + scanner.CurTokenString +' (' + token +') in '+scanner.CurLine);
  end;

  function nextToken: TJSONToken;
  begin
    try
      while scanner.FetchToken = tkWhitespace do ;
    except
      on e: EScannerError do
        raiseError('Failed to parse JSON: '+ e.Message);
    end;
    result := scanner.CurToken;
  end;


  function parseNumber: Ixqvalue;
  var
    temp64: Int64;
    tempFloat: Extended;
    tempd: BigDecimal;
    tempchar: Char;
  begin
    if jsoniqMode then begin
      if TryStrToInt64(scanner.CurTokenString, temp64) then exit(xqvalue(temp64));
      if TryStrToBigDecimal(scanner.CurTokenString, @tempd) then
        if striContains(scanner.CurTokenString, 'E')  then exit(baseSchema.double.createValue(tempd))
        else if strContains(scanner.CurTokenString, '.')  then exit(baseSchema.decimal.createValue(tempd))
        else exit(baseSchema.integer.createValue(tempd));
      if TryStrToFloat(scanner.CurTokenString, tempFloat, XQFormats) then
        if striContains(scanner.CurTokenString, 'E') then exit(baseSchema.double.createValue(tempFloat))
        else exit(TXQValueDecimal.create(tempFloat));
    end else begin
      if TryStrToFloat(scanner.CurTokenString, tempFloat, XQFormats) then
        exit(baseSchema.double.createValue(tempFloat))
    end;
    if (jpoLiberal in options) and ((scanner.CurTokenString = '+') or (scanner.CurTokenString = '-')) then begin
      tempchar := scanner.CurTokenString[1];
      if scanner.FetchToken = tkIdentifier then
        case scanner.CurTokenString of
          'INF', 'Inf', 'inf', 'INFINITY', 'Infinity', 'infinity':
            case tempchar of
              '+': exit(xqvalue(xqfloat.PositiveInfinity)); //this actually never happens, because + is parsed as tkWhitespace rather than tkNumber
              '-': exit(xqvalue(xqfloat.NegativeInfinity));
            end;
        end;
    end;
    raiseError('Invalid number');
    result := nil; //hide warning
  end;


var containerStack: array of TXQValue;
    containerCount: SizeInt;
    parsingPhase: ( jppArrayExpectValue, jppArrayExpectComma,
                   jppObjectExpectKey, jppObjectExpectComma, jppObjectExpectValue,
                   jppRoot);
    currentContainer: TXQValue;

  procedure setCurrentContainer; inline;
  begin
    if containerCount > 0 then
      currentContainer := containerStack[containerCount-1]
     else
      currentContainer := nil;
  end;

  procedure popContainer;
  begin
    if containerCount = 0 then raiseError('Unexpected closing parenthesis');
    containerCount -= 1;
    setCurrentContainer;
    if currentContainer = nil then parsingPhase := jppRoot
    else if currentContainer.ClassType = TXQValueJSONArray then parsingPhase := jppArrayExpectComma
    else parsingPhase := jppObjectExpectComma;
  end;

  function pushContainer(container: txqvalue): txqvalue;
  begin
    if length(containerStacK) = containerCount then
      if containerCount < 16 then setlength(containerstack, 16)
      else setlength(containerstack, containercount * 2);
    containerStack[containerCount] := container;
    inc(containerCount);
    result := container;
  end;


  var objectKey: string;
      tempSeq: TXQValueSequence;

  procedure pushValue(const v: ixqvalue);
  var
    hadResult: Boolean;
  begin
    case parsingPhase of
      jppArrayExpectValue: begin
        if currentContainer.ClassType <> TXQValueJSONArray then raiseError();
        TXQValueJSONArray(currentContainer).seq.addInArray(v);
        parsingPhase := jppArrayExpectComma;
      end;
      jppObjectExpectValue: begin
        if currentContainer.ClassType <> TXQValueStringMap then raiseError();
        parsingPhase := jppObjectExpectComma;
        if (duplicateResolve <> xqmdrUseLast) and TXQValueStringMap(currentContainer).hasProperty(objectKey, nil) then
          if duplicateResolve = xqmdrUseFirst then exit
          else raiseError('Duplicate key', 'FOJS0003');
        TXQValueStringMap(currentContainer).setMutable(objectKey, v);
      end;
      jppRoot: begin
        hadResult := (result <> nil);
        xqvalueSeqConstruct(result, tempSeq, v);
        if hadResult and not (jpoAllowMultipleTopLevelItems in options) then
          raiseError();
      end;
      jppArrayExpectComma,
      jppObjectExpectKey,
      jppObjectExpectComma: raiseError('value not allowed');
    end;
  end;

  function escapeAll(s: string): string;
  var sb: TStrBuilder;
    cp: Integer;
  begin
    result := '';
    sb.init(@result, length(s));
    for cp in strIterator(s) do
      case cp of
        $20..ord('\')-1, ord('\')+1..$D7FF, $E000..$FFFD, $10000..$10FFFF: sb.appendCodePoint(cp);
        ord('\'): sb.append('\\');
        9: sb.append('\t');
        10: sb.append('\n');
        13: sb.append('\r');
        8: sb.append('\b');
        else if cp <= $FFFF then begin
          sb.append('\u');
          sb.append(IntToHex(cp, 4));
        end;
      end;
    sb.final;
  end;
  function escapeInvalid(s: string): string;
  var sb: TStrBuilder;
    cp: Integer;
  begin
    result := '';
    sb.init(@result, length(s));
    for cp in strIterator(s) do
      if isValidXMLCharacter(cp) then sb.appendCodePoint(cp)
      else sb.appendCodePoint($FFFD);
    sb.final;
  end;
  function escapeInvalidCallback(s: string): string;
  var sb: TStrBuilder;
      f: TXQBatchFunctionCall;
      cp: Integer;

  begin
    result := '';
    f.init(context^, escapeFunction);
    sb.init(@result, length(s));
    for cp in strIterator(s) do
      if isValidXMLCharacter(cp) then sb.appendCodePoint(cp)
      else begin
        f.stack.topptr(0)^ := xqvalue('\u'+IntToHex(cp, 4));
        sb.append(f.call().toString);
      end;
    sb.final;
  end;

  function readString: string;
  const SUSPICIOUS_CHARS_NORMAL = [#0..#8, #11, #12, #14..#$1F, #$80..#$C1, #$ED, #$EF, #$F4..#$FF];
  const SUSPICIOUS_CHARS_ESCAPE = [#0..#$1F, '\', #$80..#$C1, #$ED, #$EF, #$F4..#$FF];
  var
    c: Char;
    suspicious: Boolean;
  begin
    result := scanner.CurTokenString;
    suspicious := false;
    if jpoEscapeCharacters in options then begin
     for c in result do begin
       suspicious := c in SUSPICIOUS_CHARS_ESCAPE;
       if suspicious then break;
     end;
    end else begin
      for c in result do begin
        suspicious := c in SUSPICIOUS_CHARS_NORMAL;
        if suspicious then break;
      end;
    end;
    if not suspicious then exit;
    if jpoEscapeCharacters in options then result := escapeAll(result)
    else if (context = nil) or (escapeFunction = nil)  then result := escapeInvalid(result)
    else result := escapeInvalidCallback(result)
  end;

  procedure readObjectKey;
  begin
    //if not (scanner.CurToken in [tkString, tkIdentifier]) then raiseError('Expected property name');
    objectKey := readString;
    if nextToken <> tkColon then raiseError('Expected : between property name and value');
    parsingPhase := jppObjectExpectValue;
  end;


var scannerOptions:  {$if FPC_FULLVERSION > 30000}TJSONOptions{$else}boolean{$endif};
  i: SizeInt;
begin
  result := nil;
  currentContainer := nil;
  containerStack := nil;
  containerCount := 0;
  parsingPhase := jppRoot;
  jsoniqMode := jpoJSONiq in options;
  tempSeq := nil;
  {$if FPC_FULLVERSION > 30000}
  scannerOptions := [joUTF8];
  if not (jpoLiberal in options) then include(scannerOptions, joStrict);
  if jpoAllowTrailingComma in options then include(scannerOptions, joIgnoreTrailingComma);
  {$else}
  scannerOptions := true;
  {$endif}
  scanner := TJSONScanner.Create(data, scannerOptions);
  try

    nextToken;
    while true do begin
      case scanner.CurToken of
        tkEOF: break;
        tkString:
          if parsingPhase = jppObjectExpectKey then readObjectKey
          else pushValue(xqvalue(readString));
        tkNumber: pushValue(parseNumber);
        tkFalse: pushValue(xqvalueFalse);
        tkTrue: pushValue(xqvalueTrue);
        tkNull: if jsoniqMode then pushValue(TXQValueJSONNull.create) else pushValue(xqvalue());

        tkComma: case parsingPhase of
          jppObjectExpectComma: parsingPhase := jppObjectExpectKey;
          jppArrayExpectComma: parsingPhase := jppArrayExpectValue;
          else raiseError();
        end;
        //tkColon,                 // ':'
        tkCurlyBraceOpen: begin
          pushValue(pushContainer(TXQValueStringMap.create()));
          setCurrentContainer;
          case nextToken of
            tkCurlyBraceClose: popContainer;
            else begin
              parsingPhase := jppObjectExpectKey;
              continue;
            end;
          end;
        end;
        tkCurlyBraceClose:
          if (parsingPhase = jppObjectExpectComma) or ((parsingPhase = jppObjectExpectKey) and (jpoAllowTrailingComma in options)) then popContainer
          else raiseError();
        tkSquaredBraceOpen: begin
          pushValue(pushContainer(TXQValueJSONArray.create()));
          setCurrentContainer;
          case nextToken of
            tkSquaredBraceClose: popContainer;
            else begin
              parsingPhase := jppArrayExpectValue;
              continue;
            end;
          end;
        end;
        tkSquaredBraceClose:
          if (parsingPhase = jppArrayExpectComma) or ((parsingPhase = jppArrayExpectValue) and (jpoAllowTrailingComma in options)) then popContainer
          else raiseError();
        tkIdentifier:
          if jpoLiberal in options then begin
            if parsingPhase = jppObjectExpectKey then readObjectKey
            else case scanner.CurTokenString of
              'INF', 'Inf', 'inf', 'INFINITY', 'Infinity', 'infinity': pushValue(xqvalue(xqfloat.PositiveInfinity));
              'NAN', 'NaN', 'nan': pushValue(xqvalue(xqfloat.NaN));
              else raiseError();
            end;
          end else raiseError();
        //tkComment:
        //tkUnknown
        else raiseError();
      end;
      nextToken;
    end;

    if containerCount > 0 then begin
      for i := containerCount - 1 downto 0 do //this prevents a crash on deeply nested invalid inputs in nst's JSONTestSuite. Still could not be used for valid inputs as the recursive free crashes later.
        if containerStack[i].ClassType = TXQValueJSONArray then TXQValueJSONArray(containerStack[i]).seq.clear
        else TXQValueStringMap(containerStack[i]).mapdata.clear;
      raiseError('unclosed');
    end;

    if result = nil then begin
     if not (jpoLiberal in options) then raiseError('No data');
      result := xqvalue;
    end;
  finally
    //for i := 0 to containerCount - 1 do containerStack[i]._Release;
    scanner.free;
  end;
end;

class function TXQJsonParser.parse(const data: string; someOptions: TOptions): IXQValue;
var parser: TXQJsonParser;
begin
  parser.init;
  parser.options:=someOptions;
  result := parser.parse(data);
end;






function xqgetTypeInfo(wrapper: Ixqvalue): TXQTermSequenceType;
begin
  if not (wrapper is TXQValueFunction) or not objInheritsFrom((wrapper as TXQValueFunction).body, TXQTermSequenceType) then
    raise EXQEvaluationException.Create('XPTY0004', 'Expected type, got: '+wrapper.toString);
  result := TXQTermSequenceType((wrapper as TXQValueFunction).body);
end;

function xqvalueCastAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := xqgetTypeInfo(tb).castAs(ta, cxt);
end;

function xqvalueCastableAs(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  ignore(cxt);
  result := xqvalue(xqgetTypeInfo(tb).castableAs(ta, cxt.staticContext));
end;

function xqvalueInstanceOf(const cxt: TXQEvaluationContext; const ta, tb: IXQValue): IXQValue;
begin
  result := xqvalue(xqgetTypeInfo(tb).instanceOf(ta, cxt));
end;

function xqvalueOrPlaceholder(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(a); ignore(b);
  raise EXQEvaluationException.create('PXP:ORPL','Placeholder called');
  result := nil;
end;

function xqvalueAndPlaceholder(const cxt: TXQEvaluationContext; const a, b: IXQValue): IXQValue;
begin
  ignore(cxt); ignore(a); ignore(b);
  raise EXQEvaluationException.create('PXP:ANDPL', 'Placeholder called');
  result := nil;
end;





procedure raisePXPInternalError;
begin
  raise EXQEvaluationException.create('pxp:INTERNAL', 'Internal error');
end;
procedure TXQEvaluationContext.raiseXPDY0002ContextItemAbsent;
begin
  raise EXQEvaluationException.create('XPDY0002', 'Context item (.) is not set');
end;

procedure TXQEvaluationContext.setContextItem(const v: IXQValue);
begin
  SeqValue := v;
  SeqIndex := 1;
  SeqLength := 1;
end;

procedure TXQEvaluationContext.setContextItem(const v: IXQValue; index, length: SizeInt);
begin
  SeqValue := v;
  SeqIndex := index;
  SeqLength:= length;
end;

procedure TXQEvaluationContext.getContextItem(out v: IXQValue; out index, length: SizeInt);
begin
  v := SeqValue;
  index := SeqIndex;
  length := SeqLength;
end;


procedure raiseFORG0001InvalidConversion(const v: IXQValue; const convTo: string);
begin
  raise EXQEvaluationException.create('FORG0001', 'Invalid conversion from '+v.toXQuery()+' to type '+convTo);
end;
procedure raiseXPTY0004TypeError(const v: IXQValue; const convTo: string);
begin
  raise EXQEvaluationException.create('XPTY0004', 'Invalid conversion from '+v.toXQuery()+' to type '+convTo);
end;
procedure raiseXPTY0004TypeError(const v: IXQValue; const typ: TXSType);
begin
  raiseXPTY0004TypeError(v, typ.name);
end;
procedure raiseFOTY0013TypeError(const v: IXQValue);
begin
  raise EXQEvaluationException.create('FOTY0013', 'Invalid conversion from '+v.toXQuery()+' to atomic value');
end;
procedure raiseXQEvaluationError(const code, s: string; const data: IXQValue);
var
  t: String;
begin
  t := s;
  if data <> nil then t += ' ; got: ' + data.toXQuery();
  raise EXQEvaluationException.create(code, t);
end;
procedure raiseInternalError(const s: string); noreturn;
begin
  raise EXQEvaluationException.create('pxp:INTERNAL', 'Internal error: ' + s);
end;
procedure raiseInternalError(const code: integer);
begin
  raiseInternalError(IntToStr(code));
end;

{ TXQStaticContext }

function TXQStaticContext.getNodeCollation: TXQCollation;
begin
  if FNodeCollation = nil then result :=collation
  else result := FNodeCollation;
end;

function TXQStaticContext.findModule(const namespaceURL: string): TXQuery;
var
  i: SizeInt;
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
  exit(module.staticContext); //the main module can contain function in any namespace
end;

function TXQStaticContext.findFunction(const anamespace, alocalname: string; const argcount: integer): TXQValueFunction;
var
  i: SizeInt;
  f: TXQValueFunction;
begin
  for i := high(functions) downto 0 do begin
    f :=  functions[i];
    if (alocalname = f.name)
       and (length(f.parameters) = argcount)
       and (f.namespaceURL = anamespace) then
      exit(f);
  end;
  exit(nil);
end;

function TXQStaticContext.findVariableDeclaration(v: TXQTermVariable): TXQTermDefineVariable;
begin
  result := findVariableDeclaration(v.namespace, v.value);
end;

function TXQStaticContext.findVariableDeclaration(const namespace, varname: string): TXQTermDefineVariable;
var
  i, mi, delta: SizeInt;
  m: TXQTermModule;
  w: TXQTermVariable;
begin
  if associatedModules = nil then exit(nil);
  if isLibraryModule then delta := 0
  else delta := 1;
  for mi := 0 to associatedModules.Count - 1 do begin
    m := TXQTermModule(TXQuery(associatedModules[mi]).FTerm);
    if m = nil then continue; //otherwise it crashes with variables accessing other variables in the same imported module
    for i := 0 to high(m.children) - delta do begin
      if not objInheritsFrom(m.children[i], TXQTermDefineVariable) then continue;
      w := TXQTermDefineVariable(m.children[i]).getVariable;
      if (w.namespace = namespace) and (w.value = varname) then
        exit(TXQTermDefineVariable(m.children[i]));
    end;
  end;
  exit(nil);
end;

function TXQStaticContext.isLibraryModule: boolean;
begin
  result := moduleNamespace <> nil;
end;

procedure TXQStaticContext.assign(sc: TXQStaticContext);
var
  i: SizeInt;
  Result: TXQStaticContext;
begin
  Result := self;
  with sc do begin
    result.sender := sender;
    result.stripboundaryspace := stripboundaryspace;
    result.moduleContextItemDeclarations := moduleContextItemDeclarations;
    if namespaces <> nil then result.namespaces := namespaces.clone;
    result.functions := functions;
    if length(result.functions) > 0 then begin
      setlength(result.functions, length(result.functions));
      for i:= 0 to high(result.functions) do begin
        result.functions[i] := result.functions[i].directClone as TXQValueFunction;
        result.functions[i].context.staticContext := result;
      end;
    end;
    result.associatedModules := associatedModules;
    if associatedModules <> nil then begin
      result.associatedModules := TFPList.Create;
      result.associatedModules.Assign(associatedModules);
    end;
    result.importedmodules := importedmodules;
    if result.importedModules <> nil then begin
      result.importedModules := TXQMapStringObject.Create;
      for i:=0 to importedModules.count - 1 do result.importedModules.AddObject(importedModules[i], importedModules.Objects[i]);
    end;
    result.importedSchemas := importedSchemas;
    if result.importedSchemas <> nil then result.importedSchemas := importedSchemas.clone;
    result.collation := collation;
    result.fnodeCollation := fnodeCollation;
    result.emptyorderspec := emptyorderspec;
    result.defaultFunctionNamespace := defaultFunctionNamespace; defaultFunctionNamespace._AddRefIfNonNil;
    result.defaultElementTypeNamespace := defaultElementTypeNamespace; defaultElementTypeNamespace._AddRefIfNonNil;
    result.defaultTypeNamespace := defaultTypeNamespace; defaultTypeNamespace._AddRefIfNonNil;
    result.baseuri := baseuri;
    result.constructionpreserve := constructionpreserve;
    result.ordering := ordering;
    result.copynamespacepreserve := copynamespacepreserve;
    result.copynamespaceinherit := copynamespaceinherit;
    result.stringEncoding:=stringEncoding;
    result.strictTypeChecking:=strictTypeChecking;
    Result.useLocalNamespaces:=useLocalNamespaces;
    result.jsonPXPExtensions := jsonPXPExtensions;
    result.AllowJSONiqOperations := AllowJSONiqOperations;
    result.model := model;
    if decimalNumberFormats <> nil then begin
      result.decimalNumberFormats:= TFPList.Create;
      for i := 0 to decimalNumberFormats.Count - 1 do
        result.decimalNumberFormats.Add(TXQDecimalFormat(decimalNumberFormats[i]).clone);
    end;
    if result.temporaryNodes <> nil then result.temporaryNodes.release;
    result.temporaryNodes := temporaryNodes;
    if temporaryNodes <> nil then temporaryNodes.addRef;
    result.serializationOptions:=serializationOptions;
  end;
end;

function TXQStaticContext.clone(): TXQStaticContext;
begin
  result := TXQStaticContext.Create;
  result.assign(self);
end;

destructor TXQStaticContext.Destroy;
var
  i: SizeInt;
begin
  FreeAndNil(associatedModules);
  FreeAndNil(importedModules);
  for i := 0 to high(functions) do
    functions[i].free;
  namespaces.free;
  importedSchemas.Free;
  if decimalNumberFormats<> nil then
    for i := 0 to decimalNumberFormats.Count - 1 do
      tobject(decimalNumberFormats[i]).free;
  decimalNumberFormats.free;
  if temporaryNodes <> nil then temporaryNodes.release;
  defaultElementTypeNamespace._ReleaseIfNonNil;
  defaultFunctionNamespace._ReleaseIfNonNil;
  defaultTypeNamespace._ReleaseIfNonNil;
  inherited Destroy;
end;

function TXQStaticContext.findSchema(const namespace: string): TXSSchema;
var
  i: SizeInt;
begin
  if namespace = baseSchema.url then exit(baseSchema);
  if importedSchemas <> nil then
    for i := 0 to importedSchemas.Count - 1 do
      if importedSchemas.items[i].getURL = namespace then
        exit(baseSchema); //todo: return real schema
  result := nil;
end;

function TXQStaticContext.findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): TNamespace;
var
  i: SizeInt;
begin
  result := nil;
  if (moduleNamespace <> nil) and (moduleNamespace.getPrefix = nsprefix) then
    exit(moduleNamespace.getSelf);
  if (defaultElementTypeNamespace <> nil) and (defaultNamespaceKind in [xqdnkAny, xqdnkElementType, xqdnkType]) and (defaultElementTypeNamespace.getPrefix = nsprefix) then
    exit(defaultElementTypeNamespace);
  if (defaultTypeNamespace <> nil) and (defaultNamespaceKind in [xqdnkAny, xqdnkType]) and (defaultTypeNamespace.getPrefix = nsprefix) then
    exit(defaultTypeNamespace);
  if (namespaces <> nil) and ((nsprefix <> '') or (defaultNamespaceKind in [xqdnkAny, xqdnkElementType])) and (namespaces.hasNamespacePrefix(nsprefix, result)) then
    exit;
  if importedModules <> nil then begin
    i := importedModules.IndexOf(nsprefix);
    if i >= 0 then exit(TXQuery(importedModules.Objects[i]).staticContext.moduleNamespace.getSelf);
  end;
  if (importedSchemas <> nil)  and (defaultNamespaceKind in [xqdnkAny, xqdnkElementType,  xqdnkType]) and (importedSchemas.hasNamespacePrefix(nsprefix, result)) then
    exit;
  if (defaultFunctionNamespace <> nil) and (defaultNamespaceKind in [xqdnkAny, xqdnkFunction]) and (defaultFunctionNamespace.getPrefix = nsprefix) then
    exit(defaultFunctionNamespace);
  result := sender.findNamespace(nsprefix);
  if (result = nil) and (nsprefix = '') then
    case defaultNamespaceKind of
      xqdnkFunction : result := defaultFunctionNamespace;
      xqdnkElementType:
        result := defaultElementTypeNamespace;
      xqdnkType:
        if defaultElementTypeNamespace <> nil then result := defaultElementTypeNamespace
        else result := defaultTypeNamespace;
      {xqdnkUnknown, xqdnkAny :} else result := nil;
    end;
end;

function TXQStaticContext.findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
var
  temp: TNamespace;
begin
  temp := findNamespace(nsprefix, defaultNamespaceKind);
  if temp <> nil then result := temp.getURL
  else result := '';
end;

function TXQStaticContext.findNamespaceURLMandatory(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
var
  temp: TNamespace;
begin
  temp := findNamespace(nsprefix, defaultNamespaceKind);
  if temp <> nil then result := temp.getURL
  else result := '';
  if (nsprefix <> '') and (result = '') then
    raise EXQParsingException.create('XPST0081', 'Unknown namespace prefix: ' + nsprefix);
end;

procedure TXQStaticContext.splitRawQName(out namespace: INamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;

procedure TXQStaticContext.splitRawQName(out namespace: TNamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;

procedure TXQStaticContext.splitRawQName(out namespace: string; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
var
  temp: TNamespace;
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
  contenttype := defaultInternet.getLastContentType;
end;

function TXQStaticContext.retrieveFromFile(url: string; out contenttype: string; failErrCode: string): string;
begin
  //contenttype is always '' unless it is an url and not a file
  if strContains(url, '://') then result := retrieveFromURI(url, contenttype, failErrCode)
  else if strBeginsWith(url, '/') then result := retrieveFromURI('file://' + url, contenttype, failErrCode)
  else result := retrieveFromURI('file://./' + url, contenttype, failErrCode);
end;

function TXQStaticContext.needTemporaryNodes: TXQTempTreeNodes;
begin
  if temporaryNodes = nil then begin
    temporaryNodes := TXQTempTreeNodes.create();
    temporaryNodes.addRef;
  end;
  result := temporaryNodes;
end;

function TXQStaticContext.ImplicitTimezoneInMinutes: integer;
begin
  {$PUSH}{$Q-}{$R-}
  if sender <> nil then result := sender.ImplicitTimezoneInMinutes
  else result := high(integer);
  {$POP}
end;

function TXQStaticContext.CurrentDateTime: TDateTime;
begin
  {$PUSH}{$Q-}{$R-}
  if sender <> nil then result := sender.CurrentDateTime
  else result := nan;
  {$POP}
end;




function TXQStaticContext.compareCommon(a, b: TXQValue; overrideCollation: TXQCollation; castUnknownToString: boolean): TXQCompareResult;
var ak, bk: TXQValueKind;
  function compareCommonFloat(): TXQCompareResult;
  var
    cmpClass: TXSType;
    ad, bd: xqfloat;
  begin
    if ((ak = pvkFloat) and TXQValueFloat(a).value.IsNan()) or ((bk = pvkFloat) and TXQValueFloat(b).value.IsNan()) then
      exit(xqcrNaN);
    cmpClass := TXSType.commonDecimalType(a, b);
    ad := a.toFloatChecked(Self);
    bd := b.toFloatChecked(self);
    if cmpClass.derivedFrom(baseSchema.Double) then begin
      result := xqfloat(ad).compare((bd));
    end else if cmpClass.derivedFrom(baseSchema.Float) then begin
      result := xqfloat(single(ad)).compare(Single(bd));
    end else
      result := xqcrIncomparable; //should not happen, but hides warning
  end;

  function compareCommonAsStrings(): TXQCompareResult;
  begin
    if overrideCollation = nil then begin
      overrideCollation := collation;
      if overrideCollation = nil then exit(TXQCompareResult.fromIntegerResult(compareStr(a.toString, b.toString)));
    end;
    result := overrideCollation.compare(a.toString, b.toString)
  end;

  function vtodecimalstr(k: TXQValueKind; v: txqvalue): string; //faster implementation of cast, mixed with vtod
  begin
    case k of
      pvkInt64, pvkBigDecimal, //this case is handled in vtod
      pvkFloat: begin
        raisePXPInternalError(); //float trigger floating point conversion
        result := ''; //hide warning
      end;
      pvkString, pvkNode: begin
        if (k <> pvkNode) and strictTypeChecking and not v.instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(v, 'decimal');
        result := trim(v.toString);
      end;
      else begin
        if strictTypeChecking then raiseXPTY0004TypeError(v, 'decimal');
        result := trim(v.toString);
      end
    end;
  end;
  function compareAsBigDecimals: TXQCompareResult;
  var bda, bdb: BigDecimal;
      temps: string;
  begin
    case ak of
      pvkInt64, pvkBigDecimal: bda := a.toDecimal; //always ok
      else begin
        temps := vtodecimalstr(ak, a);
        if not tryStrToBigDecimal(temps, @bda) then
          if strContains(temps, 'N') then exit(compareCommonFloat())
          else raiseFORG0001InvalidConversion(a, 'decimal');
      end;
    end;
    case bk of
      pvkInt64, pvkBigDecimal: bdb := b.toDecimal; //always ok
      else begin
        temps := vtodecimalstr(bk, b);
        if not tryStrToBigDecimal(temps, @bdb) then
          if strContains(temps, 'N') then exit(compareCommonFloat())
          else raiseFORG0001InvalidConversion(b, 'decimal');
      end;
    end;
    result := TXQCompareResult.fromIntegerResult(compareBigDecimals(bda, bdb));
  end;
  function compareAsBigDecimals(const i: TXQValue; const s: string): TXQCompareResult;
  var
    temp: BigDecimal;
  begin
    if not tryStrToBigDecimal(s, @temp) then
      if strContains(s, 'N') then exit(compareCommonFloat())
      else raiseFORG0001InvalidConversion(xqvalue(s), 'decimal');
    result := TXQCompareResult.fromIntegerResult(compareBigDecimals(i.toInt64, temp));
  end;

  function compareAsPossibleInt64: TXQCompareResult; //assumption ak != bk; ak, bk != pvkBigDecimal
  const MaxInt64LogDec = 18;
  var
    s: String;
    temp: int64;
  begin
    if ak = pvkInt64 then begin
      s := vtodecimalstr(bk, b);
      if (length(s) >= MaxInt64LogDec) or not TryStrToInt64(s, temp) then exit(compareAsBigDecimals(a, s));
      result := TXQCompareResult.compare( TXQValueInt64(a).value, temp);
    end else if bk = pvkInt64 then begin
      s := vtodecimalstr(ak, a);
      if length(s) >= MaxInt64LogDec then exit(compareAsBigDecimals(b, s).inverted());
        if (length(s) >= MaxInt64LogDec) or not TryStrToInt64(s, temp) then
          if strContains(s, 'N') then exit(compareCommonFloat())
          else exit(compareAsBigDecimals(b, s).inverted());
      result := TXQCompareResult.compare(temp,  TXQValueInt64(b).value);
    end else begin raisePXPInternalError; result := xqcrIncomparable; end;
  end;

  function compareMultiple(enuma, enumb: TXQValueEnumeratorPtrUnsafe): TXQCompareResult;
  begin
    if not enuma.MoveNext then exit(xqcrEmptySequence);
    if not enumb.MoveNext then exit(xqcrEmptySequence);
    result := compareCommon(enuma.Current.toValue, enumb.Current.toValue, overrideCollation, castUnknownToString);
    if enuma.MoveNext then raiseXPTY0004TypeError(a, 'singleton');
    if enumb.MoveNext then raiseXPTY0004TypeError(b, 'singleton');
  end;


  function compareCommonEqualKind(): TXQCompareResult;
  var
    adate: PXQValueDateTimeData;
    bdate: PXQValueDateTimeData;
  begin

    case ak of
      pvkBoolean:
        result := TXQCompareResult.compare(TXQValueBoolean(a).bool, TXQValueBoolean(b).bool);
      pvkInt64:
        result := TXQCompareResult.compare(TXQValueInt64(a).value, TXQValueInt64(b).value);
      pvkBigDecimal: result := compareAsBigDecimals();
      pvkFloat: result := compareCommonFloat();
      pvkDateTime: begin
        if (a.typeAnnotation.derivedFrom(baseSchema.duration)) <> (b.typeAnnotation.derivedFrom(baseSchema.duration)) then exit(xqcrIncomparable);
        adate := @TXQValueDateTime(a).value;
        bdate := @TXQValueDateTime(b).value;
        if a.typeAnnotation.derivedFrom(baseSchema.duration) and b.typeAnnotation.derivedFrom(baseSchema.duration) then begin
          result := TXQCompareResult.compare(adate^.toMonths(), bdate^.toMonths());
          if result <> xqcrEqual then exit;
          result := TXQCompareResult.compare(adate^.toDayTime(), bdate^.toDayTime());
        end else //result := compareValue(TXQValueDateTime(a).toDateTime, TXQValueDateTime(b).toDateTime);
          result := TXQValueDateTime.compare(TXQValueDateTime(a),TXQValueDateTime(b),ImplicitTimezoneInMinutes);
      end;
      pvkQName: begin
        result := xqcrIncomparable;
        if (a.instanceOf(baseSchema.QName) and b.instanceOf(baseSchema.QName))
           or (a.instanceOf(baseSchema.NOTATION) and b.instanceOf(baseSchema.NOTATION)) then
          if (TXQValueQName(a).url = TXQValueQName(b).url) and (TXQValueQName(a).local = TXQValueQName(b).local) then //ignore prefix
            result := xqcrEqual;
      end;
      pvkNull: result := xqcrEqual;
      pvkUndefined: result := xqcrEmptySequence;
      pvkNode, pvkString: result := compareCommonAsStrings;
      pvkBinary: result := TXQValueBinary.compare(a, b);
      pvkSequence: result := compareMultiple(a.GetEnumeratorPtrUnsafe, b.GetEnumeratorPtrUnsafe);
      pvkFunction: raise EXQEvaluationException.create('FOTY0013', 'Functions are incomparable');
      pvkArray: result := compareMultiple(a.GetEnumeratorMembersPtrUnsafe, b.GetEnumeratorMembersPtrUnsafe);
      else begin result := xqcrIncomparable; raisePXPInternalError; end;
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

  function cast(from: txqvalue; tok: TXQValueKind; tov: TXQValue): txqvalue;
  var temp: IXQvalue;
  begin
    if tok <> pvkQName then temp := (tov.typeAnnotation as TXSSimpleType).primitive.createValue(from)
    else temp := ((tov.typeAnnotation as TXSSimpleType).primitive as TXSQNameType).cast(from, Self);
    temp._AddRef;
    result := temp.toValue;;
  end;
begin
  ak := a.kind; bk := b.kind;
  if ak = bk then exit(compareCommonEqualKind());
  case ak of
    pvkUndefined: exit(xqcrEmptySequence);
    pvkSequence: exit(compareMultiple(a.GetEnumeratorPtrUnsafe, b.GetEnumeratorPtrUnsafe));
    pvkString, pvkNode: if bk in [pvkString, pvkNode] then exit(compareCommonEqualKind());
    pvkFunction: raise EXQEvaluationException.create('FOTY0013', 'Functions are incomparable');
    pvkArray: exit(compareMultiple(a.GetEnumeratorMembersPtrUnsafe, b.GetEnumeratorPtrUnsafe));
    else;
  end;
  case bk of
    pvkUndefined: exit(xqcrEmptySequence);
    pvkSequence: exit(compareMultiple(a.GetEnumeratorPtrUnsafe, b.GetEnumeratorPtrUnsafe));
    pvkNull: exit(xqcrGreaterThan);
    pvkFunction: raise EXQEvaluationException.create('FOTY0013', 'Functions are incomparable');
    pvkArray: exit(compareMultiple(a.GetEnumeratorPtrUnsafe, b.GetEnumeratorMembersPtrUnsafe));
    else;
  end;
  if ak = pvkNull then exit(xqcrLessThan); //can only test this after checkin b's sequence state
  if castUnknownToString and ( (ak in [pvkString, pvkNode]) or (bk in [pvkString, pvkNode]) ) then
    exit(compareCommonAsStrings());
  if strictTypeChecking then begin
    if (ak = pvkString) and not a.instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(a, b.typeAnnotation);
    if (bk = pvkString) and not b.instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(b, a.typeAnnotation);
  end;
  if (ak = pvkFloat) or (bk = pvkFloat) then
    exit(compareCommonFloat());
  if (ak in [pvkInt64, pvkBigDecimal]) or (bk in [pvkInt64, pvkBigDecimal]) then begin
    //if not (ak in [pvkInt64, pvkBigDecimal]) and strictTypeChecking and (baseSchema.decimal.tryCreateValue(a) <> xsceNoError) then raiseXPTY0004TypeError(a, 'decimal');
    //if not (bk in [pvkInt64, pvkBigDecimal]) and strictTypeChecking and (baseSchema.decimal.tryCreateValue(b) <> xsceNoError) then raiseXPTY0004TypeError(b, 'decimal');
    if (ak = pvkBigDecimal) or (bk = pvkBigDecimal) then exit(compareAsBigDecimals)
    else exit(compareAsPossibleInt64);
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
    b := cast(b,ak,a);
    bk := ak;
    try
      exit(compareCommonEqualKind());
    finally
      b._Release;
    end;
  end;
  if not (bk in [pvkString, pvkNode]) then begin
    a := cast(a,bk,b);
    ak := bk;
    try
      exit(compareCommonEqualKind());
    finally
      a._Release;
    end;
  end;
  exit(compareCommonAsStrings());
end;

function TXQStaticContext.compareAtomic(a, b: TXQValue; overrideCollation: TXQCollation): TXQCompareResult;
begin
  result := compareCommon(a, b, overrideCollation, true);
end;

function TXQStaticContext.compareAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): TXQCompareResult;
begin
  result := compareAtomic(a.toValue, b.toValue, overrideCollation);
end;

procedure TXQStaticContext.compareAtomic(const a, b: IXQValue; out result: IXQValue; accept1: TXQCompareResult; accept2: TXQCompareResult);
var
 compres: TXQCompareResult;
begin
  compres := compareAtomic(a,b, nil);
  if (compres = accept1) or (compres = accept2) then result := xqvalueTrue
  else if compres = xqcrEmptySequence then result := xqvalue
  else result := xqvalueFalse;
end;

function TXQStaticContext.equalAtomic(a, b: TXQValue; overrideCollation: TXQCollation): boolean;
begin
  result:=compareAtomic(a,b,overrideCollation)=xqcrEqual;
end;

function TXQStaticContext.equalAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): boolean;
begin
  result := equalAtomic(a.toValue,b.toValue,overrideCollation);
end;

function TXQStaticContext.compareGeneral(const a, b: TXQValueEnumeratorPtrUnsafe; overrideCollation: TXQCollation; accept1: TXQCompareResult; accept2: TXQCompareResult): boolean;
var enum1, enum2: TXQValueEnumeratorPtrUnsafe;
  compres: TXQCompareResult;
  va, vb: TXQValue;
begin
  enum1 := a;
  while enum1.MoveNext do begin
    va := enum1.Current^.toValue;
    case va.kind of
      pvkArray: begin
        result := compareGeneral( va.GetEnumeratorMembersPtrUnsafe, b, overrideCollation, accept1, accept2);
        if result then exit;
      end;
      pvkSequence: begin
        result := compareGeneral( va.GetEnumeratorPtrUnsafe, b, overrideCollation, accept1, accept2);
        if result then exit;
      end;
      else
        enum2 := b;
        while enum2.MoveNext do begin
          vb := enum2.Current^.toValue;
          case vb.kind of
            pvkArray: result := compareGeneral(va.GetEnumeratorPtrUnsafe, vb.GetEnumeratorMembersPtrUnsafe, overrideCollation, accept1, accept2);
            pvkSequence: result := compareGeneral(va.GetEnumeratorPtrUnsafe, vb.GetEnumeratorPtrUnsafe, overrideCollation, accept1, accept2);
            else
              compres := compareCommon(va, vb, overrideCollation, false);
              result := (compres = accept1) or (compres = accept2);
          end;
          if result then exit;
        end;
    end
  end;
  result := false;
end;

function TXQStaticContext.compareGeneral(a, b: TXQValue; overrideCollation: TXQCollation; accept1: TXQCompareResult; accept2: TXQCompareResult): boolean;
begin
  result := compareGeneral(a.GetEnumeratorPtrUnsafe, b.GetEnumeratorPtrUnsafe, overrideCollation,  accept1, accept2);
end;

function TXQStaticContext.compareGeneral(a, b: IXQValue; overrideCollation: TXQCollation; accept1: TXQCompareResult; accept2: TXQCompareResult): boolean;
begin
  result := compareGeneral(a.GetEnumeratorPtrUnsafe, b.GetEnumeratorPtrUnsafe, overrideCollation,  accept1, accept2);
end;

procedure TXQStaticContext.compareGeneral(a, b: IXQValue; out result: IXQValue; accept1: TXQCompareResult; accept2: TXQCompareResult);
begin
  result := xqvalue(compareGeneral(a,b, nil, accept1,accept2));
end;

function TXQStaticContext.compareDeepAtomic(a, b: TXQValue; overrideCollation: TXQCollation): TXQCompareResult;
var
  ak: TXQValueKind;
  bk: TXQValueKind;
begin
  if not comparableTypes(a, b) then exit(xqcrIncomparable); //todo:   nodes?

  ak := a.kind; bk := b.kind;
  if ((ak = pvkFloat) and IsNan(TXQValueFloat(a).value)) then begin
    if ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)) then exit(xqcrEqual)
    else exit(xqcrLessThan); //move NaNs first for fn:sort
  end else if ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)) then exit(xqcrGreaterThan);
  result := compareAtomic(a,b,overrideCollation);
end;

function TXQStaticContext.compareDeepAtomic(const a, b: IXQValue; overrideCollation: TXQCollation): TXQCompareResult;
begin
  result := compareDeepAtomic(a.toValue, b.toValue, overrideCollation);
end;

function TXQStaticContext.equalDeepAtomic(a, b: TXQValue; overrideCollation: TXQCollation): boolean;
begin
  result := compareDeepAtomic(a,b,overrideCollation) = xqcrEqual;
{var
  ak: TXQValueKind;
  bk: TXQValueKind;
begin
  result:=false;
  ak := a.kind; bk := b.kind;
  if ((ak = pvkFloat) and IsNan(TXQValueFloat(a).value)) or ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)) then
    exit(acceptNAN and ((ak = pvkFloat) and IsNan(TXQValueFloat(a).value)) and ((bk = pvkFloat) and IsNan(TXQValueFloat(b).value)));}
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
     or ((ak = bk) and (ak in [pvkBoolean, pvkString, pvkBinary])) then
    exit(true);

  ac := a.typeAnnotation;
  if ac.derivedFrom(baseSchema.node) then ac := baseSchema.string_;
  bc := b.typeAnnotation;
  if bc.derivedFrom(baseSchema.node) then bc := baseSchema.string_;

  //check if a and b are in the same branch of the type hierarchy (which is the case if their common parent is not the root of the hierarchy)
  ct := TXSType.commonType(ac, bc);
  result := (ct <> nil) and (ct <> baseSchema.anyAtomicType) and (ct <> baseSchema.anySimpleType) and (ct <> baseSchema.anyType);;
end;



{ TXQEvaluationContext }

function TXQEvaluationContext.findNamespace(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): TNamespace;
begin
  if ((defaultNamespaceKind in [xqdnkAny, xqdnkElementType]) or ((nsprefix <> '') and (defaultNamespaceKind <> xqdnkFunction))) {<- dynamic namespaces are only created from node constructors}
     and (namespaces <> nil) and namespaces.hasNamespacePrefix(nsprefix, Result) then exit;
  result := staticContext.findNamespace(nsprefix, defaultNamespaceKind);
end;

function TXQEvaluationContext.findNamespaceURL(const nsprefix: string; const defaultNamespaceKind: TXQDefaultNamespaceKind): string;
var
  temp: TNamespace;
begin
  temp := findNamespace(nsprefix, defaultNamespaceKind);
  if temp = nil then begin
    if nsprefix <> '' then raise EXQEvaluationException.Create('XPST0008', 'Unknown namespace prefix: '+nsprefix);
    result := '';
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

procedure TXQEvaluationContext.splitRawQName(out namespace: TNamespace; var name: string; const defaultNamespaceKind: TXQDefaultNamespaceKind);
begin
  if system.pos(':', name) > 0 then namespace := findNamespace(strSplitGet(':', name), defaultNamespaceKind)
  else namespace := findNamespace('', defaultNamespaceKind);
end;


function TXQEvaluationContext.getRootHighest: TTreeNode;
begin
  if (SeqValue <> nil) then begin
    if (SeqValue.kind = pvkNode) then exit(SeqValue.toNode.getRootHighest())
    else raise EXQEvaluationException.Create('XPTY0004' {<- fn:root needs this}, 'Need context item that is a node to get root element');
  end;
  if ParentElement <> nil then exit(ParentElement.getRootHighest)
  else if RootElement <> nil then exit(RootElement)
  else raise EXQEvaluationException.Create('XPDY0002', 'no root element');
end;

function TXQEvaluationContext.hasGlobalVariable(const name: string; out value: IXQValue; const namespaceURL: string): boolean;
begin
  result := true;
  if globallyDeclaredVariables.hasVariable(name, value, namespaceURL) then exit;
  if (staticContext.sender <> nil) and (staticContext.sender.VariableChangelog.hasVariable(name, value, namespaceURL)) then exit;
  result := false;
end;

function TXQEvaluationContext.hasGlobalVariable(const v: TXQTermVariable; out value: IXQValue): boolean;
begin
  result := hasGlobalVariable(v.value, value, v.namespace);
end;

function TXQEvaluationContext.getGlobalVariable(const name: string; const namespaceURL: string): IXQValue;
var
  found: boolean;
begin
  found := hasGlobalVariable(name, result, namespaceURL);
  if not found then
    raise EXQEvaluationException.Create('XPST0008', 'Variable Q{'+namespaceURL+'}'+name+' not found');
  if result = nil then result := xqvalue();
end;

function TXQEvaluationContext.getGlobalVariable(const v: TXQTermVariable): IXQValue;
begin
  result := getGlobalVariable(v.value, v.namespace);
end;

function TXQEvaluationContext.contextNode(mustExists: boolean): TTreeNode;
begin
  if SeqValue <> nil then begin //tests pass without this branch. Why???
    result := SeqValue.toNode;
    if mustExists and (result = nil) then raise EXQEvaluationException.create('XPTY0004', 'Context item is not a node', nil, SeqValue);
    exit;
  end;
  if ParentElement <> nil then exit(ParentElement);
  if RootElement <> nil then exit(RootElement);
  if staticContext.sender = nil then raise EXQEvaluationException.Create('XPTY0020', 'Context sender is nil');
  if mustExists then raiseXPDY0002ContextItemAbsent;

  result := nil;
end;

function TXQEvaluationContext.parseDoc(const data, url, contenttype: string): TTreeDocument;
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
      if (tempnode <> nil) then parser := tempnode.getDocument().getCreator;
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

function TXQEvaluationContext.parseCachedDocFromUrl(const url: string; const errorCode: string): TTreeDocument;
var
  data, contenttype: String;
begin
  result := staticContext.needTemporaryNodes.documentCache[url];
  if result = nil then begin
    data := staticContext.retrieveFromURI(url, contenttype, errorCode);

    try
      result := parseDoc(data, url, contenttype);
    except
      on e: ETreeParseException do raise EXQEvaluationException.Create(errorCode, 'Failed to parse document: '+url + LineEnding+e.Message);
    end;

    staticContext.temporaryNodes.documentCache[url] := result;
  end;
  if result = nil then raise EXQEvaluationException.Create(errorCode, 'Failed to parse document: '+url);
end;

function TXQEvaluationContext.SeqValueAsString: string;
begin
  if SeqValue <> nil then result := SeqValue.toString
  else if ParentElement <> nil then result := TXQueryInternals.treeElementAsString(ParentElement)
  else begin raiseXPDY0002ContextItemAbsent; result := ''; end;
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

function TXQuery.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  stackSize: SizeInt;
  oldAllowJSONiqOperations: Boolean;
begin
  if fterm = nil then exit(xqvalue());
  stackSize := context.temporaryVariables.Count;
  oldAllowJSONiqOperations := globalCurrentQueryInfo.AllowJSONiqOperations;
  try
    if (context.staticContext <> nil) and (staticContext.importedModules = nil) and not objInheritsFrom(fterm, TXQTermModule) then begin
      //fast track. also we want to use the functions declared in the old static context
    end else
      context.staticContext:=staticContext; //we need to use our own static context, or our own functions are inaccessible
    if staticContext.temporaryNodes <> nil then begin
      staticContext.temporaryNodes.release;
      staticContext.temporaryNodes := nil;
    end;
    globalCurrentQueryInfo.AllowJSONiqOperations := staticContext.AllowJSONiqOperations;
    result := fterm.evaluate(context);
  finally
    globalCurrentQueryInfo.AllowJSONiqOperations := oldAllowJSONiqOperations;
    context.temporaryVariables.popTo(stackSize);
  end;
end;

function TXQuery.evaluateInConstContext(const context: TXQEvaluationContext): IXQValue;
var
  temp: TXQStaticContext;
begin
  temp := context.staticContext;
  //this can still destroy temporaryNodes. But it does not seem to matter?
  result := evaluate((PXQEvaluationContext(@context))^);
  (PXQEvaluationContext(@context))^.staticContext := temp;
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

function TXQuery.getStaticContext: TXQStaticContext;
begin
  result := staticContext;
end;

destructor TXQuery.Destroy;
begin
  fterm.Free; //delete term first, since it might contain other queries that have a reference to staticContext (let's hope they all share it => requirement nested queries must share);
  if not staticContextShared then FreeAndNil(staticContext)
  else if staticContext.associatedModules <> nil then
    staticContext.associatedModules.Remove(self);
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





























//This disables exception safe reference counting
//If an exception were to occur in any of the below functions, it will cause a memory leak. So this section has to be carefully watched




threadvar threadLocalCache: record
   runningEngines: SizeInt;
   commonValues: array[TXQValueKind] of record
      head: PPointer; //This is a TXQValue. But we override the first bytes with a pointer to the previously freed element, so it cannot be used as TXQValue
      vmt: pointer;   //The first overriden bytes of the TXQValue. They should be the same between all TXQValues of one kind
   end;
end;

class procedure TXQueryEngine.freeCommonCaches;
var k: TXQValueKind;
  v, w: PPointer;
begin
  with threadLocalCache do begin
    for k := low(commonValues) to high(commonValues) do begin
      v := commonValues[k].head;
      while v <> nil do begin
        w := v^;
        Freemem(pointer(v));
        v := w;
      end;
    end;
    FillChar(commonValues, sizeof(commonValues), 0);
  end;
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




{ TXQAbstractFunctionInfo }

class procedure TXQAbstractFunctionInfo.convertType(var result: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext; term: TXQTerm);
  procedure checkSequenceCount;
  begin
    case result.getSequenceCount of
    0:  if not typ.allowNone then term.raiseTypeError0004('Expected value, but got empty sequence.');
    1: ; //ok
    else if (not typ.allowMultiple) then
      term.raiseTypeError0004('Expected singleton', result);
    end;
  end;

  procedure convertFunctionTestSequence;
  var
    temp: IXQValue;
    seq: TXQVList;
    pv: PIXQValue;
  begin
    checkSequenceCount;
    temp := result;
    seq := TXQVList.create(temp.getSequenceCount);
    result := TXQValueSequence.create(seq);
    for pv in temp.GetEnumeratorPtrUnsafe do begin
      case pv.kind of
        pvkFunction:
          if context.staticContext.strictTypeChecking then seq.add(typ.functionCoercion(pv^))
          else seq.add(pv^);
        pvkArray, pvkObject:
          seq.add(typ.functionCoercion(pv^));
        else
          term.raiseTypeError0004('Expected function', pv^);
      end;
    end;
    xqvalueSeqSqueeze(result);
  end;

  function atomizeAndCastSingle(const w: IXQValue): IXQValue;
  var
    t: TXSType;
    errCode: String;
  begin
    //assert: w is not a sequence, node, function or array
    result := w;
    if typ.instanceOf(result, context) then exit;
    t := w.typeAnnotation;
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
    term.raiseEvaluationError(errCode, 'Invalid type for function. Expected '+typ.serialize+' got '+w.toXQuery());
  end;

  procedure convert;
  var
    p: PIXQValue;
    seqResult: TXQValueSequence;
  begin
    case typ.kind of
      tikAtomic: begin
        result := xqvalueAtomize(result);
        checkSequenceCount;
        if typ.instanceOf(result, context) then exit;
        if result.kind <> pvkSequence then result := atomizeAndCastSingle(result)
        else begin
          seqResult := TXQValueSequence.create(result.getSequenceCount);
          for p in result.GetEnumeratorPtrUnsafe do
            seqResult.seq.add(atomizeAndCastSingle(p^));
          result := seqResult;
        end;
      end;
      else begin
        checkSequenceCount;
        //if typ.kind <> tikAny then
        term.raiseTypeError0004('Expected '+typ.serialize, result);
      end;
    end;
  end;

begin
  if typ = nil then exit;
  if typ.kind <> tikFunctionTest then begin
    if typ.instanceOf(result, context) then exit;
    convert;
  end else convertFunctionTestSequence;
end;

class function TXQAbstractFunctionInfo.checkType(const v: IXQValue; const typ: TXQTermSequenceType; const context: TXQEvaluationContext): boolean;
var atomicCount: SizeInt;
  function checkAtomicRecursion(const w: IXQValue): boolean;
    function fullAtomization: boolean;
    begin
      result := checkAtomicRecursion(xqvalueAtomize(w));
    end;

  var
    px: PIXQValue;
    st: TXSType;
  begin
    case w.kind of
      pvkSequence: begin
        result := true;
        for px in w.GetEnumeratorPtrUnsafe do
          if not checkAtomicRecursion(px^) then
            exit(false);
      end;
      pvkArray: begin
        result := true;
        for px in w.GetEnumeratorMembersPtrUnsafe do
          if not checkAtomicRecursion(px^) then
            exit(false);
      end;
      pvkNode: begin
        //st := TXQValueNode.nodeTypeAnnotationForAtomic(w.toNode);
        result := fullAtomization;
        //checkNode(xqvalueAtomize(w));
      end;
      else
        inc(atomicCount);
        if typ.instanceOf(w, context) then exit(true);
        if (w.kind = pvkNull) and (typ.allowNone) then exit(true); //???
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
  end;

begin
  if typ = nil then exit(true);
  if typ.instanceOf(v, context) then exit(true);
  result := false;
  if typ.kind = tikAtomic then begin
    atomicCount := 0;
    result := checkAtomicRecursion(v);
    if not result then exit;
    if ((not typ.allowMultiple) and (atomicCount > 1)) then exit(false);
    if ((not typ.allowNone) and (atomicCount = 0)) then exit(false);
    result := true;
  end;
end;

function TXQAbstractFunctionInfo.getVersion(arity: integer): PXQFunctionParameterTypes;
var
  i: sizeint;
begin
  for i := 0 to high(versions) do
    if length(versions[i].types) = arity then exit(@versions[i]);
  result := nil;
end;

procedure TXQAbstractFunctionInfo.setVersionsShared(const v: array of TXQFunctionParameterTypes);
var
  i: sizeint;
begin
  SetLength(versions, length(v));
  for i := 0 to high(v) do
    versions[i] := v[i];
  sharedVersions := true;
end;

procedure TXQAbstractFunctionInfo.setVersionsShared(const v: array of TXQTermSequenceType);
begin
  sharedVersions := true;
  SetLength(versions, 1);
  minArgCount:=length(v) - 1;
  maxArgCount:=maxArgCount;
  setVersionsShared(0, v);
end;

procedure TXQAbstractFunctionInfo.setVersionsShared(const v, w: array of TXQTermSequenceType);
begin
  sharedVersions := true;
  SetLength(versions, 2);
  minArgCount:=length(v) - 1;
  maxArgCount:=maxArgCount;
  setVersionsShared(0, v);
  setVersionsShared(1, w);
end;

procedure TXQAbstractFunctionInfo.setVersionsShared(i: integer; const v: array of TXQTermSequenceType);
var
  arglen: Integer;
begin
  arglen := length(v) - 1;
  with versions[i] do begin
    SetLength(types, arglen);
    for i := 0 to high(types) do
      types[i] := v[i];
    returnType := v[high(v)];
  end;
  minArgCount:=min(minArgCount, arglen);
  maxArgCount:=max(maxArgCount, arglen);
end;

procedure TXQAbstractFunctionInfo.setVersionsShared(count: integer);
begin
  SetLength(versions, count);
  sharedVersions := true;
  minArgCount := high(minArgCount);
  maxArgCount := low(minArgCount);
end;


function TXQAbstractFunctionInfo.checkOrConvertTypes(values: PIXQValue; count: integer; const context: TXQEvaluationContext; term: TXQTerm): integer;
var
  countMatch, valueHigh: Integer;
  matches: Boolean;

  procedure makeErrorMessage;
  var errMessage: String;
    i: Integer;
  begin
    //print a nice error message
    //nested procedure, because using strings in the outer function would create a pointless exception frame
    if countMatch = -1 then
      term.raiseEvaluationError('XPST0017', 'Failed to find function (mismatched argument count)'); //todo: move to static evaluation
    errMessage:= '';
    for i := 0 to high(versions) do begin
      if length(versions[i].types) <> count then continue;
      if i = countMatch then continue;
      errMessage += LineEnding + '          or ' + versions[i].serialize;
    end;
    versions[countMatch].raiseErrorMessage(values, count, context, term, errMessage);
  end;

var i, j: integer;
begin
  if length(versions) = 0 then exit(-1);
  valueHigh := count - 1;
  countMatch := -1;
  for i:= 0 to high(versions) do begin
    if count <> length(versions[i].types) then continue;
    matches := true;
    for j := 0 to valueHigh do
      if (versions[i].types[j].kind <> tikFunctionTest) and not checkType(values[j], versions[i].types[j], context) then begin
       matches := false;
       break;
      end;
    if matches then begin
     for j := 0 to valueHigh do
       if versions[i].types[j].kind = tikFunctionTest then
         convertType(values[j], versions[i].types[j], context, term);
      result := i;
      exit;
    end;
    if countMatch = -1 then countMatch := i;
  end;

  makeErrorMessage;
  result := -1;
end;

destructor TXQAbstractFunctionInfo.Destroy;
var
  i, j: sizeint;
begin
  if not sharedVersions then
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
  i: sizeint;
begin
  if length(versions) = 0 then raise EXQParsingException.create('pxp:INIT', 'No argument count information');
  minArgCount:=high(minArgCount);
  maxArgCount:=0;
  for i := 0 to high(versions) do begin
    if length(versions[i].types) > maxArgCount then maxArgCount:=length(versions[i].types);
    if length(versions[i].types) < minArgCount then minArgCount:=length(versions[i].types);
  end;
end;




{$I xquery_terms.inc}
{$I xquery_types.inc}
{$I xquery_schemas.inc}

function sequenceFilterConditionSatisfied(const evaluatedCondition: IXQValue; const index: SizeInt): boolean;
begin
  case evaluatedCondition.kind of
    pvkUndefined: result := false;
    pvkInt64: result := evaluatedCondition.toInt64 = index;
    pvkBigDecimal: result := xqvalueDecimalEqualInt(evaluatedCondition, index);
    pvkFloat: result := (evaluatedCondition.toFloat = index);
    pvkDateTime: raise EXQEvaluationException.create('FORG0006', 'Sequence filter returned invalid value');
    else {pvkBoolean, pvkString,pvkSequence,pvkNode,pvkArray,pvkObject,pvkNull:} result := evaluatedCondition.toBooleanEffective;
  end;
end;












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
  FreeAndNil(LastDebugInfo);
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
  FLastQuery:=parseTerm(s, xqpmXPath3_0, sharedContext);
  result := FLastQuery;
end;

function TXQueryEngine.parseXQuery3(s: string; sharedContext: TXQStaticContext): IXQuery;
begin
  FLastQuery:=parseTerm(s, xqpmXQuery3_0, sharedContext);
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

function TXQueryEngine.evaluate(var context: TXQEvaluationContext): IXQValue;
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
  result.SeqValue := nil;
  FillChar(result, sizeof(result), 0);
  if staticContextOverride = nil then result.staticContext:=StaticContext
  else result.staticContext := staticContextOverride;
  result.SeqIndex:=-1;
  result.temporaryVariables := FDefaultVariableStack;
  result.globallyDeclaredVariables := FDefaultVariableHeap;
end;


constructor TXQueryEngine.create;
begin
  self.CurrentDateTime:=now;
  ImplicitTimezoneInMinutes:=high(Integer);
  ParsingOptions.AllowExtendedStrings:=true;
  ParsingOptions.AllowPropertyDotNotation:=xqpdnAllowUnambiguousDotNotation;
  ParsingOptions.AllowJSON:=AllowJSONDefaultInternal;
  ParsingOptions.AllowJSONLiterals:=true;
  ParsingOptions.LineEndingNormalization := xqlenXML1;
  ParsingOptions.AllowMutableVariables := true;
  VariableChangelog := TXQVariableChangeLog.create();
  //OnEvaluateVariable := @VariableChangelog.evaluateVariable;
  //OnDefineVariable:= @VariableChangelog.defineVariable;
  GlobalNamespaces := TNamespaceList.Create;
  StaticContext := TXQStaticContext.Create;
  StaticContext.model := xqpmXPath3_1;
  StaticContext.defaultFunctionNamespace := XMLNamespace_MyExtensionsMerged; XMLNamespace_MyExtensionsMerged._AddRef;
  StaticContext.sender := self;
  StaticContext.collation := internalDefaultCollation;
  StaticContext.emptyOrderSpec:=xqeoEmptyGreatest;
  StaticContext.defaultTypeNamespace := XMLNamespace_XMLSchema; XMLNamespace_XMLSchema._AddRef;
  StaticContext.copyNamespaceInherit:=true;
  StaticContext.copyNamespacePreserve:=true;
  StaticContext.stringEncoding:=CP_UTF8;
  StaticContext.useLocalNamespaces:=true;
  StaticContext.jsonPXPExtensions:=true;
  FDefaultVariableStack := TXQEvaluationStack.create();
  FDefaultVariableHeap := TXQVariableChangeLog.create();
  FModules := TXQueryModuleList.Create;
  FPendingModules := TXQueryModuleList.Create;
  inc(threadLocalCache.runningEngines);
  FCreationThread := GetThreadID;
  DefaultJSONParser.init;
  if AllowJSONDefaultInternal then begin
    DefaultJSONParser.options := [jpoAllowMultipleTopLevelItems, jpoLiberal, jpoAllowTrailingComma, jpoJSONiq];
    StaticContext.AllowJSONiqOperations := true;
    globalCurrentQueryInfo.AllowJSONiqOperations := true;
  end;
end;

procedure threadSafetyViolated;
begin
  raise EXQException.create('pxp:INTERNAL', 'A TXQueryEngine must be destroyed in the thread that created it');
end;

destructor TXQueryEngine.Destroy;
begin
  VariableChangelog.Free;
  VariableChangelogUndefined.free;
  FDefaultVariableHeap.Free;
  FDefaultVariableStack.Free;
  DefaultParser.Free;
  clear;
  GlobalNamespaces.free;
  FModules.Free;
  FPendingModules.Free;
  FParserVariableVisitor.free;
  StaticContext.Free;
  //We need to call freeCommonCaches on every thread.
  //We cannot know when the thread ends, so we do it with the last engine on the thread
  with threadLocalCache do begin
    dec(runningEngines);
    if runningEngines = 0 then begin
     freeCommonCaches;
     TNamespace.freeCache;
    end;
  end;
  if FCreationThread <> GetThreadID then //otherwise the runningEngines counter above would fail
    threadSafetyViolated; //if this wass inlined, the function crashes even if not executed, see #fpc31135
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

function TXQueryEngine.evaluateXQuery(expression: string; tree: TTreeNode): IXQValue;
begin
  result := evaluate(expression, xqpmXQuery3_1, tree);
end;

function TXQueryEngine.evaluateXQuery(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := evaluate(expression, xqpmXQuery3_1, contextItem);
end;

function TXQueryEngine.evaluateXPath(expression: string; tree: TTreeNode): IXQValue;
begin
  result := evaluate(expression, xqpmXPath3_1, tree);
end;

function TXQueryEngine.evaluateXPath(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := evaluate(expression, xqpmXPath2, contextItem);
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
  result := evaluate(expression, xqpmXPath3_0, tree);
end;

function TXQueryEngine.evaluateXPath3(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := evaluate(expression, xqpmXPath3_0, contextItem);
end;

function TXQueryEngine.evaluateXQuery3(expression: string; tree: TTreeNode): IXQValue;
begin
  result := evaluate(expression, xqpmXQuery3_0, tree);
end;

function TXQueryEngine.evaluateXQuery3(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := evaluate(expression, xqpmXQuery3_0, contextItem);
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
  result := staticallyEval(expression, xqpmXPath3_0, tree);
end;

class function TXQueryEngine.evaluateStaticXPath3(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := staticallyEval(expression, xqpmXPath3_0, contextItem);
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
  result := staticallyEval(expression, xqpmXQuery3_0, tree);
end;

class function TXQueryEngine.evaluateStaticXQuery3(expression: string; const contextItem: IXQValue): IXQValue;
begin
  result := staticallyEval(expression, xqpmXQuery3_0, contextItem);
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
begin
  FModules.Add(module);
end;

function TXQueryEngine.findModule(const namespaceURL: string): TXQuery;
var
  i: Integer;
  tempModule: TXQuery;
begin
  for i := 0 to FModules.Count - 1 do begin
    tempModule := FModules.Items[i] as TXQuery;
    if tempModule.staticContext.moduleNamespace.getURL = namespaceURL then
      exit(tempModule);
  end;
  for i := 0 to FPendingModules.Count - 1 do begin
    tempModule := FPendingModules.Items[i] as TXQuery;
    if tempModule.staticContext.moduleNamespace.getURL = namespaceURL then
      exit(tempModule);
  end;
  exit(nil);
end;

function TXQueryEngine.findModule(context: TXQStaticContext; const namespace: string; const at: array of string): TXQuery;
begin
  result := findModule(namespace);
  if result <> nil then exit;
  if assigned(OnImportModule) then onImportModule(self, staticContext, namespace, at);
  result := findModule(namespace);
end;

class procedure TXQueryEngine.registerCollation(const collation: TXQCollation);
begin
  internalRegisterCollation(collation);
end;

function TXQueryEngine.parseTerm(str: string; model: TXQParsingModel; context: TXQStaticContext): TXQuery;
var cxt: TXQAbstractParsingContext;
  staticContextShared: Boolean;
begin
  if str = '' then raise EXQParsingException.create('XPST0003', 'No input');
  staticContextShared := context <> nil;
  if context = nil then context := StaticContext.clone();
  cxt := createXQParsingContext as TXQAbstractParsingContext;
  cxt.encoding:=CP_UTF8;
  cxt.options := ParsingOptions;
  cxt.staticContext := context;
  context.model := model;
  cxt.parsingModel:=model;
  cxt.engine := self;
  try
    cxt.str := str;
    cxt.pos := @cxt.str[1];
    result := TXQuery.Create(cxt.staticContext);
    result.staticContextShared := staticContextShared;
    cxt.parseQuery(result);
    if LastDebugInfo <> nil then LastDebugInfo.free;
    LastDebugInfo := cxt.debugInfo;
    cxt.debugInfo := nil;
  finally
    cxt.free;
  end;
end;

function TXQueryEngine.parseXStringNullTerminated(str: string): TXQuery;
var cxt: TXQAbstractParsingContext;
    context: TXQStaticContext;
begin
  context := StaticContext.clone();
  if str = '' then begin
    result := TXQuery.Create(context, TXQTermSequence.Create);
    exit;
  end;
  cxt := createXQParsingContext as TXQAbstractParsingContext;
  cxt.encoding:=CP_UTF8;
  cxt.options :=ParsingOptions;
  cxt.options.AllowExtendedStrings:=true;
  cxt.staticContext := context;
  cxt.parsingModel:=xqpmXPath3_1;
  cxt.engine := self;
  try
    cxt.str := str;
    cxt.pos := @cxt.str[1];
    result := TXQuery.Create(cxt.staticContext);
    cxt.parseQueryXStringOnly(result);
  finally
    cxt.free;
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

  function newBinOp(left: TXQTerm; op: string; right: TXQTerm): TXQTermBinaryOp;
  begin
    result := TXQTermBinaryOp.Create(left, op, right);
  end;
  function newMap(left, right: TXQTerm): TXQTerm;
  begin
    result := TXQTermPath.create(newBinOp(left, '/', right));
  end;

  function newFunction(f: string; args: array of TXQTerm): TXQTerm;
  begin
    result := TXQTermNamedFunction.Create(XMLNamespaceURL_MyExtensionsMerged, f, args);
  end;

  function newOne: TXQTerm;
  begin
    result := TXQTermConstant.create(xqvalue(1));
  end;

  function newReadAttrib(name: string): TXQTerm;
  begin
    result := TXQTermNodeMatcher.Create('attribute', name);
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
      else begin raiseParsingError('Unknown operator: '+op+' before ' +attribValue); result := nil; end;
    end;

    skipSpace;
    expect(']');
  end;

  function pseudoOrNegation(elementName: string): TXQTerm;
    function isNth(index: TXQTerm; a, b: SizeInt): TXQTerm;
    begin
      if a = 0 then begin
        result := newBinOp(index, '=', TXQTermConstant.create(xqvalue(b)));
      end else begin
        result := newFunction('is-nth', [index, TXQTermConstant.create(xqvalue(a)), TXQTermConstant.Create(xqvalue(b))]);
      end;
    end;

    function allOfSameType(axis: string): TXQTerm;
    var axisTerm: TXQterm;
      tempv: TXQTermVariable;

    begin
      if elementName <> '*' then begin
        if axis <> '' then result := TXQTermNodeMatcher.Create(axis, elementName)
        else result := newMap(TXQTermNodeMatcher.Create(xqnmdParent), TXQTermNodeMatcher.Create('',elementName));
      end else begin
        if axis <> '' then axisTerm := TXQTermNodeMatcher.Create(axis, '*')
        else axisTerm := newMap(TXQTermNodeMatcher.Create(xqnmdParent), TXQTermNodeMatcher.Create('', '*'));
        result := TXQTermFlower.Create();
        TXQTermFlower(result).push(TXQTermFlowerFor.Create);
        with TXQTermFlowerFor(TXQTermFlower(result).children[high(TXQTermFlower(result).children)]) do begin
          loopvar := TXQTermVariable.create('__csstemp');
          //TXQTermFlower(result).vars[0].sequenceTyp := nil;
          expr := newFunction('name', []);
        end;
        tempv := TXQTermVariable.Create('__csstemp');
        tempv.index := 0;
        TXQTermFlower(result).push(TXQTermFilterSequence.create(
          axisTerm,
          newBinOp(tempv, '=', newFunction('name', []))
        ));
      end;
    end;

  var
    t: String;
    a, b: SizeInt;
    tn: String;
  begin
    expect(':');
    if pos^ = ':' then begin raiseParsingError(':: pseudo elements are not supported'); exit(nil); end;
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
                                                  newMap(
                                                     TXQTermNodeMatcher.Create('ancestor-or-self', '*'),
                                                     newReadAttrib('lang')
                                                  ),
                                                  newFunction('last', [])
                                               )
                           );
      end
      else if striequal(t, 'not') then begin
        case pos^ of
          #0: begin raiseParsingError('Unclosed not'); exit(nil); end;
          '#': result := hash;
          '.': result := classs;
          '[': result := attrib;
          ':': result := pseudoOrNegation(elementName);
          else result := TXQTermNodeMatcher.Create('self',namespacedIdent);
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
          else begin raiseParsingError('Expected nth'); exit(nil); end;
        end;



        if striequal(t, 'nth-child') then
          result := isNth(newFunction('count', [TXQTermNodeMatcher.Create('preceding-sibling', '*')]), a,b-1)
        else if striequal(t, 'nth-last-child') then
          result := isNth(newFunction('count', [TXQTermNodeMatcher.Create('following-sibling', '*')]), a,b-1)
        else if striequal(t, 'nth-of-type') then
          result := isNth(newFunction('count', allOfSameType('preceding-sibling')), a,b-1)
        else if striequal(t, 'nth-last-of-type') then
          result := isNth(newFunction('count', allOfSameType('following-sibling')), a,b-1)
        else begin raiseParsingError('impossible'); result := nil; end;
      end else begin raiseParsingError('Unknown function: '+t); result := nil; end;
      skipSpace;
      expect(')')
    end else begin
      if striEqual(t, 'root') then result := newBinOp(TXQTermNodeMatcher.Create(xqnmdParent),'is', TXQTermNodeMatcher.Create(xqnmdRoot))
      else if striEqual(t, 'first-child') then result := newFunction('empty', [TXQTermNodeMatcher.Create('preceding-sibling', '*')])
      else if striEqual(t, 'last-child') then result := newFunction('empty', [TXQTermNodeMatcher.Create('following-sibling', '*')])
      else if striEqual(t, 'first-of-type') then result := newFunction('empty', [allOfSameType('preceding-sibling')])
      else if striEqual(t, 'last-of-type') then  result := newFunction('empty', [allOfSameType('following-sibling')])
      else if striEqual(t, 'only-child') then    result := TXQTermIf.createLogicOperation(false, newFunction('empty', [TXQTermNodeMatcher.Create('preceding-sibling', '*')]), newFunction('empty', [TXQTermNodeMatcher.Create('following-sibling', '*')]))
      else if striEqual(t, 'only-of-type') then  result := newBinOp(newFunction('count', [allOfSameType('')]), '=', newOne)
      else if striEqual(t, 'empty') then         begin
        result := TXQTermNodeMatcher.Create();
        TXQTermNodeMatcher(Result).queryCommand.typ:=tneaDirectChild;
        TXQTermNodeMatcher(Result).queryCommand.matching:=MATCH_ALL_NODES - [qmAttribute];
        result := newFunction('not', [result]);
      end else if striEqual(t, 'link') then          result := TXQTermIf.createLogicOperation(false, TXQTermNodeMatcher.Create('self', 'a'), newFunction('exists', newReadAttrib('href')))
      else if striEqual(t, 'checked') then       result := newFunction('exists', [newReadAttrib('checked')])
      else if striEqual(t, 'enabled') or striEqual(t, 'disabled') or striEqual(t, 'visited') or striEqual(t, 'active') or striEqual(t, 'hover') or striEqual(t, 'focus') or striEqual(t, 'target')  then begin raiseParsingError('Unsupported pseudo class: '+t); exit(nil); end
      else begin raiseParsingError('Unknown pseudo class: '+t); exit(nil); end;
    end;
  end;

  function simple_selector_sequence: TXQTerm;
  var axis: string;
      adjacent: boolean;
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
      if not adjacent then newMatch := TXQTermNodeMatcher.Create(axis, elementName)
      else begin
        newMatch:=TXQTermFilterSequence.Create(
                                     TXQTermFilterSequence.Create(
                                                          TXQTermNodeMatcher.Create(axis,'*'),
                                                          newOne
                                     ),
                                     TXQTermNodeMatcher.Create('self',elementName)
                                     );
      end;

      if Result = nil then result := newMatch
      else result := newMap(result, newMatch);

      while pos^ in HACPN do begin
        case pos^ of
          '#': filter := hash();
          '.': filter := classs();
          '[': filter := attrib();
          ':': filter := pseudoOrNegation(elementName);
          else begin raiseParsingError('impossible'); filter := nil; end;
        end;
        if not objInheritsFrom(result, TXQTermFilterSequence)
           or objInheritsFrom(filter, TXQTermConstant)
           or objInheritsFrom(TXQTermFilterSequence(result).children[1], TXQTermConstant) then
          result := TXQTermFilterSequence.Create(result, filter)
         else
          TXQTermFilterSequence(result).children[1] := TXQTermIf.createLogicOperation(false, TXQTermFilterSequence(result).children[1], filter);
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


class procedure TXQueryEngine.filterSequence(const sequence: IXQValue; outList: TXQVList; const filter: TXQPathMatchingStepFilter; var context: TXQEvaluationContext);
var
 v: PIXQValue;
 i, backupA, backupB: SizeInt;
 value, backupItem: IXQValue;
 i64: Int64;
 oldParent: TTreeNode;
 filterTerm: TXQTerm;
begin
  outList.Count := 0;
  if (sequence = nil) or (sequence.getSequenceCount = 0) then exit;


  if [xqcdFocusItem, xqcdFocusPosition] * filter.dependencies = [] then begin
    if not (xqcdFocusLast in filter.dependencies) then value := filter.filter.evaluate(context)
    else begin
      try
        context.getContextItem(backupItem, backupA, backupB);
        context.setContextItem(sequence.get(1), 1, sequence.getSequenceCount);
        value := filter.filter.evaluate(context)
      finally
        context.setContextItem(backupItem, backupA, backupB);
      end;
    end;
    //optimization for a single number
    if value.kind in [pvkBigDecimal, pvkInt64, pvkFloat] then begin
      if ((value.kind = pvkFloat) and (frac(value.toFloat) <> 0)) or
         ((value.kind = pvkBigDecimal) and (not isInt64(value.toDecimal) )) then exit;
      i64 := value.toInt64;
      if (i64 >= 1) and (i64 <= sequence.getSequenceCount) then outList.add(sequence.get(i64));
    end else if value.toBooleanEffective then outList.add(sequence);
    exit;
  end; //end optimization

  try
    oldParent := context.ParentElement;
    context.getContextItem(backupItem, backupA, backupB);
    context.SeqLength:=sequence.getSequenceCount;
    context.ParentElement := nil;

    outList.reserve(context.SeqLength);
    i := 1;
    filterTerm := filter.filter;
    for v in sequence.GetEnumeratorPtrUnsafe do begin
      context.SeqValue:=v^;
      context.SeqIndex:=i;
      if sequenceFilterConditionSatisfied(filterTerm.evaluate(context), i) then
        outList.add(v^);
      i+=1;
    end;
  finally
    context.ParentElement := oldParent;
    context.setContextItem(backupItem, backupA, backupB);
  end;
end;

class procedure TXQueryEngine.filterSequence(var result: IXQValue; const filter: TXQPathMatchingStepFilters; var context: TXQEvaluationContext);
var i:SizeInt;
  list1, list2, temp: TXQVList;
  seq1, seq2: IXQValue;
begin
  case length(filter) of
    0: exit;
    1: ;
    else begin
      list2 := TXQVList.create();
      seq2 := TXQValueSequence.create(list2);
    end;
  end;
  list1 := TXQVList.create();
  seq1 := TXQValueSequence.create(list1);

  filterSequence(result, list1, filter[0], context);
  for i:=1 to high(filter) do begin
    xqswap(seq1, seq2{%H-}); //hide unitialized warning, since this only happens if length(filter) > 1
    temp := list1; list1 := {%H-}list2; list2 := temp;
    filterSequence(seq2, list1, filter[i], context);
  end;
  result := seq1;
  xqvalueSeqSqueeze(result);
end;


class function TXQueryEngine.expandSequence(const previous: IXQValue; const command: TXQPathMatchingStep; var context: TXQEvaluationContext; lastExpansion: boolean): IXQValue;
var oldnode,newnode: TTreeNode;
    newList: TXQVList;
    nodeCondition: TXQPathNodeCondition;

procedure jsoniqDescendants(const node: IXQValue; const searchedProperty: string);
var
  seq: TXQVList;
  obj: TXQValue;
  temp: TXQValue;
  tempvi: PIXQValue;
  i: SizeInt;
begin
  case node.kind of
    pvkArray: begin
      seq := (node as TXQValueJSONArray).seq;
      for i := 0 to seq.Count - 1 do
        jsoniqDescendants(seq[i], searchedProperty);
    end;
    pvkObject: begin
      obj := node.toValue;
      if searchedProperty <> '' then begin
        if obj.hasProperty(searchedProperty, @temp) then newList.add(temp);
      end else newList.add(obj.enumeratePropertyValues);

      for temp in obj.getEnumeratorPropertyValuesUnsafe do
        jsoniqDescendants(temp, searchedProperty);
    end;
    pvkSequence:
      for tempvi in node.GetEnumeratorPtrUnsafe do
        jsoniqDescendants(tempvi^, searchedProperty)
    else ;//we must ignore non structured item, otherwise it would be useless for any object (like a string<->string map) containing them
  end;
end;

var
    newList2: TXQVList;
    newListSeq, newListSeq2: IXQValue;

  function filter(const v: IXQValue): TXQVList;
  var
    i, lastfilter: SizeInt;
    temp: TXQVList;
  begin
    //filtering is done with a swap list approach
    //first v is filtered and the output written in list 1
    //then list 1 is filtered and written list 2
    //then it swaps and list 2 filtered is again written in list 1
    //etc.
    filterSequence(v, newList2, command.filters[0], context);
    if length(command.filters) > 1 then begin
      i := 1;
      lastfilter := high(command.filters);
      lastfilter := lastfilter - lastfilter and 1;
      while i <= lastfilter do begin
        //technically this does not swap. It can considered to be the loop of the swap approach unrolled, so that it behaves as if it was swapping
        filterSequence(newListSeq2, newList, command.filters[i], context);
        inc(i);
        filterSequence(newListSeq, newList2, command.filters[i], context);
        inc(i);
      end;
      if length(command.filters) and 1 = 0 then begin
        filterSequence(newListSeq2, newList, command.filters[high(command.filters)], context);
        xqswap(newListSeq, newListSeq2);
        temp := newList;
        newList := newList2;
        newList2 := temp;
      end;
    end;
    result := newList2;
  end;


var
  i: SizeInt;
  tempContext: TXQEvaluationContext;
  onlyNodes: boolean;
  n: PIXQValue;
  resultSeq: TXQValueSequence;

  tempNamespace: TNamespace;
  cachedNamespaceURL: string;
  tempKind: TXQValueKind;
  namespaceMatching: TXQNamespaceMode;
  tempList: TXQVList;
  pv: PIXQValue;
  attrib: TTreeNode;




begin
  if (previous = nil) or (previous.getSequenceCount = 0) then exit(previous);

  resultSeq:=TXQValueSequence.create(previous.getSequenceCount);
  try
    if command.typ = tneaFunctionSpecialCase then begin
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
    newListSeq := TXQValueSequence.create(newList);
    if length(command.filters) > 0 then begin
      newList2 := TXQVList.create();
      newListSeq2 := TXQValueSequence.create(newList2);
    end else
      tempList := newList; //no filter, no need to copy lists

    nodeCondition.equalFunction:=@context.staticContext.nodeCollation.equal;
    onlyNodes := false;
    for n in previous.GetEnumeratorPtrUnsafe do begin
      case command.typ of
        tneaFunctionSpecialCase: begin
          if not (n^.kind in [pvkNode, pvkObject, pvkArray]) then
            raise EXQEvaluationException.create('err:XPTY0019', 'The / operator can only be applied to xml/json nodes. Got: '+n^.toXQuery()); //continue;
          newList.clear;
          tempContext.SeqIndex += 1;
          tempContext.SeqValue := n^;
          if n^.kind = pvkNode then tempContext.ParentElement := tempContext.SeqValue.toNode;
          newList.add(command.specialCase.evaluate(tempContext));
        end;
        tneaAttribute: begin
          oldnode := n^.toNode;
          if oldnode = nil then raise EXQEvaluationException.create('err:XPTY0019', 'The / operator can only be applied to xml/json nodes. Got: '+n^.toXQuery());
          if (namespaceMatching = xqnmPrefix) and (command.namespaceURLOrPrefix <> '')  then begin
            cachedNamespaceURL := oldnode.getNamespaceURL(command.namespaceURLOrPrefix);
            if cachedNamespaceURL = '' then continue;
          end;

          for attrib in oldnode.getEnumeratorAttributes do begin
            if      (not (qmValue in command.matching) or ((attrib.hash = command.valueHash) and striEqual(attrib.value, command.value)))
                and ((namespaceMatching = xqnmNone) or ( attrib.getNamespaceURL() = cachedNamespaceURL))
                and not TTreeAttribute(attrib).isNamespaceNode
                then
                  newList.add(attrib);
          end;
        end else begin
          tempKind := n^.kind;
          case tempKind of
            pvkNode: begin
              assert(n^.toNode <> nil);
              oldnode := n^.toNode;
              nodeCondition.init(oldnode, command);
              if namespaceMatching = xqnmURL then begin
                nodeCondition.requiredNamespaceURL:=cachedNamespaceURL;
                Include(nodeCondition.options, xqpncCheckNamespace);
              end else exclude(nodeCondition.options, xqpncCheckNamespace);
              newnode := nodeCondition.getNextNode(nil);
              if newnode = nil then continue;
              newList.count := 0;
              while newnode <> nil do begin
                if (namespaceMatching <> xqnmPrefix)
                   or (newnode.getNamespacePrefix() = command.namespaceURLOrPrefix)                            //extension, use namespace bindings of current item, if it is not statically known
                   or (newnode.getNamespaceURL(command.namespaceURLOrPrefix) = newnode.getNamespaceURL()) then
                newList.add(newnode);
                newnode := nodeCondition.getNextNode(newnode);
              end;
            end;
            pvkObject, pvkArray: begin
              if not context.staticContext.jsonPXPExtensions then raise EXQEvaluationException.create('pxp:JSON', 'PXP Json extensions are disabled');
              if (command.namespaceURLOrPrefix <> '') or (command.requiredType <> nil)
                 or not (command.typ in [tneaDirectChild, tneaDirectChildImplicit, tneaDescendant, tneaSameNode])
                 or ((command.typ <> tneaSameNode) and (command.matching - [qmCheckNamespaceURL, qmCheckNamespacePrefix, qmCheckOnSingleChild, qmValue, qmAttribute] <> [qmElement]))
                 or ((command.typ = tneaSameNode) and ((command.matching <> [qmElement, qmText, qmComment, qmProcessingInstruction, qmAttribute, qmDocument]) or (command.value <> '') ))
                 then
                   raise EXQEvaluationException.create('pxp:JSON', 'too complex query for JSON object');
              newList.Count:=0;
              case command.typ of
                tneaDirectChild, tneaDirectChildImplicit: begin
                  if qmValue in command.matching then begin //read named property
                    //if tempKind <> pvkObject then raise EXQEvaluationException.create('err:XPTY0020', 'Only nodes (or objects if resp. json extension is active) can be used in path expressions');
                    if tempKind = pvkObject then newList.add(n^.getProperty(command.value))
                    else for pv in (n^ as TXQValueJSONArray).GetEnumeratorMembersPtrUnsafe do begin
                      if pv^.kind <> pvkObject then raise EXQEvaluationException.create('pxp:JSON', 'The / operator can only be applied to xml nodes, json objects and jsson arrays of only objects. Got array containing "'+pv^.toXQuery()+'"');
                      newList.add(pv^.getProperty(command.value));
                    end;
                  end else begin
                    //get all properties
                    if tempKind = pvkObject then newList.add(n^.enumeratePropertyValues())
                    else for pv in (n^ as TXQValueJSONArray).GetEnumeratorMembersPtrUnsafe do begin
                      if pv^.kind <> pvkObject then raise EXQEvaluationException.create('pxp:JSON', 'The / operator can only be applied to xml nodes, json objects and jsson arrays of only objects. Got array containing "'+pv^.toXQuery()+'"');
                      newList.add(pv^.enumeratePropertyValues());
                    end;
                  end;
                end;
                tneaDescendant:
                  jsoniqDescendants(n^.toValue, command.value);
                tneaSameNode:
                  newList.add(n^);
              end;

            end;
            else raise EXQEvaluationException.create('err:XPTY0019', 'The / operator can only be applied to xml/json nodes. Got: '+n^.toXQuery()); //continue;
          end;
        end;
      end;


      if length(command.filters) > 0 then begin
        case newList.Count of
          0: continue;
          1: tempList := filter(newList[0]);
          else tempList := filter(newListSeq);
        end;
      end;

      if tempList.Count = 0 then continue;
      if command.typ in [tneaAncestor,tneaSameOrAncestor,tneaPreceding,tneaPrecedingSibling] then
        tempList.revert; //revert the list, because it is now in ancestor order (needed for indices in filtering), but need to be returned in document order

      if resultSeq.seq.Count = 0 then onlyNodes := tempList[0].kind = pvkNode;
      for i := 0 to tempList.Count - 1 do
        if (tempList.fbuffer[i].kind = pvkNode) <> onlyNodes then
          raise EXQEvaluationException.Create(IfThen(lastExpansion, 'XPTY0018', 'XPTY0019'), 'Nodes and non-node values must not be mixed in step expressions');
      if onlyNodes then resultSeq.seq.addOrdered(tempList)
      else resultSeq.seq.add(tempList);
    end;

  except
    resultSeq.free;
    raise;
  end;

  result := resultSeq;
end;

class function TXQueryEngine.evaluateSingleStepQuery(const query: TXQPathMatchingStep;var context: TXQEvaluationContext; lastExpansion: boolean): IXQValue;
var
  n: TTreeNode;
begin
  case query.typ of
    tneaDocumentRoot: begin
      n := context.getRootHighest;
      if not objInheritsFrom(n, TTreeDocument) then raise EXQEvaluationException.create('XPDY0050', '/ can only select the root if it is a document node.');
      result := xqvalue(n);
      filterSequence(result, query.filters, context);
    end;
    tneaFunctionSpecialCase: begin
      result := query.specialCase.evaluate(context);
      filterSequence(result, query.filters, context);
    end
    else begin
      if (context.SeqValue <> nil) and (context.SeqValue.kind in [pvkNode, pvkObject]) then result := context.SeqValue
      else if context.ParentElement <> nil then result := xqvalue(context.ParentElement)
      else if context.SeqValue = nil then raise EXQEvaluationException.create('XPDY0002', 'Context item is undefined')
      else raise EXQEvaluationException.Create('XPTY0020', 'Expected node as context item, got: '+context.SeqValue.toXQuery());
      result := expandSequence(result,query, context, lastExpansion);
    end;
  end;
end;



class procedure TXQueryEngine.registerNativeModule(const module: TXQNativeModule);
begin
  nativeModules.AddObject(module.namespace.getURL, module);
end;

{$ifdef DUMPFUNCTIONS}
class procedure TXQueryEngine.dumpFunctions;
var
  i: SizeInt;
begin
  for i := 0 to nativeModules.count -  1 do
    (nativeModules.Objects[i] as TXQNativeModule).dumpFunctions;
end;
{$endif}

class function TXQueryEngine.getCollation(id: string; base: string; errCode: string): TXQCollation;
var
  oldid: string;
begin
  if strEndsWith(id, '/') then delete(id, length(id), 1);
  oldid := id;
  id := strResolveURI(id, base);
  if strBeginsWith(id, MY_NAMESPACE_PREFIX_URL) then
    id := strCopyFrom(id, length(MY_NAMESPACE_PREFIX_URL)+1);
  result := internalGetCollation(id);
  if result = nil then
    result := internalGetCollation(oldid);
  if result = nil then
    raise EXQEvaluationException.Create(errCode, 'Collation ' + id + ' is not defined');
end;

function TXQueryEngine.GetNativeModules: TStringList;
begin
  result := nativeModules;
end;

function TXQueryEngine.isAWeirdGlobalVariable(const namespace, local: string): boolean;
begin
  if VariableChangelog.hasVariable(local, namespace) then exit(true);
  if VariableChangelogUndefined = nil then exit(false);
  if VariableChangelogUndefined.hasVariable(local, namespace) then exit(true);
  exit(false);
end;

procedure TXQueryEngine.addAWeirdGlobalVariable(const namespace, local: string);
begin
  if isAWeirdGlobalVariable(namespace, local) then exit;
  if VariableChangelogUndefined = nil then
    VariableChangelogUndefined := TXQVariableChangeLog.create();
  if VariableChangelogUndefined.count = 0 then
    VariableChangelogUndefined.add(local, xqvalue(), namespace)
  else
    VariableChangelogUndefined.add(local, VariableChangelogUndefined.get(0), namespace  );
end;

function TXQueryEngine.findNamespace(const nsprefix: string): TNamespace;
begin
  if (self <> nil) and (GlobalNamespaces <> nil) and (GlobalNamespaces.hasNamespacePrefix(nsprefix, result)) then exit;
  if GlobalStaticNamespaces.hasNamespacePrefix(nsprefix, result) then exit;
  case nsprefix of
    'xml': result := XMLNamespace_XML;
    'xs': result := XMLNamespace_XMLSchema;
    'xsi': result := XMLNamespace_XMLSchemaInstance;
    'fn': result := XMLNamespace_XPathFunctions;
    'local': result := XMLNamespace_XQueryLocalFunctions;
    'pxp': result := XMLNamespace_MyExtensionsMerged;
    'x': result := XMLNamespace_MyExtensionsNew;
    'array': result := XMLnamespace_XPathFunctionsArray;
    'map': result := XMLnamespace_XPathFunctionsMap;
    //'op': result := XMLNamespace_MyExtensionOperators;
    else result := nil;
  end;
end;

class function TXQueryEngine.findNativeModule(const ns: string): TXQNativeModule;
var
  index: SizeInt;
begin
  index := nativeModules.IndexOf(ns);
  if index < 0 then exit(nil);
  result := TXQNativeModule(nativeModules.Objects[index]);
end;

class function TXQueryEngine.findOperator(const pos: pchar): TXQOperatorInfo;
var
  i: SizeInt;
  j: SizeInt;
  bestMatch: SizeInt;
  sl: TStringList;
  k: SizeInt;
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
      if result <> nil then exit;
    end;
  end;
end;

{$ImplicitExceptions off}
function TXQPathNodeCondition.nodeMatchesExtendedConditions(node: TTreeNode): boolean;
begin
  result := false;
  if not (node.typ in searchedTypes) then exit(); //I tried to move this to findNextNode, but then it fails //text() because it needs to continue through non-text nodes to find one. and sequencetype needs it, too
  if not (xqpncCheckOnSingleChild in options) then begin
  end else begin
    if node.next = node.reverse then exit(); //no child
    if node.next.getNextSibling() <> nil then exit(); //too many children
    if (node.next.typ <> tetOpen) then exit;
    node := node.next //todo: does this affect requiredtype??
  end;
  if ((xqpncCheckValue in options )
      and ((node.hash <> requiredValueHash) or not equalFunction(requiredValue, node.value))) then
    exit();
  if xqpncCheckNamespace in options  then
    if node.namespace = nil then begin
      if requiredNamespaceURL <> '' then exit(); //do not call getNamespaceURL, because returning strings is slow
    end else
      if not node.namespace.equal(requiredNamespaceURL) then exit();
  if (requiredType <> nil) and not (requiredType.instanceOf(node)) then begin
    if requiredType.isSingleType() then
      case node.typ of
        tetOpen: exit(baseSchema.untyped.derivedFrom(requiredType.atomicTypeInfo));
        else exit(baseSchema.untypedAtomic.derivedFrom(requiredType.atomicTypeInfo));
      end;
    exit();
  end;
  result := true;
end;


{$ImplicitExceptions on}


function TXQPathNodeCondition.getNextNode(prev: TTreeNode): TTreeNode;
begin
  result := inherited getNextNode(prev);
  while (result <> nil) do begin
    if nodeMatchesExtendedConditions(result) then exit;
    result := inherited getNextNode(result);
  end;
end;

procedure TXQPathNodeCondition.init(const contextNode: TTreeNode; const command: TXQPathMatchingStep);
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
  options:=[];
  if (qmValue in command.matching) then begin
    Include(options, xqpncCheckValue);
    requiredValueHash := command.valueHash;
  end;
  requiredValue:=command.value;
  if (qmCheckNamespaceURL in command.matching) or (qmCheckNamespacePrefix in command.matching) then
    Include(options, xqpncCheckNamespace);
  requiredNamespaceURL:=command.namespaceURLOrPrefix; //is resolved later
  searchedTypes:=convertMatchingOptionsToMatchedTypes(command.matching);
  requiredType := command.requiredType;
  if qmCheckOnSingleChild in command.matching then Include(options, xqpncCheckOnSingleChild);

  inherited init(contextNode, command.typ);
end;


{ TXQTermModule }


function TXQTermModule.evaluate(var context: TXQEvaluationContext): IXQValue;
var
  i: SizeInt;
  staticContext: TXQStaticContext;
begin
//  if (context.temporaryVariables <> nil) then  we cannot free them here, because it fails when a query calls another module
//    context.temporaryVariables.clear;
  if context.globallyDeclaredVariables <> nil then
    context.globallyDeclaredVariables.clear; //declared global variables

  staticContext := context.staticContext;
  if (length(staticContext.moduleContextItemDeclarations) > 0) then begin
    for i := 0 to high(staticContext.moduleContextItemDeclarations) do
      if not staticContext.moduleContextItemDeclarations[i].isExternal then begin
        context.setContextItem(staticContext.moduleContextItemDeclarations[i].getExpression.evaluate(context));
        break;
      end;
    if context.SeqValue = nil then
      if context.contextNode(false) <> nil then
        context.setContextItem(xqvalue(context.contextNode()));
    if context.SeqValue = nil then
      for i := 0 to high(staticContext.moduleContextItemDeclarations) do
        if staticContext.moduleContextItemDeclarations[i].getExpression <> nil then begin
          context.setContextItem(staticContext.moduleContextItemDeclarations[i].getExpression.evaluate(context));
          break;
       end;
    if context.SeqValue = nil then begin
      if assigned(staticContext.sender.OnDeclareExternalVariable) then
        staticContext.sender.OnDeclareExternalVariable(staticContext, self, '', '$', context.SeqValue);
      if context.SeqValue = nil then
         raise EXQEvaluationException.create('XPDY0002', 'context item missing');
       context.SeqLength := 1;
       context.SeqIndex := 1;
    end;

    for i := 0 to high(staticContext.moduleContextItemDeclarations) do
      if (staticContext.moduleContextItemDeclarations[i].getSequenceType <> nil) and not staticContext.moduleContextItemDeclarations[i].getSequenceType.instanceOf(context.SeqValue, context) then
        raiseXPTY0004TypeError(context.SeqValue, staticContext.moduleContextItemDeclarations[i].getSequenceType.debugTermToString);
  end;
  //the variables must be evaluated now, because an evaluation-when-accessed might be after the context item has changed
  for i := 0 to high(allVariables) do
    if not context.globallyDeclaredVariables.hasVariable(allVariables[i].getVariable) then
      context.globallyDeclaredVariables.add(allVariables[i].getVariable, allVariables[i].getClassicValue(context));

  result := children[high(children)].evaluate(context);
end;

function TXQTermModule.getContextDependencies: TXQContextDependencies;
begin
  if (length(children) = 0) or (children[high(children)] = nil) then exit([]);
  Result:=children[high(children)].getContextDependencies;
end;


{ TXQNativeModule }

constructor TXQNativeModule.create(const anamespace: TNamespace; const aparentModule: array of TXQNativeModule);
var
  i: SizeInt;
begin
  namespace := anamespace;
  namespace._AddRef;
  basicFunctions:=TXQMapStringOwningObject.Create;
  complexFunctions:=TXQMapStringOwningObject.Create;
  interpretedFunctions:=TXQMapStringOwningObject.Create;
  binaryOpLists:=TXQMapStringOwningObject.Create;
  if length(aparentModule) > 0 then begin
    SetLength(parents, length(aparentModule));
    for i := 0 to high(aparentModule) do parents[i] := aparentModule[i];
  end;

  acceptedModels := [xqpmXPath2, xqpmXPath3_0, xqpmXPath3_1, xqpmXQuery1, xqpmXQuery3_0, xqpmXQuery3_1];
end;

constructor TXQNativeModule.create(const anamespace: TNamespace);
begin
  create(anamespace,[]);
end;

destructor TXQNativeModule.Destroy;
var
  i: SizeInt;
begin
  basicFunctions.Clear;
  complexFunctions.Clear;
  interpretedFunctions.Clear;
  binaryOpLists.Clear;

  basicFunctions.free;
  complexFunctions.free;
  binaryOpLists.free;
  interpretedFunctions.free;

  i := nativeModules.IndexOf(namespace.getURL);
  if (i >= 0) and (nativeModules.Objects[i] = self) then nativeModules.Delete(i);

  namespace._Release;

  inherited Destroy;
end;


{$ifdef dumpFunctionTypes}
function pascalType(t: TXQTermSequenceType): string;
var
  i: SizeInt;
  temp: String;
begin
  if t = nil then exit('NIL???');
  with t do begin
  result := t.name;
  case kind of
    tikNone: exit('empty');
    tikAny: result := 'item';
    tikAtomic: begin
      if atomicTypeInfo = nil then exit(name + '(TYPE NOT FOUND)????');
      if atomicTypeInfo = baseSchema.anyAtomicType then result := 'atomic'
      else if atomicTypeInfo = baseJSONiqSchema.object_ then begin
        result := 'map';
        if length(children) > 0 then result += '???';
      end else if atomicTypeInfo = baseJSONiqSchema.array_ then begin
        result := 'array';
        if length(children) > 0 then result += '???';
      end else result := atomicTypeInfo.name;
    end;
    tikFunctionTest: begin
      if length(arguments) = 0 then result := 'functiont'
      else begin
        result := 'function';
        for i := 0 to high(arguments) do begin
          temp := pascalType(arguments[i] as TXQTermSequenceType);
          temp[1] := UpCase(temp[1]);
          result += temp
        end;
        if allowNone or allowMultiple then result += '???';
      end;
    end;
    tikElementTest: begin
      if (nodeMatching.typ = tneaDirectChild) and (nodeMatching.matching = [qmElement,qmText,qmComment,qmProcessingInstruction,qmAttribute,qmDocument])
      then  result := 'node'
      else if (nodeMatching.typ = tneaDirectChild) and (nodeMatching.matching = [qmElement])
      then  result := 'element'
      else result := nodeMatching.serialize()+'????';
    end;
    tikUnion: begin
      result := '';
      for i := 0 to high(children) do
        result += '| ' + (children[i] as TXQTermSequenceType).serialize;
      delete(result, 1, 2);
      result += '???';
    end;
  end;

  if allowNone and allowMultiple then result += 'Star'
  else if allowMultiple then result += 'Plus'
  else if allowNone then result += 'OrEmpty';
  end;
  if result = 'string' then result := 'stringt';
  if result = 'array' then result := 'arrayt';
end;

procedure logFunctionTypes(const name: string; info: TXQAbstractFunctionInfo);
var
  i, j: SizeInt;
  v: TXQFunctionParameterTypes;
begin
  writeln(name);
  if (length(info.versions) > 1) then write('[');
  for i := 0 to high(info.versions) do begin
    write('[');
    v := info.versions[i];
    for j := 0 to high(v.types) do begin
      write(pascalType(v.types[j]));
      write(', ');
    end;
    write(pascalType(v.returnType));
    write(']');
    if i <> high(info.versions) then write(',  ');
  end;
  if (length(info.versions) > 1) then writeln(']')
  else writeln

end;
{$endif}

function TXQNativeModule.registerFunction(const name: string; func: TXQBasicFunction): TXQBasicFunctionInfo;
begin
  result := TXQBasicFunctionInfo.Create;
  result.func := func;
  result.name := name;
  basicFunctions.AddObject(name, result);
end;

function TXQNativeModule.registerFunction(const name: string; func: TXQComplexFunction; contextDependencies: TXQContextDependencies
  ): TXQComplexFunctionInfo;
begin
  result := TXQComplexFunctionInfo.Create;
  result.func := func;
  result.name := name;
  result.contextDependencies:=contextDependencies;
  complexFunctions.AddObject(name, result);
end;

var globalTypeParsingContext: TXQAbstractParsingContext;

procedure TXQNativeModule.setTypeChecking(const name: string; info: TXQAbstractFunctionInfo; const typeChecking: array of string);
begin
  globalTypeParsingContext.parseFunctionTypeInfo(info, typeChecking, true);
  {$ifdef dumpFunctionTypes}logFunctionTypes(name, info);{$endif}
  if length(info.versions) > 0 then begin
    info.name := name;
    info.guessArgCount;
  end;
end;

procedure TXQNativeModule.setTypeChecking(const name: string; info: TXQAbstractFunctionInfo; const typeChecking: array of TXQTermSequenceType);
begin
  info.setVersionsShared(typeChecking);
  info.name := name;
end;

function TXQNativeModule.registerFunction(const name: string; minArgCount, maxArgCount: integer; func: TXQBasicFunction; const typeChecking: array of string): TXQBasicFunctionInfo;
begin
  result := registerFunction(name, func);
  setTypeChecking(name, result, typeChecking);
  result.minArgCount := minArgCount;
  if maxArgCount <> - 1 then result.maxArgCount := maxArgCount
  else result.maxArgCount:=high(result.maxArgCount);
end;


function TXQNativeModule.registerFunction(const name: string; func: TXQBasicFunction; const typeChecking: array of string): TXQBasicFunctionInfo;
begin
  result := registerFunction(name, func);
  setTypeChecking(name, result, typeChecking);
end;

function TXQNativeModule.registerBasicFunction(const name: string; func: TXQBasicFunction; const typeChecking: array of TXQTermSequenceType): TXQBasicFunctionInfo;
begin
  result := registerFunction(name, func);
  setTypeChecking(name, result, typeChecking);
end;


function TXQNativeModule.registerFunction(const name: string; minArgCount, maxArgCount: integer; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies): TXQComplexFunctionInfo;
begin
  result := registerFunction(name, func, contextDependencies);
  setTypeChecking(name, result, typeChecking);
  result.minArgCount := minArgCount;
  if maxArgCount <> - 1 then result.maxArgCount := maxArgCount
  else result.maxArgCount:=high(result.maxArgCount);
end;

function TXQNativeModule.registerFunction(const name: string; func: TXQComplexFunction; const typeChecking: array of string; contextDependencies: TXQContextDependencies = [low(TXQContextDependency)..high(TXQContextDependency)]): TXQComplexFunctionInfo;
begin
  result := registerFunction(name, func, contextDependencies);
  setTypeChecking(name, result, typeChecking);
end;

function TXQNativeModule.registerFunction(const name: string; func: TXQComplexFunction;
  const typeChecking: array of TXQTermSequenceType; contextDependencies: TXQContextDependencies): TXQComplexFunctionInfo;
begin
  result := registerFunction(name, func, contextDependencies);
  setTypeChecking(name, result, typeChecking);
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
  setTypeChecking(name, temp, [typeDeclaration]);
end;

function TXQNativeModule.registerBinaryOp(const name: string; func: TXQBinaryOp; priority: integer; flags: TXQOperatorFlags;
  const typeChecking: array of string; contextDependencies: TXQContextDependencies): TXQOperatorInfo;
begin
  result := registerBinaryOp(name, func, priority, flags, contextDependencies);
  setTypeChecking(name, result, typeChecking);
end;

function TXQNativeModule.registerBinaryOp(const name: string; func: TXQBinaryOp; priority: integer; flags: TXQOperatorFlags;
  const typeChecking: array of TXQTermSequenceType; contextDependencies: TXQContextDependencies): TXQOperatorInfo;
begin
  result := registerBinaryOp(name, func, priority, flags, contextDependencies);
  setTypeChecking(name, result, typeChecking);
end;

function TXQNativeModule.registerBinaryOp(const name: string; func: TXQBinaryOp; priority: integer; flags: TXQOperatorFlags;
  contextDependencies: TXQContextDependencies): TXQOperatorInfo;
var
  spacepos: SizeInt;
  i: SizeInt;
  list: TStringList;
begin
  result := TXQOperatorInfo.Create;
  result.minArgCount := 2; //rather pointless default values
  result.maxArgCount := 2;
  result.name:=name;
  result.func:=func;
  result.priority:=priority;
  result.flags := flags;
  result.contextDependencies:=contextDependencies;
  result.acceptedModels := [xqpmXPath2, xqpmXQuery1, xqpmXPath3_0, xqpmXQuery3_0, xqpmXPath3_1, xqpmXQuery3_1];
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
    include(result.flags, xqofSpecialParsing);
  end;
end;

function TXQNativeModule.findBasicFunction(const name: string; argCount: integer; model: TXQParsingModel): TXQBasicFunctionInfo;
var
  i: SizeInt;
begin
  for i := 0 to high(parents) do begin
    result := parents[i].findBasicFunction(name, argCount, model);
    if result <> nil then exit;
  end;
  if model in acceptedModels then begin
    result := TXQBasicFunctionInfo(findFunction(basicFunctions, name, argCount));
    if result <> nil then exit;
  end;
  result := nil;
end;

function TXQNativeModule.findComplexFunction(const name: string; argCount: integer; model: TXQParsingModel): TXQComplexFunctionInfo;
var
  i: SizeInt;
begin
  for i := 0 to high(parents) do begin
    result := parents[i].findComplexFunction(name, argCount, model);
    if result <> nil then exit;
  end;
  if model in acceptedModels then begin
   result := TXQComplexFunctionInfo(findFunction(complexFunctions, name, argCount));
   if result <> nil then exit;
  end;
  result := nil;
end;

function TXQNativeModule.findInterpretedFunction(const name: string; argCount: integer; model: TXQParsingModel): TXQInterpretedFunctionInfo;
var
  i: SizeInt;
begin
  for i := 0 to high(parents) do begin
    result := parents[i].findInterpretedFunction(name, argCount, model);
    if result <> nil then exit;
  end;
  if model in acceptedModels then begin
   result := TXQInterpretedFunctionInfo(findFunction(interpretedFunctions, name, argCount));
   if result <> nil then exit;
  end;
  result := nil;
end;

{function TXQNativeModule.findBinaryOp(const name: string; model: TXQParsingModel): TXQOperatorInfo;
var
  i: SizeInt;
begin
  for i := 0 to high(parents) do begin
    result := parents[i].findBinaryOp(name, model);
    if result <> nil then exit;
  end;
  if model in acceptedModels then begin
   result := TXQOperatorInfo(findFunction(binaryOpFunctions, name, 2));
   if result <> nil then exit;
  end;
  result := nil;
end;            }

function TXQNativeModule.findSimilarFunctionsDebug(searched: TList; const localname: string): string;
  function strSimilar(const s, ref: string): boolean;
  begin
    result := strContains(s, ref) or strContains(ref, s)
              or (strSimilarity(s, ref) <= min(5, min(length(s) div 2, length(ref) div 2)));
  end;
  function functionName(const name: string; const f: TXQAbstractFunctionInfo): string;
  var
    i, j: SizeInt;
  begin
    result := name + ' ' + '#' + IntToStr(f.minArgCount);
    if f.minArgCount <> f.maxArgCount then result += '-' + IntToStr(f.maxArgCount);;
    if length(f.versions) > 0 then begin
      result += ':';
      for i := 0 to high(f.versions) do begin
        if i <> 0 then result += ';'#9;
        result += '  (';
        for j := 0 to high(f.versions[i].types) do begin
          if j <> 0 then result += ', ';
          if f.versions[i].types[j] <> nil then result += f.versions[i].types[j].serialize;
        end;
        result += ')';
        if f.versions[i].returnType <> nil then result += ' as ' + f.versions[i].returnType.serialize;
      end;
    end;
    result += LineEnding;
  end;
var moduleResult: String;
  i: SizeInt;
begin
  result := '';
  if searched.IndexOf(self) >= 0 then exit;
  searched.Add(self);
  moduleResult := '';
  for  i := 0 to basicFunctions.Count - 1 do
    if strSimilar(localname, basicFunctions[i]) then begin
      moduleResult += '    ' + functionName(basicFunctions[i], TXQBasicFunctionInfo(basicFunctions.Objects[i]));
    end;
  for i := 0 to complexFunctions.Count - 1 do
    if strSimilar(localname, complexFunctions[i]) then
      moduleResult += '    ' + functionName(complexFunctions[i], TXQComplexFunctionInfo(complexFunctions.Objects[i]));
  if moduleResult <> '' then begin
    result += '  In module ' + namespaceGetURL(namespace);
//    if equalNamespaces(namespace, XMLNamespace_XPathFunctions) then
      result += acceptedModels.requiredModelToString;
    result += ':'+LineEnding+moduleResult+LineEnding;
  end;
  for i := 0 to high(parents) do
    result += parents[i].findSimilarFunctionsDebug(searched, localname);
end;


{$ifdef dumpFunctions}
var dumpedModules: array of TXQNativeModule;
procedure TXQNativeModule.dumpFunctions();
var
  prefix,version: String;
  procedure logFunction(f: TXQAbstractFunctionInfo);
  var i: SizeInt;
  begin
    with f do begin
      if length(versions) = 0 then
        writeln(prefix,' name="', xmlStrEscape(name,true), '" min-arg-count="',minArgCount,'" max-arg-count="', maxArgCount,'"/>')
      else
        for i := 0 to high(versions) do begin
          write(prefix,' name="', xmlStrEscape(name,true), '" arg-count="',length(versions[i].types),'" args="');
          if i < length(versions) then write(xmlStrEscape(versions[i].serialize(';'), true));
          writeln('"/>');
        end;
    end;
  end;

var i: SizeInt;
begin
//  writeln(self.namespace.getURL);
  for i := 0 to high(dumpedModules) do if dumpedModules[i] = self then exit;
  dumpedModules := concat(dumpedModules, [self]);
  //const name: string; const info: TXQAbstractFunctionInfo; const typeChecking: array of string
  if xqpmXQuery1 in acceptedModels then version := '1.0'
  else if xqpmXQuery3_0 in acceptedModels then version := '3.0'
  else version := '3.1';
  prefix := '<f m="' + xmlStrEscape(trim(namespace.getURL),true) + '" version="'+version+'"';
  for i := 0 to basicFunctions.Count - 1 do logFunction(basicFunctions.Objects[i] as TXQAbstractFunctionInfo);
  for i := 0 to complexFunctions.Count - 1 do logFunction(complexFunctions.Objects[i] as TXQAbstractFunctionInfo);
  for i := 0 to interpretedFunctions.Count - 1 do logFunction(interpretedFunctions.Objects[i] as TXQAbstractFunctionInfo);

  for i := 0 to high(parents) do parents[i].dumpFunctions;
end;
{$endif}

{$ifdef dumpFunctions}
procedure TXSSchema.logConstructorFunctions;
var
  i, j: SizeInt;
  t: TXSType;
  sversion: string;
  ok: Boolean;
begin
  for i := 0 to typeList.Count - 1 do begin
    t := TXSType( typeList.Objects[i] );
    if isAbstractType(t) or baseSchema.isValidationOnlyType(t) then continue;
    sversion := '1.0';
    case t.name of
      'dateTimeStamp', 'error': sversion := '3.0';
      'null': continue;
    end;
    ok := true;
    for j := 1 to length(t.name) do if not (t.name[j] in ['a'..'z','A'..'Z','-','0'..'9']) then begin ok := false; break; end;
    if not ok then continue;
    writeln('<f m="' , XMLNamespaceURL_XMLSchema, '" version="'+sversion+'" name="', xmlStrEscape(t.name,true), '" args="($arg as xs:anyAtomicType?) as xs:' ,xmlStrEscape(t.name,true), '?"/>');
  end;
end;
{$endif}

class function TXQNativeModule.findFunction(const sl: TStringList; const name: string; argCount: integer): TXQAbstractFunctionInfo;
var
  i: SizeInt;
  idx: SizeInt;
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
  result := defaultQueryEngine.evaluateXQuery(q);
end;

function query(q: string; const vs: array of ixqvalue): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := defaultQueryEngine.evaluateXQuery(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function query(q: string; const vs: array of string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := defaultQueryEngine.evaluateXQuery(q);
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
  TNamespace.freeCache; //if no query engine existed in this thread, this needs to be freed explicitely
end;






procedure raiseXQEvaluationExceptionCallbackImpl(const code, message: string);
begin
  raise EXQEvaluationException.create(code, message);
end;



var xs: TXQNativeModule;
initialization

raiseXQEvaluationExceptionCallback := @raiseXQEvaluationExceptionCallbackImpl;

assert(SizeOf(IXQValue) = sizeof(pointer));
nativeModules := TStringList.Create;
globalTypeParsingContext := createXQParsingContext as TXQAbstractParsingContext;
globalTypeParsingContext.parsingModel := xqpmXQuery3_1;
globalTypeParsingContext.staticContext := TXQStaticContext.Create;
globalTypeParsingContext.options.AllowJSON:=true;
//namespaces
GlobalStaticNamespaces:=TNamespaceList.Create;
XMLNamespace_XPathFunctions:=TNamespace.makeWithRC1(XMLNamespaceURL_XPathFunctions, 'fn');
XMLnamespace_XPathFunctionsArray:=TNamespace.makeWithRC1(XMLNamespaceURL_XPathFunctionsArray, 'array');
XMLnamespace_XPathFunctionsMap:=TNamespace.makeWithRC1(XMLNamespaceURL_XPathFunctionsMap, 'map');
XMLNamespace_XMLSchema:=TNamespace.makeWithRC1(XMLNamespaceURL_XMLSchema, 'xs');
XMLNamespace_XMLSchemaInstance:=TNamespace.makeWithRC1(XMLNamespaceURL_XMLSchemaInstance, 'xsi');
XMLNamespace_XQueryLocalFunctions:=TNamespace.makeWithRC1(XMLNamespaceURL_XQueryLocalFunctions, 'local');
XMLNamespace_MyExtensionsMerged:=TNamespace.makeWithRC1(XMLNamespaceURL_MyExtensionsMerged, 'pxp');
XMLNamespace_MyExtensionsNew:=TNamespace.makeWithRC1(XMLNamespaceURL_MyExtensionsNew, 'x');
XMLNamespace_MyExtensionOperators:=TNamespace.makeWithRC1(XMLNamespaceURL_MyExtensionOperators, 'op');
XMLNamespace_XQuery := TNamespace.makeWithRC1(XMLNamespaceURL_XQuery, '');


TXQueryEngine.registerCollation(TXQCollationCodepointInsensitiveClever.Create('case-insensitive-clever')); //first is default
TXQueryEngine.registerCollation(TXQCollationCodepoint.Create('http://www.w3.org/2005/xpath-functions/collation/codepoint'));
TXQueryEngine.registerCollation(TXQCollationCodepointClever.Create('case-sensitive-clever'));
TXQueryEngine.registerCollation(TXQCollationCodepointInsensitive.Create('http://www.w3.org/2005/xpath-functions/collation/html-ascii-case-insensitive'));
TXQueryEngine.registerCollation(TXQCollationCodepointLocalized.Create('fpc-localized-case-sensitive'));
TXQueryEngine.registerCollation(TXQCollationCodepointLocalizedInsensitive.Create('fpc-localized-case-insensitive'));




GlobalInterpretedNativeFunctionStaticContext:=TXQStaticContext.Create;
GlobalInterpretedNativeFunctionStaticContext.defaultFunctionNamespace := XMLNamespace_MyExtensionsMerged; XMLNamespace_MyExtensionsMerged._AddRef;
GlobalInterpretedNativeFunctionStaticContext.collation := internalDefaultCollation;
GlobalInterpretedNativeFunctionStaticContext.emptyOrderSpec:=xqeoEmptyGreatest;
GlobalInterpretedNativeFunctionStaticContext.defaultTypeNamespace := XMLNamespace_XMLSchema; XMLNamespace_XMLSchema._AddRef;
GlobalInterpretedNativeFunctionStaticContext.copyNamespaceInherit:=true;
GlobalInterpretedNativeFunctionStaticContext.copyNamespacePreserve:=true;
GlobalInterpretedNativeFunctionStaticContext.stringEncoding:=CP_UTF8;
GlobalInterpretedNativeFunctionStaticContext.jsonPXPExtensions:=true;
globalUnnamedVariable := TXQTermVariable.create('placeholder!',nil);


//Constructors (xs: namespace, not fn:)
xs := TXQNativeModule.Create(XMLNamespace_XMLSchema,[]);
TXQueryEngine.registerNativeModule(xs);
globalTypeParsingContext.staticContext.defaultElementTypeNamespace := xs.namespace; xs.namespace._AddRef;
baseSchema := TJSONiqOverrideSchema.create;
baseSchema.url:=XMLNamespace_XMLSchema.getURL;
baseJSONiqSchema := TJSONiqAdditionSchema.create();

globalTypes.init;
xquery__functions.initializeFunctions;

TXQueryInternals.commonValuesUndefined := TXQValueUndefined.create(baseSchema.untyped); //the type should probably be sequence (as undefined = empty-sequence()). however that cause xqts failures atm.
TXQueryInternals.commonValuesTrue := TXQValueBoolean.create(true);
TXQueryInternals.commonValuesFalse := TXQValueBoolean.create(false);


baseSchema.cacheDescendants; //this ignores the jsoniq types

baseSchema.hide('node()');
baseSchema.hide('sequence*');
baseSchema.hide('function(*)');

{$ifdef dumpFunctions}baseSchema.logConstructorFunctions;{$endif}

InitCriticalSection(interpretedFunctionSynchronization);
finalization
freeThreadVars;
DoneCriticalsection(interpretedFunctionSynchronization);

xquery__functions.finalizeFunctions;
xs.free;

globalTypes.free;

nativeModules.free;
globalUnnamedVariable.free;
globalTypeParsingContext.staticContext.Free;
globalTypeParsingContext.free;
baseSchema.free;
baseJSONiqSchema.free;
GlobalInterpretedNativeFunctionStaticContext.Free;
GlobalStaticNamespaces.Free;
TXQueryInternals.commonValuesUndefined := nil;
TXQueryInternals.commonValuesTrue := nil;
TXQueryInternals.commonValuesFalse := nil;
TXQueryEngine.freeCommonCaches;

XMLNamespace_XPathFunctions._Release;
XMLnamespace_XPathFunctionsArray._Release;
XMLnamespace_XPathFunctionsMap._Release;
XMLNamespace_XMLSchema._Release;
XMLNamespace_XMLSchemaInstance._Release;
XMLNamespace_XQueryLocalFunctions._Release;
XMLNamespace_MyExtensionsMerged._Release;
XMLNamespace_MyExtensionsNew._Release;
XMLNamespace_MyExtensionOperators._Release;
XMLNamespace_XQuery._Release;

end.

