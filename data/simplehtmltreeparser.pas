{**
  @abstract This unit contains a html/xml -> tree converter

  @author Benito van der Zander (http://www.benibela.de)
}
unit simplehtmltreeparser;

{$I ../internettoolsconfig.inc}

interface

uses
  Classes, SysUtils, simplehtmlparser, bbutils, contnrs,{$ifdef USE_FLRE}FLRE{$else}ghashmap{$endif};

type
TXQHashKeyString = {$ifdef USE_FLRE}TFLRERawByteString{$else}RawByteString{$endif};
{$ifndef USE_FLRE}TXQHash = record
  function hash(const a: TXQHashKeyString; n: SizeUInt): SizeUInt;
end;{$endif}
generic TXQHashmapStr<TValue> = class({$ifdef USE_FLRE}TFLRECacheHashMap{$else}specialize THashmap<TXQHashKeyString, TValue, TXQHash>{$endif})
protected
  function GetValue(const Key: TXQHashKeyString): TValue; inline;
  procedure SetValue(const Key: TXQHashKeyString; const AValue: TValue); inline;
public
  procedure Add(const Key:TXQHashKeyString; const AValue:TValue); inline;
  property Values[const Key:TXQHashKeyString]: TValue read GetValue write SetValue; default;
end;
generic TXQHashmapStrOwning<TValue, TOwningList> = class(specialize TXQHashmapStr<TValue>)
protected
  owner: TOwningList;
  procedure SetValue(const Key: TXQHashKeyString; const AValue: TValue); inline;
public
  constructor create;
  destructor destroy; override;
  procedure Add(const Key:TXQHashKeyString; const Value:TValue); inline;
  property Values[const Key:TXQHashKeyString]: TValue read GetValue write SetValue; default;
end;
TXQHashmapStrOwningObject = specialize TXQHashmapStrOwning<TObject, TObjectList>;
TXQHashmapStrOwningInterface = specialize TXQHashmapStrOwning<IUnknown, TInterfaceList>;



//**The type of a tree element. <Open>, text, or </close>
TTreeNodeType = (tetOpen, tetClose, tetText, tetComment, tetProcessingInstruction, tetAttribute, tetDocument,
                 tetInternalDoNotUseCDATAText, //tetInternalDoNotUseCDATAText is only used temporarily during parsing to mark elements in which entities should not be replaced.
                 tetNamespace); //not used here, only for XQuery
TTreeNodeTypes = set of TTreeNodeType;
//**Controls the search for a tree element.@br
//**ignore type: do not check for a matching type, ignore text: do not check for a matching text,
//**case sensitive: do not ignore the case, no descend: only check elements that direct children of the current node
TTreeNodeFindOptions = set of (tefoIgnoreType, tefoIgnoreText, tefoCaseSensitive, tefoNoChildren, tefoNoGrandChildren);

TTreeParser = class;
TTreeDocument = class;

{ TTreeNode }

TStringComparisonFunc = function (const a,b: string): boolean of object;

TNamespace = class;

//** Namespace interface, storing url and prefix. (Interface, so it is ref-counted)
INamespace = interface
['{5F6DF5F2-548C-4F13-9BEA-CE59EBAE4CAB}']
  function getPrefix: string; //**< Returns the prefix
  function getURL: string; //**< Returns the url
  function serialize: string; //**< Returns a xmlns attribute declaring this namespace with url and prefix
  function getSelf: TNamespace;
  function equal(const ns: string): boolean;
end;


{ TNamespace }

//** Class implementing the INamespace interface
TNamespace = class(TInterfacedObject, INamespace)
public
  url: string;
  prefix: string;
  //** Creates a new namespace with url and prefix. (watch the argument order. It follows the XPath fn:QName function)
  constructor create(const aurl: string; aprefix: string);

  class function make(const aurl: string; const aprefix: string): TNamespace; static;
  class function uniqueUrl(const aurl: string): string; static;
  class procedure freeCache; static;

  function getPrefix: string;
  function getURL: string;
  function serialize: string;
  function getSelf: TNamespace;
  function equal(const ns: string): boolean;
  destructor Destroy; override;
end;

{ TNamespaceList }

//** List of namespaces
TNamespaceList = class(TInterfaceList)
private
  function getNamespace(const prefix: string): INamespace;
  function getNamespace(i: integer): INamespace;
  function hasNamespacePrefixBefore(const prefix: string; const c: integer): boolean;
public
  function hasNamespacePrefix(const prefix: string; out ns: INamespace): boolean;
  function hasNamespacePrefix(const prefix: string): boolean;
  function hasNamespace(const n: INamespace): boolean;

  function lastIndexOfNamespacePrefix(const prefix: string): integer;

  procedure add(const ns: TNamespace);
  procedure add(const ns: INamespace);
  procedure addIfNewPrefix(const ns: TNamespace);
  procedure addIfNewPrefix(const ns: INamespace);
  procedure addIfNewPrefixUrl(const ns: TNamespace);
  procedure addIfNewPrefixUrl(const ns: INamespace);

  procedure deleteFrom(i: integer);

  function clone: TNamespaceList;

  property namespaces[prefix: string]: INamespace read getNamespace;
  property items[i: integer]: INamespace read getNamespace;
end;

TTreeNode = class;
TTreeAttribute = class;

TAttributeList = class;

{ TAttributeEnumerator }

TAttributeEnumerator = record
  list: TAttributeList;
  index: integer;
  function GetCurrent: TTreeAttribute;
public
  function MoveNext: Boolean;
  property Current: TTreeAttribute read GetCurrent;
end;

{ TAttributeList }

//**@abstract A list of attributes.
//**Currently this is a simple string list, and you can get the values with the values property. (with c++ I would have used map<string, string> but this doesn't exist in Pascal)

TAttributeList = class(TStringList)
public
  constructor Create;
  function getAttribute(i: integer): TTreeAttribute;
  function getAttributeIgnoringNS(const name: string; const cmpFunction: TStringComparisonFunc): TTreeAttribute;
  function getAttributeWithNSPrefix(const namespaceprefix, localname: string; const cmpFunction: TStringComparisonFunc): TTreeAttribute;
  function getValue(i: integer): string;

  procedure add(const name, value: string; const namespace: INamespace = nil);
  procedure add(att: TTreeNode);

  function clone: TAttributeList;

  function GetEnumerator: TAttributeEnumerator;
  destructor Destroy; override;

  property Items[i: integer]: TTreeAttribute read getAttribute;
  property Values[i: integer]: string read getValue;
end;

//**@abstract This class representates an element of the html file
//**It is stored in an unusual  tree representation: All elements form a linked list and the next element is the first children, or if there is none, the next node on the same level, or if there is none, the closing tag of the current parent.@br
//**E.g. a xml file like @code(<foo><bar>x</bar></foo>) is stored as a quadro-linked list:
//**  @longCode(#
//**   /---------------------------------\
//**   |         |  -----------          |                                   link to parent (for faster access, it would work without it)
//**  \|/        | \|/        |          |
//**   '            '
//** <foo> <---> <bar>  <---> x <--->  </bar>  <--->  </foo>                 double linked list of tags (previous link again for faster access, a single linked list would work as well)
//**   .           .                     .               .
//**  /|\         /|\                   /|\             /|\
//**   |           -----------------------               |                   single linked of corresponding node
//**   ---------------------------------------------------
//**  #)
//**There are functions (getNextSibling, getFirstChild, findNext, ...) to access it like a regular tree, but it is easier and faster to work directly with the list.@br
//**Some invariants: (SO: set of opening tags in sequence)@br
//**∀a \in SO: a < a.reverse@br
//**∀a,b \in SO: a < b < a.reverse => a < b.reverse < a.reverse@br
//**@br
//**Attributes should be accessed with the getAttribute or getAttributeTry method. Or you can enumerate them all @code(for attrib in attributes), if attributes is not nil. @br
//** #)
TTreeNode = class
//use the fields if you know what you're doing
  typ: TTreeNodeType; //**<open, close, text or comment node
  value: string; //**< tag name for open/close nodes, text for text/comment nodes
  hash: cardinal; //**< nodeNameHash(value)
  attributes: TAttributeList;  //**<nil if there are no attributes
  next: TTreeNode; //**<next element as in the file (first child if there are childs, else next on lowest level), so elements form a linked list
  previous: TTreeNode; //**< previous element (self.next.previous = self)
  parent: TTreeNode;
  document: TTreeNode;
  reverse: TTreeNode; //**<element paired by open/closing, or corresponding attributes
  namespace: INamespace; //**< Currently local namespace prefix. Might be changed to a pointer to a namespace map in future. (so use getNamespacePrefix and getNamespaceURL instead)

  offset: longint; //**<count of characters in the document before this element (so document_pchar + offset begins with value)

//otherwise use the functions
  //procedure deleteNext(); delete the next node (you have to delete the reverse tag manually)
  procedure deleteAll(); //**< deletes the tree
  procedure changeEncoding(from,toe: TSystemCodePage; substituteEntities: boolean; trimText: boolean); //**<converts the tree encoding from encoding from to toe, and substitutes entities (e.g &auml;)


  //Complex search functions.
  //**Returns the element with the given type and text which occurs before sequenceEnd.@br
  //**This function is nil-safe, so if you call TTreeNode(nil).findNext(...) it will return nil
  function findNext(withTyp: TTreeNodeType; const withText:string; findOptions: TTreeNodeFindOptions=[]; sequenceEnd: TTreeNode = nil):TTreeNode;
  //**Find a matching direct child (equivalent to findNext with certain parameters, but easier to use)@br
  //**A direct child of X is a node Y with Y.parent = X. @br
  //**The options tefoNoChildren, tefoNoGrandChildren have of course no effect. (former is set to false, latter to true)
  function findChild(withTyp: TTreeNodeType; const withText:string; findOptions: TTreeNodeFindOptions=[]): TTreeNode;

  function deepNodeText(separator: string=''):string; //**< concatenates the text of all (including indirect) text children
  function outerXML(insertLineBreaks: boolean = false):string;
  function innerXML(insertLineBreaks: boolean = false):string;
  function outerHTML(insertLineBreaks: boolean = false):string;
  function innerHTML(insertLineBreaks: boolean = false):string;

  function getValue(): string; //**< get the value of this element
  function getValueTry(out valueout:string): boolean; //**< get the value of this element if the element exists
  function hasAttribute(const a: string; const cmpFunction: TStringComparisonFunc = nil): boolean; //**< returns if an attribute with that name exists. cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttribute(const a: string):string; overload; //**< get the value of an attribute of this element or '' if this attribute doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttribute(const a: string; const cmpFunction: TStringComparisonFunc):string; overload; //**< get the value of an attribute of this element or '' if this attribute doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttribute(const a: string; const def: string; const cmpFunction: TStringComparisonFunc = nil):string; overload; //**< get the value of an attribute of this element or '' if this attribute doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttributeTry(const a: string; out valueout: string; const cmpFunction: TStringComparisonFunc = nil):boolean; //**< get the value of an attribute of this element and returns false if it doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttributeTry(a: string; out valueout: TTreeAttribute; cmpFunction: TStringComparisonFunc = nil):boolean; //**< get the value of an attribute of this element and returns false if it doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttributeCount(): integer;

  function getPreviousSibling(): TTreeNode; //**< Get the previous element on the same level or nil if there is none
  function getNextSibling(): TTreeNode; //**< Get the next element on the same level or nil if there is none
  function getFirstChild(): TTreeNode; //**< Get the first child, or nil if there is none
  function getParent(): TTreeNode; //**< Searchs the parent, notice that this is a slow function (neither the parent nor previous elements are stored in the tree, so it has to search the last sibling)
  function getPrevious(): TTreeNode; //**< Searchs the previous, notice that this is a slow function (neither the parent nor previous elements are stored in the tree, so it has to search the last sibling)
  function getRootHighest(): TTreeNode;    //**< Returns the highest node ancestor
  function getRootElement(): TTreeNode;    //**< Returns the highest element node ancestor
  function getDocument(): TTreeDocument; //**< Returns the document node containing this node. Raises an exception if there is no associated document
  function hasDocument(): boolean; //**< Returns if this node is contained in a document

  function getNodeName(): string;        //**< Returns the name as namespaceprefix:name if a namespace exists, or name otherwise. Only attributes, elements and PIs have names.
  function getNamespacePrefix(): string; //**< Returns the namespace prefix. (i.e. 'a' for 'a:b', '' for 'b')
  function getNamespaceURL(): string;    //**< Returns the namespace url. (very slow, it searches the parents for a matching xmlns attribute) cmpFunction controls is used to compare the xmlns: attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getNamespaceURL(prefixOverride: string; cmpFunction: TStringComparisonFunc = nil): string; //**< Returns the url of a namespace prefix, defined in this element or one of his parents cmpFunction controls is used to compare the xmlns: attribute name the searched string. (can be used to switch between case/in/sensitive)
  procedure getOwnNamespaces(var list: TNamespaceList); //**< Returns all namespaces declared or used in this element and its attributes
  procedure getAllNamespaces(var list: TNamespaceList; first: boolean = true); //**< Returns all namespaces declared/used by this element and all ancestors
  function isNamespaceUsed(const n: INamespace): boolean; //**< Tests if a namespace is used by this element or any child (same prefix + url)

  property defaultProperty[name: string]: string read getAttribute; default;

  function isDeepEqual(cmpTo: TTreeNode; ignoredTypes: TTreeNodeTypes = [tetComment, tetProcessingInstruction]; cmpFunction: TStringComparisonFunc = nil): boolean;

  procedure insert(el: TTreeNode); //**< inserts el after the current element (does only change next+previous, not reverse+parent)
  procedure insertSurrounding(before, after: TTreeNode); //**< Surrounds self by before and after, i.e. inserts "before" directly before the element and "after" directly after its closing tag (slow)
  procedure insertSurrounding(basetag: TTreeNode); //**< inserts basetag before the current tag, and creates a matching closing tag after the closing tag of self (slow)

  procedure addAttribute(const aname, avalue: string; const anamespace: TNamespace = nil); inline;
  procedure addAttributes(const props: array of THTMLProperty);
  procedure addNamespaceDeclaration(n: INamespace; overridens: boolean );
  procedure addChild(child: TTreeNode);

  procedure removeElementFromDoubleLinkedList; //removes the element from the double linked list (only updates previous/next)
  function deleteElementFromDoubleLinkedList: TTreeNode; //removes the element from the double linked list (only updates previous/next), frees it and returns next  (mostly useful for attribute nodes)

  function clone: TTreeNode; virtual;
protected
  function serializeXML(nodeSelf: boolean; insertLineBreaks: boolean): string;
  function serializeHTML(nodeSelf: boolean; insertLineBreaks: boolean): string;
  function cloneShallow(): TTreeNode;
  procedure assign(source: TTreeNode); virtual;

  procedure removeAndFreeNext(); //**< removes the next element (the one following self). (ATTENTION: looks like there is a memory leak for opened elements)
  procedure removeElementKeepChildren; //**< removes/frees the current element, but keeps the children (i.e. removes self and possible self.reverse. Will not remove the opening tag, if called on a closing tag)


public
  function toString(): string; reintroduce; //**< converts the element to a string (not recursive)
  function toString(includeText: boolean; includeAttributes: array of string): string; reintroduce; //**< converts the element to a string (not recursive)

  constructor create(); virtual;
  constructor create(atyp: TTreeNodeType; avalue: string = ''); virtual;
  class function createElementPair(const anodename: string): TTreeNode;
  destructor destroy();override;
  procedure initialized; virtual; //**<is called after an element is read, before the next one is read (therefore all fields are valid except next (and reverse for opening tags))

  function caseInsensitiveCompare(const a,b: string): boolean; //**< returns true if a=b case insensitive. Can be passed to getAttribute
  function caseSensitiveCompare(const a,b: string): boolean;   //**< returns true if a=b case sensitive. Can be passed to getAttribute

  class function compareInDocumentOrder(const a,b: TTreeNode): integer; static;
end;


TTreeNodeClass = class of TTreeNode;

{ TTreeAttribute }

TTreeAttribute = class(TTreeNode)
  realvalue: string;
  function isNamespaceNode: boolean;
  function toNamespace: INamespace;
  constructor create(const aname, avalue: string; const anamespace: INamespace = nil);

  procedure setDataTypeHack(i: integer);
  function getDataTypeHack(): integer;
end;

{ TTreeDocument }

TTreeDocument = class(TTreeNode)
protected
  FEncoding: TSystemCodePage;
  FBaseURI, FDocumentURI: string;
  FCreator: TTreeParser;

public
  constructor create(creator: TTreeParser);
  property baseURI: string read FBaseURI write FBaseURI;
  property documentURI: string read FDocumentURI write FDocumentURI;

  function getCreator: TTreeParser;

  //**Returns the current encoding of the tree. After the parseTree-call it is the detected encoding, but it can be overriden with setEncoding.
  function getEncoding: TSystemCodePage;
  //**Changes the tree encoding
  //**If convertExistingTree is true, the strings of the tree are actually converted, otherwise only the meta encoding information is changed
  //**If convertEntities is true, entities like &ouml; are replaced (which is only possible if the encoding is known)
  procedure setEncoding(new: TSystemCodePage; convertFromOldToNew: Boolean; convertEntities: boolean);

  destructor destroy; override;
end;

ETreeParseException = class(Exception);
{ TTreeParser }

TBasicParsingState = (bpmBeforeHtml, bpmBeforeHead, bpmInHead, bpmAfterHead, bpmInBody, bpmInFrameset, bpmAfterBody, bpmAfterAfterBody);
//**Parsing model used to interpret the document
//**pmStrict: every tag must be closed explicitely (otherwise an exception is raised)
//**pmHtml: accept everything, tries to create the best fitting tree using a heuristic to recover from faulty documents (no exceptions are raised), detect encoding
TParsingModel = (pmStrict, pmHTML, pmUnstrictXML);
//**@abstract This parses a html/sgml/xml file to a tree like structure.
//**To use it, you have to call @code(parseTree) with a string containing the document. Afterwards you can call @code(getLastTree) to get the document root node.@br
//**
//**The data structure is like a stream of annotated tokens with back links (so you can traverse it like a tree).@br
//**If TargetEncoding is not CP_NONE, the parsed data is automatically converted to that encoding. (the initial encoding is detected depending on the unicode BOM, the xml-declaration, the content-type header, the http-equiv meta tag and invalid characters.)
//**You can change the class used for the elements in the tree with the field treeNodeClass.
TTreeParser = class
  function processingInstruction(text: pchar; textLen: longint; unusedParameter: TTextFlags): TParsingResult;
protected
  FAutoDetectHTMLEncoding: boolean;
  FReadProcessingInstructions: boolean;
//  FConvertEntities: boolean;
  FCurrentElement: TTreeNode;
  FTemplateCount: Integer;
  FElementStack: TList;
  FAutoCloseTag: boolean;
  FCurrentFile: string;
  FParsingModel: TParsingModel;
  FTrimText, FReadComments: boolean;
  FTrees: TList;
  FCurrentTree: TTreeDocument;
  FXmlHeaderEncoding: TSystemCodePage;
  FRepairMissingStartTags, FRepairMissingEndTags: boolean;


  function newTreeNode(typ:TTreeNodeType; text: pchar; len:longint):TTreeNode;
  function newTreeNode(typ:TTreeNodeType; s: string; offset: integer):TTreeNode;
  procedure autoCloseLastTag();
  function autoCloseTill(const ctag: string): TTreeNode;

  function prependTag(const tag: string): TTreeNode;
  procedure doRepairMissingStartTags(const tag: string);

  function enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):TParsingResult;
  function leaveTag(tagName: pchar; tagNameLen: longint):TParsingResult;
  function readText(text: pchar; textLen: longint; tf: TTextFlags):TParsingResult;
  function readComment(text: pchar; textLen: longint):TParsingResult;

private
  //in-scope namespaces
  FCurrentNamespace: INamespace;
  FCurrentNamespaces: TNamespaceList;
  FCurrentNamespaceDefinitions: TList;
  FCurrentAndPreviousNamespaces: TNamespaceList; //all namespaces encountered during parsing
  FTargetEncoding: TSystemCodePage;
  FHasOpenedPTag: boolean;
  FBasicParsingState: TBasicParsingState; //similar to html 5. Only used when repair start tags is enabled
  FLastHead, flastbody, flasthtml: TTreeNode;
  procedure pushNamespace(const url, prefix: string);
  function findNamespace(const prefix: string): INamespace;

  function htmlTagWeight(s:string): integer;
  class function htmlElementChildless(const s:string): boolean; static;
  class function htmlElementIsCDATA(const s: string): boolean; static;
  class function htmlElementClosesPTag(const s: string): boolean; static;
public
  treeNodeClass: TTreeNodeClass; //**< Class of the tree nodes. You can subclass TTreeNode if you need to store additional data at every node
  globalNamespaces: TNamespaceList;
  allowTextAtRootLevel: boolean;

  constructor Create;
  destructor destroy;override;
  procedure clearTrees;
  //** Creates a new tree from a html document contained in html. @br
  //** contentType is used to detect the encoding
  function parseTree(html: string; uri: string = ''; contentType: string = ''): TTreeDocument; virtual;
  function parseTreeFromFile(filename: string): TTreeDocument; virtual;

  function getLastTree: TTreeDocument; //**< Returns the last created tree

  procedure removeEmptyTextNodes(const whenTrimmed: boolean);
published
  //** Parsing model, see TParsingModel
  property parsingModel: TParsingModel read FParsingModel write FParsingModel;
  property repairMissingStartTags: boolean read FrepairMissingStartTags write FrepairMissingStartTags ;
  property repairMissingEndTags: boolean read FRepairMissingEndTags  write FRepairMissingEndTags ;
  //** If this is true (default is false), white space is removed from text nodes
  property trimText: boolean read FTrimText write FTrimText;
  //** If this is true (default is false) comments are included in the generated tree
  property readComments: boolean read FReadComments write FReadComments;
  //** If this is true (default is false) processing instructions are included in the generated tree
  property readProcessingInstructions: boolean read FReadProcessingInstructions write FReadProcessingInstructions;
  //** Determines if the encoding should be automatically detected (default true)
  property autoDetectHTMLEncoding: boolean read FAutoDetectHTMLEncoding write fautoDetectHTMLEncoding;
//  property convertEntities: boolean read FConvertEntities write FConvertEntities;
  property TargetEncoding: TSystemCodePage read FTargetEncoding write FTargetEncoding;
  //trees owned/created by this object (which will be destroyed, when it is freed) (mostly for internal use)
  property OwnedTrees: TList read FTrees;
end;


function xmlStrEscape(s: string; attrib: boolean = false):string;
function xmlStrWhitespaceCollapse(const s: string):string;
function htmlStrEscape(s: string; attrib: boolean = false; encoding: TSystemCodePage = CP_NONE):string;

const XMLNamespaceUrl_XML = 'http://www.w3.org/XML/1998/namespace';
      XMLNamespaceUrl_XMLNS = 'http://www.w3.org/2000/xmlns/';
const TreeNodesWithChildren = [tetOpen, tetDocument];

var
   XMLNamespace_XMLNS, XMLNamespace_XML: INamespace;

function equalNamespaces(const ans, bns: INamespace): boolean; inline;
function equalNamespaces(const ans, bns: string): boolean; inline;
function namespaceGetURL(const n: INamespace): string; inline;


type TInternetToolsFormat = (itfXML, itfHTML, itfJSON, itfXMLPreparsedEntity {<- not used, might be used in future});
function guessFormat(const data, uri, contenttype: string): TInternetToolsFormat;

function strEncodingFromContentType(const contenttype: string): TSystemCodePage;
function isInvalidUTF8(const s: string): boolean;
function nodeNameHash(const s: RawByteString): cardinal;


implementation
uses xquery;

type THTMLOmittedEndTagInfo = class
  siblings, parents, additionallyclosed: TStringArray;
  constructor create(somesiblings, someparents: array of string);
  constructor create(somesiblings, someparents, someadditionallyclosed: array of string);
end;

{ THTMLOmittedEndTags }

THTMLOmittedEndTags = class
  tagInfo: array[0..25] of array of THTMLOmittedEndTagInfo;
  procedure add(tag: THTMLOmittedEndTagInfo);
  function find(tag: string): THTMLOmittedEndTagInfo;
  destructor destroy; override;
end;
var omittedEndTags: THTMLOmittedEndTags;
    omittedStartTags: THTMLOmittedEndTags;

function arrayContainsI(const a: array of string; const s: string): boolean;
var
  i: Integer;
begin
  for i:=0 to high(a) do if striEqual(a[i], s) then exit(true);
  exit(false);
end;

{$PUSH}{$RangeChecks off}{$OverflowChecks off}
function nodeNameHash(const s: RawByteString): cardinal;
var
  p, last: PByte;
begin
  if s = '' then exit(1);
  p := pbyte(pointer(s));
  last := p + length(s);
  result := 0;
  while p < last do begin
    if p^ < 128  then begin //give the same hash independent of latin1/utf8 encoding and collation
      result := result + p^;
      if (p^ >= ord('a')) and (p^ <= ord('z')) then result := result - ord('a') + ord('A');
      result := result + (result shl 10);
      result := result xor (result shr 6);
    end;
    inc(p);
  end;

  result := result + (result shl 3);
  result := result xor (result shr 11);
  result := result + (result shl 15);
end;
function nodeNameHashCheckASCII(const s: RawByteString): cardinal;
var
  i: Integer;
begin
  for i := 1 to length(s) do if s[i] >= #128 then exit(0);
  result := nodeNameHash(s);
end;

{$ifndef USE_FLRE}
function TXQHash.hash(const a: TXQHashKeyString; n: SizeUInt): SizeUInt;
begin
  result := nodeNameHash(a) and (n-1);
end;
{$endif}

{$POP}



function TXQHashmapStr.GetValue(const Key: TXQHashKeyString): TValue;
begin
  {$ifdef USE_FLRE}
  result := TValue(pointer(inherited GetValue(key)));
  {$else}
  if not inherited GetValue(key, result) then result := default(TValue);
  {$endif}
end;

procedure TXQHashmapStr.SetValue(const Key: TXQHashKeyString; const AValue: TValue);
begin
  {$ifdef USE_FLRE}
  inherited SetValue(key, TFLRECacheHashMapData(pointer(AValue)) );
  {$else}
  insert(key, AValue);
  {$endif}
end;

procedure TXQHashmapStr.Add(const Key: TXQHashKeyString; const AValue: TValue);
begin
  {$ifdef USE_FLRE}
  inherited Add(key, TFLRECacheHashMapData(pointer(AValue)));
  {$else}
  insert(key, AValue);
  {$endif}
end;

procedure TXQHashmapStrOwning.SetValue(const Key: TXQHashKeyString; const AValue: TValue);
var
  old: TValue;
begin
  old := GetValue(key);
  if old = AValue then exit;
  if old <> nil then owner.remove(old);
  add(key, Avalue);
end;

constructor TXQHashmapStrOwning.create;
begin
  inherited;
  owner := TOwningList.create;
end;

destructor TXQHashmapStrOwning.destroy;
begin
  owner.free;
  inherited destroy;
end;

procedure TXQHashmapStrOwning.Add(const Key: TXQHashKeyString; const Value: TValue);
begin
  owner.add(value);
  inherited add(key, value);
end;

constructor THTMLOmittedEndTagInfo.create(somesiblings, someparents: array of string);
var
  i: Integer;
begin
  SetLength(siblings, length(somesiblings));
  for i := 0 to high(siblings) do siblings[i] := somesiblings[i];
  SetLength(parents, length(someparents));
  for i := 0 to high(parents) do parents[i] := someparents[i];
  for i := 1 to high(somesiblings) do
    if somesiblings[i][1] <> somesiblings[0][1] then raise Exception.Create('initialization error');
end;
constructor THTMLOmittedEndTagInfo.create(somesiblings, someparents, someadditionallyclosed: array of string);
var
  i: Integer;
begin
  SetLength(siblings, length(somesiblings));
  for i := 0 to high(siblings) do siblings[i] := somesiblings[i];
  SetLength(parents, length(someparents));
  for i := 0 to high(parents) do parents[i] := someparents[i];
  SetLength(additionallyclosed, length(someadditionallyclosed));
  for i := 0 to high(additionallyclosed) do additionallyclosed[i] := someadditionallyclosed[i];
  for i := 1 to high(somesiblings) do
    if somesiblings[i][1] <> somesiblings[0][1] then raise Exception.Create('initialization error');
end;

procedure THTMLOmittedEndTags.add(tag: THTMLOmittedEndTagInfo);
var
  idx: Integer;
begin
  idx := ord(tag.siblings[0][1]) - ord('a');
  SetLength(tagInfo[idx], length(tagInfo[idx])+1);
  tagInfo[idx][high(tagInfo[idx])] := tag;
end;

function THTMLOmittedEndTags.find(tag: string): THTMLOmittedEndTagInfo;
var
  idx: Integer;
  i: Integer;
begin
  if tag = '' then exit(nil);
  if (tag[1] >= 'a') and (tag[1] <= 'z') then idx := ord(tag[1]) - ord('a')
  else if (tag[1] >= 'A') and (tag[1] <= 'Z') then idx := ord(tag[1]) - ord('A')
  else exit(nil);
  for i := 0 to high(tagInfo[idx]) do
    if arrayContainsI(tagInfo[idx][i].siblings, tag) then exit(tagInfo[idx][i]);
  exit(nil);
end;

destructor THTMLOmittedEndTags.destroy;
var
  i,j: Integer;
begin
  for i := 0 to high(tagInfo) do
    for j := 0 to high(tagInfo[i]) do
      tagInfo[i][j].free;
  inherited destroy;
end;


{ TAttributeEnumerator }

function TAttributeEnumerator.GetCurrent: TTreeAttribute;
begin
  result := list.items[index];
end;

function TAttributeEnumerator.MoveNext: Boolean;
begin
  index += 1;
  result := index < list.Count;
end;

{ TTreeAttribute }

function TTreeAttribute.isNamespaceNode: boolean;
begin
  result := ((namespace = nil) and (value = 'xmlns')) or ((namespace <> nil) and (namespace.getURL = XMLNamespaceUrl_XMLNS));
end;

function TTreeAttribute.toNamespace: INamespace;
begin
  if namespace = nil then result := TNamespace.Make(realvalue, '') //TODO: reuse
  else result := TNamespace.Make(realvalue, value);
end;

constructor TTreeAttribute.create(const aname, avalue: string; const anamespace: INamespace = nil);
begin
  inherited create(tetAttribute, aname);
  realvalue := avalue;
  namespace := anamespace;
end;

var attributeDataTypeHackList1, attributeDataTypeHackList2: TAttributeList;

procedure TTreeAttribute.setDataTypeHack(i: integer);
begin
  case i of
    1: attributes := attributeDataTypeHackList1;
    2: attributes := attributeDataTypeHackList2;
    else attributes := nil;
  end;
end;

function TTreeAttribute.getDataTypeHack: integer;
begin
  if attributes = attributeDataTypeHackList1 then exit(1);
  if attributes = attributeDataTypeHackList2 then exit(2);
  result := 0;
end;

{ TAttributeList }

constructor TAttributeList.Create;
begin
  OwnsObjects:=true;
end;

function TAttributeList.getAttribute(i: integer): TTreeAttribute;
begin
  result := TTreeAttribute(Objects[i]);
end;

function TAttributeList.getAttributeIgnoringNS(const name: string; const cmpFunction: TStringComparisonFunc): TTreeAttribute;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if cmpFunction(Items[i].value, name) then
      exit(items[i]);
  exit(nil);
end;

function TAttributeList.getAttributeWithNSPrefix(const namespaceprefix, localname: string; const cmpFunction: TStringComparisonFunc
  ): TTreeAttribute;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if cmpFunction(Items[i].value, localname) and cmpFunction(namespaceprefix, items[i].getNamespacePrefix()) then
      exit(items[i]);
  exit(nil);
end;

function TAttributeList.getValue(i: integer): string;
begin
  result := TTreeAttribute(Objects[i]).value;
end;

procedure TAttributeList.add(const name, value: string; const namespace: INamespace = nil);
begin
  AddObject(name, TTreeAttribute.create(name, value, namespace));
end;

procedure TAttributeList.add(att: TTreeNode);
begin
  AddObject(TTreeAttribute(att).value, att);
end;

function TAttributeList.clone: TAttributeList;
var
  i: Integer;
begin
  result := TAttributeList.Create;
  for i:= 0 to count - 1 do
    Result.add(items[i].clone);
end;


function TAttributeList.GetEnumerator: TAttributeEnumerator;
begin
  result.list := self;
  Result.index:=-1;
end;

destructor TAttributeList.Destroy;
begin
  clear;
  inherited Destroy;
end;

{ TNamespaceList }

function TNamespaceList.getNamespace(const prefix: string): INamespace;
begin
  hasNamespacePrefix(prefix, result);
end;

function TNamespaceList.getNamespace(i: integer): INamespace;
begin
  result := INamespace(inherited get(i)) ;
end;

function TNamespaceList.hasNamespacePrefixBefore(const prefix: string; const c: integer): boolean;
var
  i: Integer;
begin
  for i := c - 1 downto 0 do
    if (Items[i]).getPrefix = prefix then exit(true);
  exit(false);
end;

function TNamespaceList.hasNamespacePrefix(const prefix: string; out ns: INamespace): boolean;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if (Items[i]).getPrefix = prefix then begin
      ns := items[i];
      exit(true);
    end;
  ns := nil;
  exit(false);
end;

function TNamespaceList.hasNamespacePrefix(const prefix: string): boolean;
var temp: INamespace;
begin
  result := hasNamespacePrefix(prefix, temp);
end;

function TNamespaceList.hasNamespace(const n: INamespace): boolean;
var
  temp: INamespace;
begin
  if not hasNamespacePrefix(n.getPrefix, temp) then exit(false);
  if temp.getURL <> n.getURL then exit(false);
  result := true;
end;

function TNamespaceList.lastIndexOfNamespacePrefix(const prefix: string): integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if (Items[i]).getPrefix = prefix then
      exit(i);
  exit(-1);
end;

procedure TNamespaceList.add(const ns: TNamespace);
begin
  inherited add(INamespace(ns)); //hide ancestor method to prevent crash when tnamespace is treated as inamespace instead being cast
end;

procedure TNamespaceList.add(const ns: INamespace);
begin
  inherited add(ns);
end;

procedure TNamespaceList.addIfNewPrefix(const ns: TNamespace);
begin
  addIfNewPrefix(INamespace(ns));
end;

procedure TNamespaceList.addIfNewPrefix(const ns: INamespace);
var
  temp: INamespace;
begin
  if (ns = nil) or (ns.getURL = XMLNamespaceUrl_XMLNS) or (ns.getURL = XMLNamespaceUrl_XML) then exit;
  if not hasNamespacePrefix(ns.getPrefix, temp) then
    add(ns);
end;

procedure TNamespaceList.addIfNewPrefixUrl(const ns: TNamespace);
begin
  addIfNewPrefixUrl(INamespace(ns));
end;

procedure TNamespaceList.addIfNewPrefixUrl(const ns: INamespace);
var
  temp: INamespace;
begin
  if (ns = nil) or (ns.getURL = XMLNamespaceUrl_XMLNS) or (ns.getURL = XMLNamespaceUrl_XML) then exit;
  if not hasNamespacePrefix(ns.getPrefix, temp) then
    add(ns)
  else if temp.getURL <> ns.getURL then
    add(ns);
end;

procedure TNamespaceList.deleteFrom(i: integer);
begin
  if i < 0 then i := 0;
  while count > i do
    delete(count - 1);
end;


function TNamespaceList.clone: TNamespaceList;
var
  i: Integer;
begin
  result := TNamespaceList.Create;
  for i := 0 to count - 1 do
    result.Add(items[i]);
end;

type TNamespaceCache = class
  uniqueUrl: string;
  prefixes: TXQHashmapStrOwningInterface;
  constructor Create;
  destructor Destroy; override;
end;

constructor TNamespaceCache.Create;
begin
  prefixes := TXQHashmapStrOwningInterface.Create;
end;

destructor TNamespaceCache.Destroy;
begin
  prefixes.free;
  inherited Destroy;
end;

threadvar globalNamespaceCache: TXQHashmapStrOwningObject;

function TNamespace.getSelf: TNamespace;
begin
  result := self;
end;

function TNamespace.equal(const ns: string): boolean;
begin
  result := strEqual(url, ns);
end;

constructor TNamespace.create(const aurl: string; aprefix: string);
begin
  url := aurl;
  prefix := aprefix;
end;

function namespaceCache(const aurl: string): TNamespaceCache;
begin
  if globalNamespaceCache = nil then globalNamespaceCache := TXQHashmapStrOwningObject.Create();
  result := TNamespaceCache(globalNamespaceCache[aurl]);
  if result = nil then begin
    result := TNamespaceCache.Create;
    result.uniqueUrl := aurl;
    globalNamespaceCache.Add(aurl, result);
    //writeln(strFromPtr(pointer(aurl)), ' ',aurl);
  end;
end;

class function TNamespace.make(const aurl: string; const aprefix: string): TNamespace;
var cache : TNamespaceCache;
  tempptr: Pointer;
begin
  cache := namespaceCache(aurl);
  tempptr := pointer(cache.prefixes[aprefix]);
  if tempptr = nil then begin
    result := TNamespace.create(cache.uniqueUrl, aprefix);
    cache.prefixes.Add(aprefix, result);
  end else result := (IUnknown(tempptr) as INamespace).getSelf;
end;

class function TNamespace.uniqueUrl(const aurl: string): string;
begin
  result := namespaceCache(aurl).uniqueUrl;
end;

class procedure TNamespace.freeCache;
begin
  FreeAndNil(globalNamespaceCache);
end;

function TNamespace.getPrefix: string;
begin
  if self = nil then exit('');
  result := prefix;
end;

function TNamespace.getURL: string;
begin
  if self = nil then exit('');
  result := url;
end;

function TNamespace.serialize: string;
begin
  if prefix = '' then result := 'xmlns="'+xmlStrEscape(url, true)+'"'
  else result := 'xmlns:'+prefix+'="'+xmlStrEscape(url, true)+'"'
end;

destructor TNamespace.Destroy;
begin
  inherited Destroy;
end;

{ TTreeDocument }

constructor TTreeDocument.create(creator: TTreeParser);
begin
  inherited create(tetDocument);
  FCreator := creator;
end;

function TTreeDocument.getCreator: TTreeParser;
begin
  result := FCreator;
end;

function TTreeDocument.getEncoding: TSystemCodePage;
begin
  if self = nil then exit(CP_NONE);
  result := FEncoding;
end;

procedure TTreeDocument.setEncoding(new: TSystemCodePage; convertFromOldToNew: Boolean; convertEntities: boolean);
begin
  if self = nil then exit;
  if (FEncoding = CP_NONE) or not convertFromOldToNew then FEncoding:= new;
  if convertFromOldToNew or convertEntities then changeEncoding(FEncoding, new, convertEntities, FCreator.FTrimText);
  FEncoding := new;
end;

destructor TTreeDocument.destroy;
begin
  inherited destroy;
end;

{ TTreeNode }

{procedure TTreeNode.deleteNext();
var
  temp: TTreeNode;
begin
  if next = nil then exit;
  temp := next;
  next := next.next;
  temp.Free;
end;}

procedure TTreeNode.deleteAll();
var cur: TTreeNode;
  cnext: TTreeNode;
begin
  cur := self;
  while cur <> nil do begin
    cnext := cur.next;
    cur.next := nil;
    cur.free;
    cur := cnext;
  end;
end;

procedure TTreeNode.changeEncoding(from, toe: TSystemCodePage; substituteEntities: boolean; trimText: boolean);
  function change(s: string): string;
  begin
    result := strChangeEncoding(s, from, toe);
    result := strNormalizeLineEndings(result);
    if substituteEntities then result := strDecodeHTMLEntities(result, toe, false);
    if trimText then result := trim(result); //retrim because &#x20; replacements could have introduced new spaces
  end;

var tree: TTreeNode;
  attrib: TTreeAttribute;
begin
  if (from = CP_NONE) or (toe = CP_NONE) then exit;
  if (from = toe) and not substituteEntities then exit;
  tree := self;
  while tree <> nil do begin
    case tree.typ of
      tetText: tree.value := change(tree.value);
      tetInternalDoNotUseCDATAText: begin
        tree.value:=strNormalizeLineEndings(strChangeEncoding(tree.value, from, toe));
        tree.typ := tetText;
      end;
      tetComment: tree.value:=strChangeEncoding(tree.value, from, toe);
      tetProcessingInstruction: begin
        tree.value:=trim(strNormalizeLineEndings(strChangeEncoding(tree.value, from, toe))); //line endings/trim needed?
        tree.hash := nodeNameHash(tree.value);
      end;
      tetDocument, tetOpen, tetClose: begin
        tree.value := change(tree.value);
        if tree.hash = 0 then tree.hash := nodeNameHash(tree.value);
        if tree.attributes <> nil then
          for attrib in tree.attributes do begin
            attrib.value := change(attrib.value);
            if attrib.hash = 0 then attrib.hash := nodeNameHash(attrib.value);
            attrib.realvalue := change(attrib.realvalue);
            if attrib.isNamespaceNode then attrib.realvalue := xmlStrWhitespaceCollapse(attrib.realvalue);
          end;
      end;
      else raise ETreeParseException.Create('Unkown tree element: '+tree.outerXML());
    end;
    tree := tree.next;
  end;
end;

{$ImplicitExceptions off}
function TTreeNode.findNext(withTyp: TTreeNodeType; const withText: string; findOptions: TTreeNodeFindOptions =[]; sequenceEnd: TTreeNode = nil): TTreeNode;
var cur: TTreeNode;
  //splitted: array of string;
begin
  if self = nil then exit(nil);
  {if (tefoSplitSlashes in findOptions) and not (tefoIgnoreType in findOptions) and not (tefoIgnoreText in findOptions) and (withTyp = tetOpen) and (pos('/'.withText) > 0) then begin
    result := findNext(tetOpen, strSplitGet('/', withText), findOptions - [tefoSplitSlashes], sequenceEnd);
    while (result <> nil) and (withText <> '') do
      result := result.findNext(tetOpen, strSplitGet('/', withText), findOptions - [tefoSplitSlashes], result.reverse);
    exit();
  end;}
  if (tefoNoChildren in findOptions) and (self.typ in TreeNodesWithChildren) then cur := self.reverse
  else cur := self.next;
  while (cur <> nil) and (cur <> sequenceEnd) do begin
    if ((cur.typ = withTyp) or (tefoIgnoreType in findOptions)) and
       ((tefoIgnoreText in findOptions) or
           ( (tefoCaseSensitive in findOptions) and (cur.value = withText) ) or
           ( not (tefoCaseSensitive in findOptions) and (striequal(cur.value, withText) ) ) ) then
             exit(cur);
    if (tefoNoGrandChildren in findOptions) and (cur.typ in TreeNodesWithChildren) then cur := cur.reverse
    else cur := cur.next;
  end;
  result := nil;
end;

function TTreeNode.findChild(withTyp: TTreeNodeType; const withText: string;
  findOptions: TTreeNodeFindOptions): TTreeNode;
begin
  result := nil;
  if self = nil then exit;
  if not (typ in TreeNodesWithChildren) then exit;
  if reverse = nil then exit;
  result:=findNext(withTyp, withText, findOptions + [tefoNoGrandChildren] - [tefoNoChildren], reverse);
end;

{$ImplicitExceptions on}

function TTreeNode.deepNodeText(separator: string): string;
var cur:TTreeNode;
begin
  result:='';
  if self = nil then exit;
  cur := next;
  while (cur<>nil) and (cur <> reverse) do begin
    if cur.typ = tetText then result:=result+cur.value+separator;
    cur := cur.next;
  end;
  if (result<>'') and (separator<>'') then setlength(result,length(result)-length(separator));
end;

function TTreeNode.outerXML(insertLineBreaks: boolean = false): string;
begin
  result := serializeXML(true, insertLineBreaks);
end;

function TTreeNode.innerXML(insertLineBreaks: boolean = false): string;
begin
  result := serializeXML(false, insertLineBreaks);
end;

function TTreeNode.outerHTML(insertLineBreaks: boolean): string;
begin
  result := serializeHTML(true, insertLineBreaks);
end;

function TTreeNode.innerHTML(insertLineBreaks: boolean): string;
begin
  result := serializeHTML(false, insertLineBreaks);
end;

function TTreeNode.getValue(): string;
begin
  if self = nil then exit('');
  result := value;
end;

function TTreeNode.getValueTry(out valueout:string): boolean;
begin
  if self = nil then exit(false);
  valueout := self.value;
  result := true;
end;

function TTreeNode.hasAttribute(const a: string; const cmpFunction: TStringComparisonFunc = nil): boolean;
var temp: TTreeAttribute;
begin
  exit(getAttributeTry(a, temp, cmpFunction));
end;

function TTreeNode.getAttribute(const a: string): string; overload;
begin
  result := getAttribute(a, @caseInsensitiveCompare);
end;

function TTreeNode.getAttribute(const a: string; const cmpFunction: TStringComparisonFunc): string; overload;
begin
  if not getAttributeTry(a, result, cmpFunction) then
    result:='';
end;

function TTreeNode.getAttribute(const a: string; const def: string; const cmpFunction: TStringComparisonFunc = nil):string; overload;
begin
  if not getAttributeTry(a, result, cmpFunction) then
    result:=def;
end;

function TTreeNode.getAttributeTry(const a: string; out valueout: string; const cmpFunction: TStringComparisonFunc = nil): boolean;
var temp: TTreeAttribute;
begin
  result := getAttributeTry(a, temp, cmpFunction);
  if not result then exit;
  valueout := temp.realvalue;
end;

function TTreeNode.getAttributeTry(a: string; out valueout: TTreeAttribute; cmpFunction: TStringComparisonFunc = nil): boolean;
var
  ns: string;
begin
  result := false;
  if (self = nil) or (attributes = nil) then exit;
  if cmpFunction = nil then cmpFunction:=@caseInsensitiveCompare;
  if pos(':', a) = 0 then
    valueout := attributes.getAttributeIgnoringNS(a, cmpFunction)
  else begin
    ns := strSplitGet(':', a);
    valueout := attributes.getAttributeWithNSPrefix(ns, a, cmpFunction)
  end;
  result := valueout <> nil;
end;

function TTreeNode.getAttributeCount: integer;
begin
  if (self = nil) or (attributes = nil) then exit(0);
  result := attributes.Count;
end;

function TTreeNode.getPreviousSibling: TTreeNode;
begin
  result := previous;
  if result = nil then exit;
  case result.typ of
    tetOpen, tetDocument: result := nil;
    tetClose: result := result.reverse;
  end;
end;

function TTreeNode.getNextSibling(): TTreeNode;
begin
  case typ of
    tetOpen, tetDocument: result:=reverse.next;
    tetText, tetClose, tetComment, tetProcessingInstruction: result := next;
    else raise ETreeParseException.Create('Invalid tree element type');
  end;
  if result = nil then exit;
  if result.typ = tetClose then exit(nil);
end;

function TTreeNode.getFirstChild(): TTreeNode;
begin
  if not (typ in TreeNodesWithChildren) then exit(nil);
  if next = reverse then exit(nil);
  exit(next);
end;

function TTreeNode.getParent(): TTreeNode;
begin
  if (self = nil) then exit(nil);
  exit(parent);
end;

function TTreeNode.getPrevious: TTreeNode;
begin
  if self = nil then exit(nil);
  result := previous
end;

function TTreeNode.getRootHighest: TTreeNode;
begin
  if self = nil then exit(nil);
  result := document;
end;

function TTreeNode.getRootElement: TTreeNode;
begin
  result := document;
  if (result = nil) or (result.typ = tetOpen) then exit;
  exit(result.findChild(tetOpen,'',[tefoIgnoreText]));
end;

function TTreeNode.getDocument: TTreeDocument;
begin
  result := document as TTreeDocument;
end;

function TTreeNode.hasDocument: boolean;
begin
  result := document is TTreeDocument;
end;

{$ImplicitExceptions off}

function TTreeNode.getNodeName: string;
begin
  case typ of
    tetOpen, tetAttribute, tetClose, tetProcessingInstruction: begin
      if (namespace = nil) or (namespace.getPrefix = '') then exit(value);
      exit(getNamespacePrefix() + ':' + value);
    end;
    else result := '';
  end;
end;



function TTreeNode.getNamespacePrefix: string;
begin
  if namespace = nil then exit('');
  result := namespace.getPrefix;
end;

function TTreeNode.getNamespaceURL(): string;
begin
  if namespace = nil then exit('');
  result := namespace.getURL;
end;

function TTreeNode.getNamespaceURL(prefixOverride: string; cmpFunction: TStringComparisonFunc = nil): string;
var
  n: TTreeNode;
  attrib: String;
begin
  if (namespace <> nil) and (namespace.getPrefix = prefixOverride) then exit(namespace.getURL) ;
  if prefixOverride <> '' then prefixOverride:=':'+prefixOverride;
  attrib := 'xmlns' + prefixOverride;
  n := self;
  while n <> nil do begin
    if n.getAttributeTry(attrib, result, cmpFunction) then
      exit;
    n := n.getParent();
  end;
  case prefixOverride of
    ':xml': exit(XMLNamespaceUrl_XML);
  end;
  exit('');
end;

{$ImplicitExceptions on}

procedure TTreeNode.getOwnNamespaces(var list: TNamespaceList);
var attrib: TTreeAttribute;
begin
  if attributes <> nil then
    for attrib in attributes do
      if attrib.isNamespaceNode then
        list.addIfNewPrefixUrl(attrib.toNamespace);
  list.addIfNewPrefixUrl(namespace);
  if attributes <> nil then
    for attrib in attributes do
      if not attrib.isNamespaceNode then
        list.addIfNewPrefixUrl(attrib.namespace);
end;

procedure TTreeNode.getAllNamespaces(var list: TNamespaceList; first: boolean);
var attrib: TTreeAttribute;
begin
  if first then getOwnNamespaces(list)
  else begin
    if attributes <> nil then
      for attrib in attributes do
        if attrib.isNamespaceNode then
          list.addIfNewPrefix(attrib.toNamespace);
    //list.addIfNewPrefix(namespace); //implicit namespaces of the ancestor are not in scope, see XQTS cbcl-directconelem-001
   { if attributes <> nil then
      for attrib in attributes do
        if not attrib.isNamespaceNode then
          list.addIfNewPrefix(attrib.namespace);}
  end;
  if parent <> nil then parent.getAllNamespaces(list, false);
end;

function TTreeNode.isNamespaceUsed(const n: INamespace): boolean;
var attrib: TTreeAttribute;
  temp: TTreeNode;
begin
  if (namespace = nil) and (n = nil) then exit(true);
  if (namespace <> nil) and (n <> nil) and (namespace.getPrefix = n.getPrefix) then
    exit(namespace.getURL = n.getURL);
  if attributes <> nil then
    for attrib in attributes do begin
      if (attrib.namespace = nil) and (n = nil) and (attrib.value <> 'xmlns') then exit(true);
      if (attrib.namespace <> nil) and (n <> nil) and (attrib.namespace.getPrefix = n.getPrefix) then
        exit(attrib.namespace.getURL = n.getURL);
    end;
  temp := getFirstChild();
  while temp <> nil do begin
    if (temp.typ in [tetOpen, tetDocument]) and temp.isNamespaceUsed(n)  then exit(true);
    temp := temp.getNextSibling();
  end;
  result := false;
end;

function TTreeNode.isDeepEqual(cmpTo: TTreeNode; ignoredTypes: TTreeNodeTypes; cmpFunction: TStringComparisonFunc): boolean;
    function getAttributeCountExcludingNamespaceNodes(n: TTreeNode): integer;
    var a: TTreeAttribute;
    begin
      result := 0;
      if n.attributes = nil then exit();
      for a in n.attributes do
        if not a.isNamespaceNode then inc(result);
    end;

var
  attrib, tempattrib: TTreeAttribute;
  temp1, temp2: TTreeNode;
  ok: Boolean;
begin
  //this follows the XPath deep-equal function
  result := false;
  if typ <> cmpTo.typ then exit();
  if getNamespaceURL() <> cmpto.getNamespaceURL() then exit;
  case typ of
    tetAttribute:
      if    (value <> TTreeAttribute(cmpTo).value)
         or not cmpFunction(TTreeAttribute(self).realvalue, TTreeAttribute(cmpTo).realvalue) then exit;
    tetProcessingInstruction: if (value <> cmpto.value) or ( getAttribute('') <> cmpTo.getAttribute('')) then exit;
    tetOpen, tetDocument: begin
      if (value <> cmpTo.value) then exit;
      if getAttributeCountExcludingNamespaceNodes(self) <> getAttributeCountExcludingNamespaceNodes(cmpTo) then exit;
      if (attributes <> nil) and (cmpto.attributes <> nil) then
        for attrib in attributes do begin
          if attrib.isNamespaceNode then continue;
          ok := false;
          for tempattrib in cmpto.attributes do   //todo: optimize
            if attrib.isDeepEqual(tempattrib, ignoredTypes, cmpFunction) then begin
              ok :=  true;
              break;
            end;
          if not ok then exit;
        end;


      temp1 := next; temp2 := cmpTo.next;
      while (temp1 <> nil) and (temp1.typ in ignoredTypes) do temp1 := temp1.getNextSibling();
      while (temp2 <> nil) and (temp2.typ in ignoredTypes) do temp2 := temp2.getNextSibling();
      while (temp1 <> nil) and (temp1 <> reverse) and (temp2 <> nil) and (temp2 <> cmpTo.reverse) do begin
        if not temp1.isDeepEqual(temp2, ignoredTypes, cmpFunction) then exit;
        temp1 := temp1.getNextSibling();
        temp2 := temp2.getNextSibling();
        while (temp1 <> nil) and (temp1 <> reverse) and (temp1.typ in ignoredTypes) do temp1 := temp1.getNextSibling();
        while (temp2 <> nil) and (temp2 <> cmpto.reverse) and (temp2.typ in ignoredTypes) do temp2 := temp2.getNextSibling();
      end;
      if temp1 = reverse then temp1 := nil;
      if temp2 = cmpTo.reverse then temp2 := nil;
      if (temp1 <> nil) <> (temp2 <> nil) then exit;
    end;
    tetComment, tetText, tetClose:
      if not cmpFunction(value, cmpTo.value) then exit;
    else raise ETreeParseException.Create('Invalid node type');
  end;
  result := true;
end;

procedure TTreeNode.insert(el: TTreeNode);
begin
  // self  self.next  => self el self.next
  if self = nil then exit;
  el.next := self.next;
  self.next := el;
  el.offset := offset;
  el.previous:=self;
  if el.next <> nil then el.next.previous := el;
end;

procedure TTreeNode.insertSurrounding(before, after: TTreeNode);
var surroundee, prev: TTreeNode;
  el: TTreeNode;
begin
  if self = nil then exit;
  if self.typ = tetClose then surroundee := reverse
  else surroundee := self;
  prev := surroundee.getPrevious();
  if prev = nil then exit;

  prev.insert(before);

  if surroundee.typ in TreeNodesWithChildren then surroundee.reverse.insert(after)
  else surroundee.insert(after);

  before.reverse := after;
  after.reverse := before;

  if (before.typ in TreeNodesWithChildren) and (before.reverse = after) then begin
    prev := surroundee.getParent();
    el := surroundee;
    while (el <> nil) and (el.parent = prev) do begin
      el.parent := before;
      el := el.getNextSibling();
    end;
  end;
end;

procedure TTreeNode.insertSurrounding(basetag: TTreeNode);
var closing: TTreeNode;
begin
  if not (basetag.typ in TreeNodesWithChildren) then raise ETreeParseException.Create('Need an opening tag to surround another tag');
  closing := TTreeNode(basetag.ClassType.Create);
  closing.typ := tetClose;
  closing.value := basetag.value;
  insertSurrounding(basetag, closing);
end;

procedure TTreeNode.addAttribute(const aname, avalue: string; const anamespace: TNamespace = nil);
begin
  if attributes = nil then attributes := TAttributeList.Create;
  attributes.add(aname, avalue, anamespace);
  attributes.Items[attributes.count - 1].parent := self;
  attributes.Items[attributes.count - 1].offset := offset + 1;
  attributes.Items[attributes.count - 1].document := document;
end;

procedure TTreeNode.addAttributes(const props: array of THTMLProperty);
var
  i: Integer;
begin
  if length(props) = 0 then exit();
  if attributes = nil then attributes := TAttributeList.Create;
  attributes.Capacity:=attributes.count + length(props);
  for i := 0 to high(props) do begin
    attributes.add(strFromPchar(props[i].name, props[i].nameLen), strFromPchar(props[i].value, props[i].valueLen));
    attributes.Items[attributes.Count - 1].offset := offset + attributes.Count+1; //offset hack to sort attributes after their parent elements in result sequence
    attributes.Items[attributes.count - 1].parent := self;
    attributes.Items[attributes.count - 1].document := document;
  end;
end;

procedure TTreeNode.addNamespaceDeclaration(n: INamespace; overridens: boolean );
var a : TTreeAttribute;
begin
  if attributes = nil then attributes := TAttributeList.Create
  else for a in attributes do
    if (a.isNamespaceNode) and (a.toNamespace.getPrefix = n.getPrefix) then begin
      if overridens then a.realvalue:=n.getURL;
      exit;
    end;
  if n.getPrefix = '' then attributes.add('xmlns', n.getURL)
  else attributes.add(n.getPrefix, n.getURL, XMLNamespace_XMLNS);
end;

procedure TTreeNode.addChild(child: TTreeNode);
var
  oldprev: TTreeNode;
begin
  child.parent := self;
  child.document := document;
  oldprev := reverse.previous;
  oldprev.next := child;
  child.previous := oldprev;
  if child.reverse = nil then begin
    reverse.previous := child;
    child.next := reverse;
  end else begin
    reverse.previous := child.reverse;
    child.reverse.next := reverse;
    child.reverse.parent := self;
    child.reverse.document := document;
  end;
end;

procedure TTreeNode.removeElementFromDoubleLinkedList;
begin
  if previous <> nil then previous.next := next;
  if next <> nil then next.previous := nil;
end;

function TTreeNode.deleteElementFromDoubleLinkedList: TTreeNode;
begin
  result := next;
  removeElementFromDoubleLinkedList;
  free;
end;

function namespaceUsedByNodeOrChild(n: TTreeNode; const url, prefix: string): boolean;
var
  m: TTreeNode;
  attrib: TTreeAttribute;
begin
  m := n;
  while m <> n.reverse do begin
    if (m.namespace <> nil) and (m.namespace.getPrefix = prefix) and (m.namespace.getURL = url) then exit(true);
    if m.attributes <> nil then
       for attrib in m.attributes do
         if (attrib.namespace <> nil) and (attrib.namespace.getPrefix = prefix) and (attrib.namespace.getURL = url) then exit(true);
    m := m.next;
  end;
  result := false;
end;

function serializationWrapper(base: TTreeNode; nodeSelf: boolean; insertLineBreaks, html: boolean): string;
var known: TNamespaceList;
  encoding: TSystemCodePage;
  function requireNamespace(n: INamespace): string;
  begin //that function is useless the namespace should always be in known. But just for safety...
    if (n = nil) or (n.getURL = XMLNamespaceUrl_XML) or (n.getURL = XMLNamespaceUrl_XMLNS) or (known.hasNamespace(n)) then exit('');
    known.add(n);
    result := ' ' + n.serialize;
  end;

  function inner(n: TTreeNode): string; forward;

  function outer(n: TTreeNode): string;
    function attribEscape(const s: string): string; inline;
    begin
      if html then result := htmlStrEscape(s, true, encoding)
      else result := xmlStrEscape(s, true);
    end;

  var attrib: TTreeAttribute;
      oldnamespacecount: integer;
      i: Integer;
      temp: INamespace;
  begin
    with n do
    case typ of
      tetText:
        if not html then result := xmlStrEscape(value)
        else if (getParent() <> nil) and TTreeParser.htmlElementIsCDATA(getParent().value) then result := value
        else result := htmlStrEscape(value, false, encoding);
      tetClose: result := '</'+getNodeName()+'>';
      tetComment: result := '<!--'+value+'-->';
      tetProcessingInstruction: begin
        result := '<?'+value;
        if attributes <> nil then result += ' '+getAttribute('');
        result += '?>';
      end;
      tetOpen: begin
        oldnamespacecount:=known.Count;
        result := '<' + getNodeName();

        {
        writeln(stderr,'--');
         if attributes <> nil then
          for attrib in attributes do
            if attrib.isNamespaceNode then
             writeln(stderr, value+': '+attrib.toNamespace.serialize);
        }
        if oldnamespacecount = 0 then n.getAllNamespaces(known)
        else n.getOwnNamespaces(known);
        for i:=oldnamespacecount to known.Count - 1 do
          if (known.items[i].getURL <> '') or
             (known.hasNamespacePrefixBefore(known.items[i].getPrefix, oldnamespacecount)
                and (isNamespaceUsed(known.items[i])
                     or ((known.items[i].getPrefix = '') and (isNamespaceUsed(nil))))) then
            result += ' '+known.items[i].serialize;

        if namespace <> nil then result += requireNamespace(namespace)
        else if known.hasNamespacePrefix('', temp) then
          if temp.getURL <> '' then begin
            known.add(TNamespace.Make('', ''));
            result += ' xmlns=""';
          end;
        if attributes <> nil then
          for attrib in attributes do
            result += requireNamespace(attrib.namespace);


        if attributes <> nil then
          for attrib in attributes do
            if not attrib.isNamespaceNode then
              result += ' ' + attrib.getNodeName()+'="'+ attribEscape(attrib.realvalue)+'"';

        if (next = reverse) and (not html or (TTreeParser.htmlElementChildless(value))) then begin
          if html then result += '>'
          else result += '/>';
          if insertLineBreaks then Result+=LineEnding;
          while known.count > oldnamespacecount do
            known.Delete(known.count-1);
          exit();
        end;
        result +='>';
        if insertLineBreaks then Result+=LineEnding;
        result += inner(n);
        result+='</'+n.getNodeName()+'>';
        if insertLineBreaks then Result+=LineEnding;
        while known.count > oldnamespacecount do
          known.Delete(known.count-1);
      end;
      tetDocument: if html then result := innerHTML(insertLineBreaks) else result := innerXML(insertLineBreaks);
      else result := ''; //should not happen
    end;
  end;

  function inner(n: TTreeNode): string;
  var sub: TTreeNode;
  begin
    result := '';
    if not (n.typ in TreeNodesWithChildren) then exit;
    sub := n.next;
    while sub <> n.reverse do begin
      result += outer(sub);
      if not (sub.typ in TreeNodesWithChildren) then sub:=sub.next
      else if sub.reverse = nil then raise ETreeParseException.Create('Failed to serialize, no closing tag for '+sub.value)
      else sub := sub.reverse.next;
    end;
  end;
begin
  known := TNamespaceList.Create;
  encoding := CP_NONE;
  if base.document is TTreeDocument then encoding := TTreeDocument(base.document).FEncoding;
  if nodeSelf then result := outer(base)
  else result := inner(base);
  known.free;
end;

function TTreeNode.serializeXML(nodeSelf: boolean; insertLineBreaks: boolean): string;

begin
  if self = nil then exit('');
  result := serializationWrapper(self, nodeSelf, insertLineBreaks, false);
end;

function TTreeNode.serializeHTML(nodeSelf: boolean; insertLineBreaks: boolean): string;
begin
  if self = nil then exit('');
  result := serializationWrapper(self, nodeSelf, insertLineBreaks, true);
end;

function TTreeNode.cloneShallow(): TTreeNode;
begin
  case typ of
    tetAttribute: begin
      result := TTreeAttribute.create(value, TTreeAttribute(self).realvalue);
    end;
    tetDocument: begin
      result := TTreeDocument.create(TTreeDocument(self).FCreator);
      TTreeDocument(result).FEncoding:=TTreeDocument(self).FEncoding;
      TTreeDocument(result).FBaseURI:=TTreeDocument(self).FBaseURI;
      TTreeDocument(result).FDocumentURI:=TTreeDocument(self).FDocumentURI;
    end
    else begin
      result := TTreeNode(newinstance);
      result.create();
    end;
  end;
  result.Assign(self);
end;

procedure TTreeNode.assign(source: TTreeNode);
var
  result: TTreeNode;
begin
  result := self;
  with source do begin
    result.typ := typ;
    result.value := value;
    result.hash := hash;
    result.attributes := attributes;
    result.next := nil;
    result.previous := nil;
    result.parent := nil;
    result.reverse := nil;
    result.namespace := namespace;
    result.offset := offset;
  end;
end;


function TTreeNode.clone: TTreeNode;
var
  kid: TTreeNode;
begin
  case typ of
    tetOpen, tetDocument: begin
      result := cloneShallow;
      result.reverse := reverse.cloneShallow;
      result.reverse.reverse := result;
      result.next := result.reverse;
      result.reverse.previous := result;

      kid := getFirstChild();
      while kid <> nil do begin
        result.addChild(kid.clone);
        kid := kid.getNextSibling();
      end;

      if attributes <> nil then result.attributes := attributes.clone;
    end;
    tetProcessingInstruction: begin
      result := cloneShallow;
      if attributes <> nil then attributes := attributes.clone;
    end;
    tetText, tetComment, tetAttribute: result := cloneShallow;
    tetClose: raise ETreeParseException.Create('Cannot clone closing tag');
    else raise ETreeParseException.Create('Unknown tag');
  end;
  result.previous := nil;
  if result.reverse <> nil then Result.reverse.next := nil
  else result.next := next;
end;

procedure TTreeNode.removeAndFreeNext();
var
  toFree: TTreeNode;
  temp: TTreeNode;
begin
  if (self = nil) or (next = nil) then exit;
  toFree := next;
  if toFree.typ in TreeNodesWithChildren then begin
    temp := toFree.next;
    next := toFree.reverse.next;
    while temp <> toFree.next do begin //remove all between ]toFree, toFree.reverse] = ]toFree, toFree.next[
      temp.free;
      temp := temp.next;
    end;
  end else if toFree.typ = tetClose then
    raise ETreeParseException.Create('Cannot remove single closing tag')
  else
    next := toFree.next;
  next.previous := self;
  tofree.free;
end;

procedure TTreeNode.removeElementKeepChildren;
var
  temp: TTreeNode;
begin
  if previous = nil then raise ETreeParseException.Create('Cannot remove first tag');
  previous.next := next;
  next.previous := previous;
  if typ in TreeNodesWithChildren then begin
    temp := next;
    while temp <> reverse do begin
      if temp.parent = self then temp.parent := parent;
      temp := temp.getNextSibling();
    end;
    reverse.removeElementKeepChildren;
  end;
  free;
end;

function TTreeNode.caseInsensitiveCompare(const a, b: string): boolean;
begin
  result := striEqual(a, b);
end;

function TTreeNode.caseSensitiveCompare(const a, b: string): boolean;
begin
  result := a = b;
end;

function TTreeNode.toString(): string;
var
  attrib: TTreeAttribute;
begin
  if self = nil then exit('');
  case typ of
    tetText: exit(value);
    tetClose: exit('</'+value+'>');
    tetOpen: begin
      result := '<'+value;
      if attributes <> nil then
        for attrib in attributes do
          result += ' '+attrib.value + '="'+attrib.realvalue+'"';
      result+='>';
    end;
    tetDocument: result := innerXML();
    tetComment: exit('<!--'+value+'-->');
    else exit('??');
  end;
end;

function TTreeNode.toString(includeText: boolean; includeAttributes: array  of string): string;
var
  attrib: TTreeAttribute;
begin
  result := '';
  if self = nil then exit();
  case typ of
    tetText: if includeText then result := value;
    tetClose: result := '</'+value+'>';
    tetOpen: begin
      result := '<'+value;
      if attributes <> nil then
        for attrib in attributes do
          if arrayContainsI(includeAttributes, attrib.value) then
            result += ' '+attrib.value + '="'+attrib.realvalue+'"';
      result+='>';
    end;
    tetDocument: if getFirstChild() <> nil then result := getFirstChild().toString(includeText, includeAttributes);
    tetComment: if includeText then result := '<!--'+value+'-->';
    else result := '??';
  end;
end;

constructor TTreeNode.create();
begin
end;

constructor TTreeNode.create(atyp: TTreeNodeType; avalue: string);
begin
  self.typ := atyp;
  self.value := avalue;
  if avalue <> '' then self.hash := nodeNameHash(avalue);
end;

class function TTreeNode.createElementPair(const anodename: string): TTreeNode;
begin
  result := TTreeNodeClass(ClassType).create(tetOpen, anodename);
  result.hash := nodeNameHash(anodename);
  result.reverse := TTreeNodeClass(ClassType).create(tetClose, anodename);
  result.reverse.hash := nodeNameHash(anodename);
  result.reverse.reverse := result;
  result.next := Result.reverse;
  result.reverse.previous := Result;
end;

destructor TTreeNode.destroy();
begin
  if typ <> tetAttribute then attributes.Free;
  inherited destroy();
end;

procedure TTreeNode.initialized;
begin

end;

class function TTreeNode.compareInDocumentOrder(const a,b: TTreeNode): integer;
begin
  if a.document = b.document then begin
    result := a.offset - b.offset;
    if (result <> 0) or (a = b) then exit
    else begin
      if pointer(a) < pointer(b) then exit(-1)
      else exit(1);
    end;
  end;
  if pointer(a.document) < pointer(b.document) then exit(-1)
  else exit(1);
end;




{ THTMLTreeParser }

function TTreeParser.processingInstruction(text: pchar; textLen: longint; unusedParameter: TTextFlags): TParsingResult;
  function cutproperty(var remainingtext: string; out value: string): string;
  var
    eq: LongInt;
    closing: LongInt;
  begin
    result := '';
    eq := strIndexOf(remainingtext, '=');
    if eq <= 0 then exit;
    result := strTrim(copy(remainingtext, 1, eq - 1));
    remainingtext := strTrim(strCopyFrom(remainingtext, eq + 1));
    if (length(remainingtext) = 0) or not (remainingtext[1] in ['''', '"']) then exit('');
    closing := strIndexOf(remainingtext, remainingtext[1], 2);
    if closing <= 0 then exit('');
    value := copy(remainingtext, 2, closing - 1 - 1);
    remainingtext := strCopyFrom(remainingtext, closing+1);
  end;

var content: string;
    target: string;
    value: String;
    tempcontent: string;
    ws: Integer;
    i: Integer;
    new: TTreeNode;


begin
  result := prContinue;
  ws := -1;
  for i := 0 to textLen - 1do
    if (text + i)^ in WHITE_SPACE then begin
      ws := i;
      break;
    end;
  if ws < 0 then begin
    target := strFromPchar(text, textLen);
    content := '';
  end else begin
    target := strFromPchar(text, ws);
    inc(ws);
    //while ((text + ws)^ in WHITE_SPACE) and (ws < textLen)  do inc(ws); to trim or to not trim?
    content := strFromPchar(text + ws, textLen - ws);
  end;

  if target = 'xml' then begin
    tempcontent := content;
    while tempcontent <> '' do begin
      case cutproperty(tempcontent, value) of
        'encoding': begin
          case lowercase(value) of
            'utf-8': FXmlHeaderEncoding:=CP_UTF8;
            'windows-1252', 'iso-8859-1', 'iso-8859-15', 'latin1': FXmlHeaderEncoding:=CP_Windows1252;
          end;
        end;
        'standalone':
          if allowTextAtRootLevel and (parsingModel = pmStrict) then
            raise ETreeParseException.Create('External-preparsed-entity cannot be standalone');
        '': break;
      end;
    end;
    exit;
  end;
  if not FReadProcessingInstructions then exit;
  new := newTreeNode(tetProcessingInstruction, text, length(target));
  if content <> '' then new.addAttribute('', strNormalizeLineEndings(content));
  new.hash := nodeNameHashCheckASCII(new.value);
  new.initialized;
end;

function TTreeParser.newTreeNode(typ:TTreeNodeType; text: pchar; len: longint): TTreeNode;
begin
  result := newTreeNode(typ, strFromPchar(text, len), longint(text - @FCurrentFile[1]));
end;

function TTreeParser.newTreeNode(typ: TTreeNodeType; s: string; offset: integer): TTreeNode;
begin
  result:=treeNodeClass.Create;
  result.typ := typ;
  result.value := s;
  result.document := FCurrentTree;
  FTemplateCount+=1;

  if offset > FCurrentElement.offset then result.offset :=  offset
  else result.offset := FCurrentElement.offset + 1;

  FCurrentElement.next := result;
  result.previous := FCurrentElement;
  FCurrentElement := result;

  if typ <> tetClose then result.parent := TTreeNode(FElementStack.Last)
  else result.parent := TTreeNode(FElementStack.Last).getParent();

  if (parsingModel = pmHTML) then
    if (typ = tetClose) then
      FHasOpenedPTag := FHasOpenedPTag and not ((s = 'p') or (s = 'P'));

  //FCurrentElement.id:=FTemplateCount;
end;

procedure TTreeParser.autoCloseLastTag();
var
  last: TTreeNode;
  new: TTreeNode;
begin
  last := TTreeNode(FElementStack.Last);
  Assert(last<>nil);
  if last.typ = tetOpen then begin
    new := newTreeNode(tetClose, last.value, last.offset);
    new.hash := nodeNameHashCheckASCII(new.value);
    //new := treeElementClass.create();
    //new.typ:=tetClose;
    //new.value:=last.value;
    //new.offset:=last.offset;
    //new.next:=last.next;
    //last.next:=new;
    last.reverse:=new; new.reverse:=last;
  end;
  FElementStack.Delete(FElementStack.Count-1);
  FAutoCloseTag:=false;
end;

function TTreeParser.autoCloseTill(const ctag: string): TTreeNode;
var
  i: Integer;
  temp: TTreeNode;
  closeFrom: Integer;
begin
  result := nil;
  closeFrom := FElementStack.Count;
  for i := FElementStack.Count-1 downto 0 do begin
    temp :=TTreeNode(FElementStack[i]);
    if (temp.typ = tetOpen) and striEqual(temp.value, ctag) then begin
      closeFrom:=i;
      result := temp;;
      break;
    end;
  end;
  for i:=closeFrom to FElementStack.count-1 do
    autoCloseLastTag();
end;

function TTreeParser.prependTag(const tag: string): TTreeNode;
begin
  result := newTreeNode(tetOpen, tag, FCurrentElement.offset+1);
  result.hash := nodeNameHashCheckASCII(result.value);
  if result.parent <> nil then
    result.namespace := result.parent.namespace;
  FElementStack.Add(result);
  result.initialized;
end;

procedure TTreeParser.doRepairMissingStartTags(const tag: string);
  procedure goBack(t: TTreeNode);  //remove t.reverse
  var
    u: TTreeNode;
  begin
    if t = nil  then exit;
    if FCurrentElement = t.reverse then begin
      FCurrentElement := FCurrentElement.previous;
      FCurrentElement.next := nil;
    end else begin
      //t.reverse is somewhere in the middle => update doubly linked list
      u := t.reverse.previous;
      u.next := t.reverse.next;
      t.reverse.next.previous := u;

      //correct parents (no parent can be t.parent, since t will now still be open)
      u := u.next;
      while u <> nil do begin
        if u.parent = t.parent then
          u.parent := t;
        u := u.next;
      end;
    end;
    t.reverse.free;
    t.reverse := nil;
    FElementStack.Add(t);
  end;

var
  omittedTag: THTMLOmittedEndTagInfo;
begin
  omittedTag := omittedStartTags.find(tag);
  if (omittedTag <> nil) and (FElementStack.Count > 0) and striEqual(TTreeNode(FElementStack.Last).value, omittedTag.parents[0]) then begin
    prependTag(omittedTag.additionallyclosed[0]);
    exit;
  end;

  if (FBasicParsingState = bpmBeforeHtml) and not striEqual(tag, 'html') then begin
    flasthtml := prependTag('html');
    FBasicParsingState:=bpmBeforeHead;
  end;
  if (FBasicParsingState = bpmBeforeHead) and not striEqual(tag, 'head') then begin
    FBasicParsingState:=bpmInHead;
    FLastHead := prependTag('head');
  end;
  if (FBasicParsingState = bpmInHead) and not (
    striEqual(tag, 'base') or striEqual(tag, 'basefont') or striEqual(tag, 'bgsound') or striEqual(tag, 'link') or striEqual(tag, 'meta')
    or striEqual(tag, 'title') or striEqual(tag, 'title') or striEqual(tag, 'noscript') or striEqual(tag, 'noframes') or striEqual(tag, 'style')
    or striEqual(tag, 'script')) then begin
      autoCloseTill('head');
      FBasicParsingState:=bpmAfterHead;
    end;
  if FBasicParsingState = bpmAfterHead then begin
    if striEqual(tag, 'body') or striEqual(tag, 'frameset') then exit;
    if (FLastHead <> nil) and (striEqual(tag, 'base') or striEqual(tag, 'basefont') or striEqual(tag, 'bgsound') or striEqual(tag, 'link') or striEqual(tag, 'meta')
    or striEqual(tag, 'title') or striEqual(tag, 'title') or striEqual(tag, 'noscript') or striEqual(tag, 'noframes') or striEqual(tag, 'style')
    or striEqual(tag, 'script')
    or striEqual(tag, 'head')) then begin
      goBack(FLastHead);
      FBasicParsingState:=bpmInHead;
    end else begin
      flastbody := prependTag('body');
      FBasicParsingState:=bpmInBody;
    end;
  end;
  if FBasicParsingState = bpmAfterAfterBody then begin
    goBack(flasthtml);
    FBasicParsingState:=bpmAfterBody;
  end;
  if FBasicParsingState = bpmAfterBody then begin
    goBack(flastbody);
    FBasicParsingState:=bpmInBody;
  end;
  //if FBasicParsingState = bpmInBody, bpmInFrameset..;
  end;

function TTreeParser.enterTag(tagName: pchar; tagNameLen: longint;
  properties: THTMLProperties): TParsingResult;
var
  tag: String;



  procedure doRepairMissingEndTags;
  var
    omittedTag: THTMLOmittedEndTagInfo;
    specialCaseOptGroup: Boolean;
    closeFrom: Integer;
    temp: TTreeNode;
    i: Integer;
  begin
    omittedTag := omittedEndTags.find(tag);
    if omittedTag <> nil then begin
      specialCaseOptGroup := striEqual(tag, 'optgroup');
      closeFrom := FElementStack.Count;
      for i := FElementStack.Count-1 downto 0 do begin
        temp :=TTreeNode(FElementStack[i]);
        if not (temp.typ in [tetOpen, tetClose] {is there ever a close?}) then continue;
        if arrayContainsI(omittedTag.parents, temp.value) then break;
        if arrayContainsI(omittedTag.siblings, temp.value) or arrayContainsI(omittedTag.additionallyclosed, temp.value) then
          closeFrom := i;
      end;
      for i:=closeFrom to FElementStack.count-1 do
        autoCloseLastTag();
    end;

    if FHasOpenedPTag and htmlElementClosesPTag(tag) then
      autoCloseTill('p')
  end;


var
  new: TTreeNode;
  attrib: TTreeAttribute;
begin
  result:=prContinue;

  if tagName^ = '?' then begin //processing instruction
    if length(properties) > 0 then tagNameLen := properties[high(properties)].value + properties[high(properties)].valueLen - tagName
    else tagNameLen -= 1;
    processingInstruction(tagName+1, tagNameLen, []);
    exit;
  end;

  if FAutoCloseTag then autoCloseLastTag();

  tag := strFromPchar(tagName, tagNameLen);
  if (FParsingModel = pmHTML) then begin
    if (FBasicParsingState = bpmInBody) and repairMissingStartTags and striEqual(tag, 'body') then exit;
    if repairMissingEndTags then doRepairMissingEndTags;
    if repairMissingStartTags then doRepairMissingStartTags(tag)
    else if striEqual(tag, 'body') then FBasicParsingState:=bpmInBody;

    FHasOpenedPTag := FHasOpenedPTag or (tag = 'p') or (tag = 'P');
  end;


  if (FParsingModel = pmHTML) then begin//normal auto close
    case FBasicParsingState of
      bpmBeforeHtml: if striEqual(tag, 'html') then FBasicParsingState:=bpmBeforeHead;
      bpmBeforeHead: if striEqual(tag, 'head') then FBasicParsingState:=bpmInHead;
      bpmInHead: if striEqual(tag, 'head') then exit; //skip
      bpmAfterHead:
        if striEqual(tag, 'body') then FBasicParsingState:=bpmInBody
        else if striEqual(tag, 'frameset') then FBasicParsingState:=bpmInFrameset;
      bpmInBody: if repairMissingStartTags and (striEqual(tag, 'body') or striEqual(tag, 'html') or striEqual(tag, 'head')) then exit; //skip
      bpmInFrameset: if repairMissingStartTags and (striEqual(tag, 'frameset') or striEqual(tag, 'html') or striEqual(tag, 'head')) then exit; //skip
    end;
    FAutoCloseTag:=htmlElementChildless(tag);
    if striEqual(tag, 'table') and (FElementStack.Count > 0) and striEqual(TTreeNode(FElementStack.Last).value, 'table') then
      leaveTag(tagName, tagNameLen);
  end;
  new := newTreeNode(tetOpen, tag, longint(tagName - @FCurrentFile[1]));

  FElementStack.Add(new);
  if length(properties)>0 then begin
    new.addAttributes(properties);
    for attrib in new.attributes do
      if strBeginsWith(attrib.value, 'xmlns') then begin
        if attrib.value = 'xmlns' then
           pushNamespace(attrib.realvalue, '')
         else if strBeginsWith(attrib.value, 'xmlns:') then begin
           attrib.value:=strCopyFrom(attrib.value, 7);
           pushNamespace(attrib.realvalue, attrib.value);
           attrib.namespace := XMLNamespace_XMLNS;
         end;
      end;
    for attrib in new.attributes do begin
      if pos(':', attrib.value) > 0 then
        attrib.namespace := findNamespace(strSplitGet(':', attrib.value));
      attrib.hash := nodeNameHashCheckASCII(attrib.value);
    end;
    if (FParsingModel = pmHTML) and (striEqual(tag, 'base')) and (FCurrentTree.baseURI = '') and new.hasAttribute('href') then
      FCurrentTree.baseURI := strResolveURI(new.getAttribute('href'), FCurrentTree.documentURI);
  end;
  if (pos(':', new.value) > 0) then begin
    new.namespace := findNamespace(strSplitGet(':', new.value))
  end else
    new.namespace := FCurrentNamespace;;
  if new.value = '' then
    if parsingModel = pmStrict then raise ETreeParseException.Create('Invalid node with empty name: '+strFromPchar(tagName, tagNameLen))
    else begin
      new.value:= 'x';// '<' +  strFromPchar(tagName, tagNameLen);
      //new.typ :=tetText;
      if pos(':', strFromPchar(tagName, tagNameLen)) > 0 then
        new.value:=strFromPchar(tagName, tagNameLen);
    end;
  new.hash := nodeNameHashCheckASCII(new.value);

  new.initialized;
end;

function TTreeParser.leaveTag(tagName: pchar; tagNameLen: longint): TParsingResult;
var
  new,last,temp: TTreeNode;
  match: longint;
  i: Integer;
  weight: LongInt;
  parenDelta: integer;
  name: String;
  removedCurrentNamespace: Boolean;
begin
  result:=prContinue;

  last := TTreeNode(FElementStack.Last);
  if (FParsingModel = pmStrict) and (last = nil) then
    raise ETreeParseException.create('The tag <'+strFromPchar(tagName,tagNameLen)+'> was closed, but none was open');

  if (tagNameLen = 0) then
    if  (tagName <> nil) then exit  //skip tags like </ br>
    else tagName := pchar(''); //but allow (nil, 0) call to close all still open tags

  if last = nil then exit;

  if FAutoCloseTag and (not strliequal(tagName, last.value, tagNameLen)) then autoCloseLastTag();
  FAutoCloseTag:=false;

  if (FParsingModel = pmHTML) and repairMissingStartTags then begin
    if (strliEqual(tagName, 'html', tagNameLen) or strliEqual(tagName, '', tagNameLen) ) then begin
      case FBasicParsingState of
        bpmBeforeHead: begin
          prependTag('head'); autoCloseLastTag();
          last := prependTag('body');
          FBasicParsingState:=bpmInBody;
        end;
        bpmInHead: begin
          autoCloseLastTag();
          last := prependTag('body');
          FBasicParsingState:=bpmInBody;
        end;
        bpmAfterHead: begin
          last := prependTag('body');
          FBasicParsingState:=bpmInBody;
        end;
        bpmAfterBody: begin
          FBasicParsingState:=bpmAfterAfterBody;
        end;
      end;
    end else if (FBasicParsingState = bpmInBody) and strliEqual(tagname, 'body', tagNameLen) then begin
      FBasicParsingState:=bpmAfterBody;
    end;
  end;
  new := nil;
  if (strliequal(tagName, last.getNodeName, tagNameLen)) then begin
    new := newTreeNode(tetClose, tagName, tagNameLen);
    new.hash := nodeNameHashCheckASCII(new.value);
    new.reverse := last; last.reverse := new;
    FElementStack.Delete(FElementStack.Count-1);
    new.initialized;
  end else if FParsingModel = pmStrict then
    raise ETreeParseException.Create('The tag <'+strFromPchar(tagName,tagNameLen)+'> was closed, but the latest opened was <'+last.value+'>  (url: '+FCurrentTree.FBaseURI+')')
  else if FParsingModel in [pmHTML, pmUnstrictXML] then begin
    //try to auto detect unclosed tags
    match:=-1;
    for i:=FElementStack.Count-1 downto 0 do
      if strliequal(tagName, TTreeNode(FElementStack[i]).value, tagNameLen) then begin
        match:=i;
        break;
      end;
    if match > -1 then begin
      //there are unclosed tags, but a tag opening the currently closed exist, close all in between
      if FParsingModel = pmHTML then begin
        weight := htmlTagWeight(strFromPchar(tagName, tagNameLen));
        for i:=match+1 to FElementStack.Count-1 do
          if htmlTagWeight(TTreeNode(FElementStack[i]).value) > weight then
              exit;
      end;
      for i:=match+1 to FElementStack.Count-1 do
        autoCloseLastTag();
      new := newTreeNode(tetClose, tagName, tagNameLen);
      new.hash := nodeNameHashCheckASCII(new.value);
      last := TTreeNode(FElementStack[match]);
      last.reverse := new; new.reverse := last;
      FElementStack.Count:=match;
      new.initialized;
    end;

    name := strFromPchar(tagName, tagNameLen);
    if (FParsingModel = pmHTML) and htmlElementChildless(name) then begin
      parenDelta := 0;
      last := FCurrentElement;
      weight := htmlTagWeight(strFromPchar(tagName, tagNameLen));
      while last <> nil do begin
        if last.typ = tetClose then parenDelta -= 1
        else if (last.typ = tetOpen) then begin
          if htmlTagWeight(last.value) > weight then break;//this will still crash with same weight elements
          parenDelta+=1;
          if (last.value = name) then begin
            if (last.reverse <> last.next) or (parenDelta <> 0) then break; //do not allow nested auto closed elements (reasonable?)
            //remove old closing tag, and insert new one at the end
            new := newTreeNode(tetClose, tagName, tagNameLen);
            new.hash := nodeNameHashCheckASCII(new.value);
            last.reverse.removeElementKeepChildren;
            last.reverse := new; new.reverse := last;

            new.parent := last.parent;

            //update parents
            temp := last.getFirstChild();
            while (temp <> nil) and (last <> new) do begin
              if temp.parent = last.parent then temp.parent := last;
              if (temp.typ in [tetOpen, tetDocument]) and (temp.reverse = nil) then break;
              temp := temp.getNextSibling();
            end;
            break;
          end;
        end;
        last := last.previous;
      end;
    end;

    //if no opening tag can be found the closing tag is ignored (not contained in tree)
  end;

  if new = nil then exit;

  if pos(':', new.value) > 0 then new.namespace := findNamespace(strSplitGet(':', new.value))
  else new.namespace := FCurrentNamespace;
  removedCurrentNamespace := false;
  while (FCurrentNamespaceDefinitions.Count > 0) and (FCurrentNamespaceDefinitions[FCurrentNamespaceDefinitions.Count-1] = pointer(new.reverse)) do begin
    if FCurrentNamespaces.items[FCurrentNamespaces.Count - 1].getPrefix = '' then
      removedCurrentNamespace := true;
    FCurrentNamespaceDefinitions.Delete(FCurrentNamespaceDefinitions.Count-1);
    FCurrentNamespaces.Delete(FCurrentNamespaces.Count-1);
  end;
  if removedCurrentNamespace then
    FCurrentNamespace := findNamespace('');


  if (FParsingModel = pmHTML) then begin
    if (FBasicParsingState = bpmInHead) and strliEqual(tagName, 'head', tagNameLen)  then begin
      FBasicParsingState:=bpmAfterHead;
      FLastHead := new.reverse;
    end else if (FBasicParsingState in [bpmInBody, bpmAfterBody]) and strliEqual(tagName, 'body', tagNameLen) then begin
      FBasicParsingState:=bpmAfterBody;
      if striEqual(new.value, 'body') then flastbody := new.reverse;
    end else if (FBasicParsingState in [bpmInBody, bpmAfterBody, bpmAfterAfterBody]) and strliEqual(tagName, 'html', tagNameLen) then begin
      FBasicParsingState:=bpmAfterAfterBody;
      if striEqual(new.value, 'html') then flasthtml := new.reverse;
      if flastbody = nil then begin
        flastbody := flasthtml.previous;
        while (flastbody <> nil) and (flastbody.typ <> tetClose) do flastbody := flastbody.previous;
      end;
    end;
  end;
end;

function TTreeParser.readText(text: pchar; textLen: longint; tf: TTextFlags): TParsingResult;
var
  tempLen: LongInt;
  temp: PChar;
  typ: TTreeNodeType;
begin
  result:=prContinue;

  if not allowTextAtRootLevel then
    if (FParsingModel = pmStrict) and (FElementStack.Count < 2)  then begin
      strlTrimLeft(text, textLen);
      if textLen = 0 then exit;
      if strBeginsWith(text, #239#187#191) or strBeginsWith(text,#254#255) or strBeginsWith(text, #255#254) or
         strBeginsWith(text, #43#47#118) then raise ETreeParseException.Create('xml ' + FCurrentTree.FBaseURI + ' starts with unicode BOM. That is not supported');
      raise ETreeParseException.Create('Data not allowed at root level: '+strFromPchar(text,textLen));
    end;

  if FAutoCloseTag then
    autoCloseLastTag();

  if FTrimText then
    strlTrim(text, textLen, [' ',#0,#9,#10,#13]);

  if textLen = 0 then
    exit;

  if (FParsingModel = pmHTML) and repairMissingStartTags
     and ( (FBasicParsingState in [bpmAfterBody, bpmAfterAfterBody])
           or (not (FBasicParsingState in [bpmInBody, bpmInFrameset])
               and ((FElementStack.Count = 0) or striEqual(TTreeNode(FElementStack.Last).value, 'head')
                     or striEqual(TTreeNode(FElementStack.Last).value, 'html') or striEqual(TTreeNode(FElementStack.Last).value, ''))) )
         then begin
    tempLen := textLen;
    temp := text;
    strlTrim(temp, tempLen, [#0..' ']);
    if tempLen > 0 then doRepairMissingStartTags('');
  end;


  typ := tetText;
  if tfCDATA in tf then typ := tetInternalDoNotUseCDATAText;
  newTreeNode(typ, text, textLen).initialized;
end;

function TTreeParser.readComment(text: pchar; textLen: longint): TParsingResult;
begin
  result:=prContinue;
  if not FReadComments then
    exit;
  if textLen <= 0 then
    exit;
  newTreeNode(tetComment, text, textLen).initialized;
end;

procedure TTreeParser.pushNamespace(const url, prefix: string);
var
  ns: INamespace;
  nsurl: string;
begin
  nsurl := strChangeEncoding(url, FXmlHeaderEncoding, FTargetEncoding);
  nsurl := strDecodeHTMLEntities(nsurl, FTargetEncoding, false);
  nsurl := xmlStrWhitespaceCollapse(nsurl);

  ns := TNamespace.Make(nsurl, prefix);
  FCurrentNamespaces.Add(ns);
  FCurrentNamespaceDefinitions.Add(FCurrentElement);
  FCurrentAndPreviousNamespaces.Add(ns);
  if prefix = '' then FCurrentNamespace := ns;
end;

function TTreeParser.findNamespace(const prefix: string): INamespace;
begin
  result := nil;
  if FCurrentNamespaces.hasNamespacePrefix(prefix, result) then exit;
  if globalNamespaces.hasNamespacePrefix(prefix, result) then exit;
  case prefix of
    'xml': result := XMLNamespace_XML;
    'xmlns': result := XMLNamespace_XMLNS;
    '': result := FCurrentNamespace;
    else if parsingModel = pmStrict then raise ETreeParseException.Create('Unknown namespace in XML file: '+prefix);
  end;
end;

function TTreeParser.htmlTagWeight(s: string): integer;
begin
  result := 0;
  //feather tags "b br em hr i p"
  if striequal(s, 'b') or striequal(s, 'em') or
     striequal(s, 'em') or striequal(s, 'p') or striequal(s, 'i') or
     striequal(s, 'o') then exit(-1);

  //middle weight  a 'applet'     'area'    'caption'    'center'     'form'    'h1'    'h2'    'h3'    'h4'    'h5'    'h6'    'iframe'    'input'    'span'
  if striequal(s, 'span') then exit(1);

  //heavy weight 'bod y' 'code' 'dd' 'dl' 'div' 'dt' 'fieldset' 'head' 'html' 'li' 'menu'  'table' 'td' 'tr' 'ul'
  if striequal(s, 'code') or  striequal(s,'fieldset') or striequal(s,'head') or striequal(s,'menu') then exit(2);
  if striequal(s, 'ul') or striequal(s, 'ol') or striequal(s, 'dd') or striequal(s, 'dt') then exit(3);
  if striequal(s, 'li') or striequal(s, 'dl') then exit(4);
  if striequal(s, 'div') then exit(5);

  //super heavy tables
  if striequal(s, 'td') then exit(6);
  if striequal(s, 'tr') then exit(7);
  if striequal(s, 'body') or striequal(s, 'html') or striequal(s, 'table') then exit(8);

  if striequal(s, '') then exit(100); //force closing of root element
end;

class function TTreeParser.htmlElementChildless(const s: string): boolean;
begin
  //elements that should/must not have children
  //area, base, basefont, bgsound, br, col, command, embed, frame, hr, img, input, keygen, link, meta, param, source, track or wbr
  //Regex ([a-z]+),  => striequal(s,'\1') or
  if s = '' then exit(false);
  result:=striequal(s,'area') or striequal(s,'base') or striequal(s,'basefont') or striequal(s,'bgsound') or striequal(s,'br') or striequal(s,'col')
          or striequal(s,'command') or striequal(s,'embed') or striequal(s,'frame') or striequal(s,'hr') or striequal(s,'img') or striequal(s,'input')
          or striequal(s,'keygen') or striequal(s,'link') or striequal(s,'meta') or striequal(s,'param') or striequal(s,'source') or striequal(s,'track')
          or striequal(s,'wbr');

  //elements listed above, not being void are probably (??) deprecated?
  //void elements: area, base, br, col, command, embed, hr, img, input, keygen, link, meta, param, source, track, wbr

end;

class function TTreeParser.htmlElementIsCDATA(const s: string): boolean;
begin
  result := simplehtmlparser.htmlElementIsCDATA(pchar(s), length(s));
end;

class function TTreeParser.htmlElementClosesPTag(const s: string): boolean;
begin
  if s = '' then exit(false);
  case s[1] of
    'a', 'A': result := striequal(s, 'address') or striequal(s, 'article') or striequal(s, 'aside');
    'b', 'B': result := striequal(s, 'blockquote');
    'd', 'D': result := striequal(s, 'dir') or striequal(s, 'div') or striequal(s, 'dl');
    'f', 'F': result := striequal(s, 'fieldset') or striequal(s, 'footer') or striequal(s, 'form');
    'h', 'H': result := ((length(s) = 2) and (s[2] >= '1') and (s[2] <= '6') {h1,h2,...,h6}) or striequal(s, 'header') or striequal(s, 'hgroup') or striequal(s, 'hr');
    'm', 'M': result := striequal(s, 'menu');
    'n', 'N': result := striequal(s, 'nav');
    'o', 'O': result := striequal(s, 'ol');
    'p', 'P': result := striequal(s, 'p') or striequal(s, 'pre');
    's', 'S': result := striequal(s, 'section');
    't', 'T': result := striequal(s, 'table');
    'u', 'U': result := striequal(s, 'ul');
    else result := false;
  end;
end;

constructor TTreeParser.Create;
begin
  FElementStack := TList.Create;
  treeNodeClass := TTreeNode;
  FTrimText:=false;
  FReadComments:=false;
  FReadProcessingInstructions:=false;
  FAutoDetectHTMLEncoding:=true;
  FTrees := TList.Create;

  FCurrentNamespaceDefinitions := TList.Create;
  FCurrentNamespaces := TNamespaceList.Create;
  FCurrentAndPreviousNamespaces := TNamespaceList.Create;
  globalNamespaces := TNamespaceList.Create;
  FTargetEncoding:=CP_UTF8;

  FRepairMissingStartTags:=false; //??
  FRepairMissingEndTags:=true;
  //FConvertEntities := true;
end;

destructor TTreeParser.destroy;
begin
  clearTrees;
  FElementStack.free;
  ftrees.Free;
  FCurrentNamespaces.Free;
  FCurrentNamespaceDefinitions.Free;
  FCurrentAndPreviousNamespaces.free;
  globalNamespaces.free;
  inherited destroy;
end;

procedure TTreeParser.clearTrees;
var
  i: Integer;
begin
  for i:=0 to FTrees.Count-1 do
    TTreeDocument(FTrees[i]).deleteAll();
  ftrees.Clear;
end;


//like in TeXstudio
function isInvalidUTF8(const s: string): boolean;
var
  prev, cur: Integer;
  good, bad: Integer;
  i: Integer;
begin
  prev := 0;
  good := 0;
  bad := 0;
  for i := 1 to length(s) do begin
    cur := ord(s[i]);
    if (cur and $C0) = $80 then begin
      if (prev and $C0) = $C0 then good += 1
      else if (prev and $80) = $00 then bad += 1;
    end else begin
      if (prev and $C0) = $C0 then bad+=1
    end;
    prev := cur;
  end;
  result := good < 10 * bad;
end;


function TTreeParser.parseTree(html: string; uri: string; contentType: string): TTreeDocument;


var
  el: TTreeNode;
  attrib: TTreeAttribute;
  encMeta, encBOM: TSystemCodePage;
begin
  FTemplateCount:=0;
  FElementStack.Clear;
  FCurrentTree:=nil;
  FCurrentNamespaces.clear;
  FCurrentNamespaceDefinitions.Clear;
  FCurrentAndPreviousNamespaces.Clear;

  allowTextAtRootLevel := strBeginsWith(contentType, 'text/xml-external-parsed-entity'); //todo: should it detect parsing mode from content-type? so far it does not. but parsed entity is so rarely used, it does not matter

  //FVariables.clear;
  if (html='') and not allowTextAtRootLevel then exit(nil);

  FCurrentFile:=html;
  FAutoCloseTag:=false;
  FCurrentNamespace := nil;


  //initialize document node
  FCurrentTree:=TTreeDocument.create(self);
  FCurrentTree.FCreator:=self;
  FCurrentTree.typ := tetDocument;
  FCurrentTree.FBaseURI:=uri;
  FCurrentTree.FDocumentURI:=uri;
  FCurrentTree.document := FCurrentTree;
  FCurrentElement:=FCurrentTree;
  FElementStack.Clear;
  FElementStack.Add(FCurrentElement);
  FTemplateCount:=1;
  FHasOpenedPTag := false;
  FBasicParsingState:=bpmBeforeHtml;
  FLastHead := nil;
  flastbody := nil;
  flasthtml := nil;

  encBOM := strEncodingFromBOMRemove(FCurrentFile);
  if encBOM = CP_NONE then
    encBOM := strEncodingFromContentType(contentType);
  case encBOM of
    CP_UTF8, CP_Windows1252, CP_NONE: ;
    else begin //do no want to handle multi-byte chars
      FCurrentFile := strConvertToUtf8(FCurrentFile, encBOM);
      encBOM := CP_UTF8;
    end;
  end;
  FXmlHeaderEncoding := encBOM;



  //parse
  if FParsingModel = pmHTML then simplehtmlparser.parseHTML(FCurrentFile,@enterTag, @leaveTag, @readText, @readComment)
  else simplehtmlparser.parseML(FCurrentFile,[poRespectProcessingInstructions],@enterTag, @leaveTag, @readText, @readComment, @processingInstruction);

  //close root element
  leaveTag(nil,0);

  if FAutoDetectHTMLEncoding  then begin
    FCurrentTree.FEncoding:=CP_NONE;
    if (encBOM = CP_NONE) and (parsingModel = pmHTML) then
      encMeta := strEncodingFromContentType(TXQueryEngine.evaluateStaticXPath2('html/head/meta[@http-equiv=''content-type'']/@content', FCurrentTree).toString)
     else
      encMeta := encBOM;

    if encBOM = CP_NONE then encBOM := FXmlHeaderEncoding;
    if encBOM = CP_NONE then encBOM := encMeta;
    if encMeta  = CP_NONE then encMeta := encBOM;
    if FXmlHeaderEncoding = CP_NONE then FXmlHeaderEncoding := encBOM;
    if (encMeta = encBOM) and (encMeta = FXmlHeaderEncoding) and (encMeta <> CP_NONE) then
      FCurrentTree.FEncoding := encMeta
    else begin //if in doubt, detect encoding and ignore meta/header data
      FCurrentTree.FEncoding:=CP_UTF8;
      el := FCurrentTree.next;
      while el <> nil do begin
        case el.typ of
          tetText, tetInternalDoNotUseCDATAText: if isInvalidUTF8(el.value) then begin
            FCurrentTree.FEncoding:=CP_WINDOWS1252;
            break;
          end;
          tetOpen, tetDocument: if el.attributes <> nil then begin
            for attrib in el.attributes do
              if isInvalidUTF8(attrib.value) or isInvalidUTF8(attrib.realvalue) then begin
                FCurrentTree.FEncoding:=CP_WINDOWS1252;
                break;
              end;
            if FCurrentTree.FEncoding <> CP_UTF8 then break;
          end;
        end;
        el := el.next;
      end;
    end;

  end;

  FTrees.Add(FCurrentTree);
  result := FCurrentTree;
  if FTargetEncoding <> CP_NONE then begin
    FCurrentTree.setEncoding(FTargetEncoding, true, true);
  end else begin
     el := FCurrentTree.next;
     while el <> nil do begin
       if el.typ = tetInternalDoNotUseCDATAText then el.typ := tetText;
       el := el.next;
     end;
   end;
//  if FRootElement = nil then
//    raise ETemplateParseException.Create('Ungültiges/Leeres Template');
end;

function TTreeParser.parseTreeFromFile(filename: string): TTreeDocument;
var
  absfilename: String;
begin
  absfilename :=  fileNameExpand(filename);
  result := parseTree(strLoadFromFile(filename), absfilename);
end;

function TTreeParser.getLastTree: TTreeDocument;
begin
  if FTrees.Count = 0 then exit(nil);
  result := TTreeDocument(FTrees[FTrees.Count-1]);
end;

procedure TTreeParser.removeEmptyTextNodes(const whenTrimmed: boolean);
  function strIsEmpty(const s: string): boolean;
  var p: pchar; l: longint;
  begin
    p := pointer(s);
    l := length(s);
    strlTrimLeft(p, l);
    result := l = 0;
  end;

var
  temp: TTreeNode;
begin
  temp := getLastTree;
  if temp = nil then exit;
  while temp.next <> nil do begin
    while (temp.next <> nil) and (temp.next.typ = tetText) and ( (temp.next.value = '') or (whenTrimmed and (strIsEmpty(temp.next.value)))) do
      temp.removeAndFreeNext();
    temp := temp.next;
  end;
end;


function xmlStrEscape(s: string; attrib: boolean = false):string;
var
  i: Integer;
  builder: TStrBuilder;

begin
  builder.init(@result, length(s));
  i := 1;
  while i <= length(s) do begin
    case s[i] of
      '<': builder.add('&lt;');
      '>': builder.add('&gt;');
      '&': builder.add('&amp;');
      '''': builder.add('&apos;');
      '"': builder.add('&quot;');
      #13: builder.add('&#xD;');
      #10: if attrib then builder.add('&#xA;') else builder.add(#10);
      #9: if attrib then builder.add('&#x9;') else builder.add(#9);
      #0..#8,#11,#12,#14..#$1F,#$7F: builder.addhexentity(ord(s[i]));
      #$C2: if (i = length(s)) or not (s[i+1] in [#$80..#$9F]) then builder.add(#$C2) else begin
        i+=1;
        builder.addhexentity(ord(s[i]));
      end;
      #$E2: if (i + 2 > length(s)) or (s[i+1] <> #$80) or (s[i+2] <> #$A8) then builder.add(#$E2) else begin
        builder.add('&#x2028;');
        i+=2;
      end;
      else builder.add(s[i]);
    end;
    i+=1;
  end;
  builder.final;
end;

function xmlStrWhitespaceCollapse(const s: string): string;
begin
  result := strTrimAndNormalize(s, [#9,#$A,#$D,' ']);
end;

function htmlStrEscape(s: string; attrib: boolean; encoding: TSystemCodePage): string;
var
  i: Integer;
  builder: TStrBuilder;

begin
  builder.init(@result, length(s));
  i := 1;
  if attrib then begin
    while i <= length(s) do begin
      case s[i] of
        '&': builder.add('&amp;');
        '"': builder.add('&quot;');
        #$A0: if encoding = CP_WINDOWS1252 then builder.add('&nbsp;') else builder.add(s[i]);
        #$C2: if (encoding = CP_UTF8) and (i+1 <= length(s)) and (s[i+1] = #$A0) then begin builder.add('&nbsp;'); i+=1; end else builder.add(s[i]);
        //#0..#8,#11,#12,#14..#$1F,#$7F: builder.addhexentity(ord(s[i])); not needed?
        else builder.add(s[i]);
      end;
      i+=1;
    end
  end else begin
    while i <= length(s) do begin
      case s[i] of
        '&': builder.add('&amp;');
        '<': builder.add('&lt;');
        '>': builder.add('&gt;');
        #$A0: if encoding = CP_WINDOWS1252 then builder.add('&nbsp;') else builder.add(s[i]);
        #$C2: if (encoding = CP_UTF8) and (i+1 <= length(s)) and (s[i+1] = #$A0) then begin builder.add('&nbsp;'); i+=1; end  else builder.add(s[i]);
        //#0..#8,#11,#12,#14..#$1F,#$7F: builder.addhexentity(ord(s[i]));
        else builder.add(s[i]);
      end;
      i+=1;
    end;
  end;
  builder.final;
end;

function equalNamespaces(const ans, bns: INamespace): boolean;
begin
  result := (ans = bns) or ((ans <> nil) and (bns <> nil) and strEqual(ans.getURL, bns.getURL));
end;

function equalNamespaces(const ans, bns: string): boolean;
begin
  result := strEqual(ans, bns);
end;

function namespaceGetURL(const n: INamespace): string;
begin
  if n = nil then result := ''
  else result := n.getURL;
end;

function guessFormat(const data, uri, contenttype: string): TInternetToolsFormat;
var
  tdata: PChar;
  tdatalength: Integer;
  function checkRawDataForHtml: boolean; //following http://mimesniff.spec.whatwg.org/ (except allowing #9 as TT ) todo: what is with utf-16?
  var tocheck: array[1..16] of string = ('<!DOCTYPE HTML', '<HTML', '<HEAD', '<SCRIPT', '<IFRAME', '<H1', '<DIV', '<FONT', '<TABLE', '<A', '<STYLE', '<TITLE', '<B', '<BODY', '<BR', '<P');
    i: Integer;
  begin
    for i := low(tocheck) to high(tocheck) do
      if (tdatalength > length(tocheck[i])) and
         (tdata[length(tocheck[i])] in [' ', '>', #9]) and
         (striBeginsWith(tdata, tocheck[i])) then
        exit(true);
    exit(false);
  end;

begin
  tdata := pchar(data);
  tdatalength := length(data);
  strlTrim(tdata, tdatalength);
  if striEndsWith(uri, 'html') or striEndsWith(uri, 'htm')
     or striContains(contenttype, 'html')
     or checkRawDataForHtml() then
      Result := itfHTML
    else if strBeginsWith(tdata, '<?xml') //mimesniff.spec says to check for this
            or striContains(contenttype, 'xml') then
      result := itfXML
    else if striEndsWith(uri, '.json')
         or striContains(contentType, 'json')
         or strBeginsWith(tdata, '{')
         or strBeginsWith(tdata, '[') then
      result := itfJSON
    else
      result := itfXML;
end;

function strEncodingFromContentType(const contenttype: string): TSystemCodePage;
var
  encoding: String;
begin
  encoding := lowercase(contenttype);
  if pos('charset=utf-8', encoding) > 0 then exit(CP_UTF8);
  if (pos('charset=windows-1252',encoding) > 0) or
     (pos('charset=latin1',encoding) > 0) or
     (pos('charset=iso-8859-1',encoding) > 0) then //also -15
      exit(CP_Windows1252);
  if (pos('charset=utf-16le',encoding) > 0) then exit(CP_UTF16);
  if (pos('charset=utf-16be',encoding) > 0) then exit(CP_UTF16BE);
  if (pos('charset=utf-16',encoding) > 0) then exit({$IFDEF ENDIAN_BIG}CP_UTF16BE{$ELSE}CP_UTF16{$ENDIF});
  if (pos('charset=utf-32le',encoding) > 0) then exit(CP_UTF32);
  if (pos('charset=utf-32be',encoding) > 0) then exit(CP_UTF32BE);
  if (pos('charset=utf-32',encoding) > 0) then exit({$IFDEF ENDIAN_BIG}CP_UTF32BE{$ELSE}CP_UTF32{$ENDIF});
  exit(CP_NONE);
end;


initialization
  XMLNamespace_XML := TNamespace.Make(XMLNamespaceUrl_XML, 'xml');
  XMLNamespace_XMLNS := TNamespace.Make(XMLNamespaceUrl_XMLNS, 'xmlns');

  omittedEndTags:=THTMLOmittedEndTags.Create;
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['li'], ['ol', 'ul', 'menu' {only if @type in toolbar state}]));
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['dd', 'dt' {omitting dt closing tag not allowed if last}], ['dl']));

  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['rt', 'rp'], ['ruby']));
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['optgroup'], ['select' {useful? nesting selects might not be allowed}], ['option']));
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['option'],  ['optgroup', 'select']));
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['tbody', 'tfoot'], ['table'], ['colgroup', 'col']));
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['thead'], ['table'], ['colgroup', 'col']));
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['tr'], ['thead', 'tbody', 'tfoot', 'table' {table not allowed but safer so}], ['colgroup', 'col']));
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['td', 'th'], ['tr', 'table', 'tbody', 'thead', 'tfoot' {only tr allowed """}], ['colgroup', 'col']));
  //autoclosing of p tags not handled here, there are too many siblings "address, article, aside, blockquote, dir, div, dl, fieldset, footer, form, h1, h2, h3, h4, h5, h6, header, hgroup, hr, menu, nav, ol, p, pre, section, table, or ul, element, or if there is no more content in the parent element and the parent element is not an a element."

  //?? these are not listed in allowed omitted end tags
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['colgroup'], ['table'], ['colgroup', 'col']));
  omittedEndTags.add(THTMLOmittedEndTagInfo.Create(['button'], ['']));


  omittedStartTags:=THTMLOmittedEndTags.Create;
  omittedStartTags.add(THTMLOmittedEndTagInfo.Create(['tr'], ['table'], ['tbody', 'tfoot', 'thead']));
  omittedStartTags.add(THTMLOmittedEndTagInfo.Create(['col'], ['table'], ['colgroup']));


  attributeDataTypeHackList1 := TAttributeList.Create;
  attributeDataTypeHackList2 := TAttributeList.Create;
finalization
  attributeDataTypeHackList1.Free;
  attributeDataTypeHackList2.free;
  XMLNamespace_XML := nil;  //prevent heaptrc warning
  XMLNamespace_XMLNS := nil;
  omittedEndTags.free;
  omittedStartTags.free;
end.

