{**
  @abstract This unit contains a html/xml -> tree converter

  @author Benito van der Zander (http://www.benibela.de)
}
unit simplehtmltreeparser;

{$mode objfpc} {$H+}
{$modeswitch advancedrecords}
interface

uses
  Classes, SysUtils, simplehtmlparser, bbutils;

type

{ TAttributeMap }


//**The type of a tree element. <Open>, text, or </close>
TTreeElementType = (tetOpen, tetClose, tetText, tetComment, tetProcessingInstruction, tetAttribute, tetDocument);
TTreeElementTypes = set of TTreeElementType;
//**Controls the search for a tree element.@br
//**ignore type: do not check for a matching type, ignore text: do not check for a matching text,
//**case sensitive: do not ignore the case, no descend: only check elements that direct children of the current node
TTreeElementFindOptions = set of (tefoIgnoreType, tefoIgnoreText, tefoCaseSensitive, tefoNoChildren, tefoNoGrandChildren);

TTreeParser = class;
TTreeDocument = class;

{ TTreeElement }

TStringComparisonFunc = function (const a,b: string): boolean of object;

INamespace = interface
  function getPrefix: string;
  function getURL: string;
  function serialize: string;
end;


{ TNamespace }

TNamespace = class(TInterfacedObject, INamespace)
  url: string;
  prefix: string;
  constructor create(const aurl: string; aprefix: string);
  function getPrefix: string;
  function getURL: string;
  function serialize: string;
  destructor Destroy; override;
end;

{ TNamespaceList }

TNamespaceList = class(TInterfaceList)
private
  function getNamespace(const prefix: string): INamespace;
  function getNamespace(i: integer): INamespace;
  function hasNamespacePrefixBefore(const prefix: string; const c: integer): boolean;
public
  function hasNamespacePrefix(const prefix: string; out ns: INamespace): boolean;
  function hasNamespacePrefix(const prefix: string): boolean;
  function hasNamespace(const n: INamespace): boolean;

  procedure add(const ns: TNamespace);
  procedure add(const ns: INamespace);
  procedure addIfNewPrefix(const ns: TNamespace);
  procedure addIfNewPrefix(const ns: INamespace);
  procedure addIfNewPrefixUrl(const ns: TNamespace);
  procedure addIfNewPrefixUrl(const ns: INamespace);

  function clone: TNamespaceList;

  property namespaces[prefix: string]: INamespace read getNamespace;
  property items[i: integer]: INamespace read getNamespace;
end;

TTreeElement = class;
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

//**@abstract A list of attributes.
//**Currently this is a simple string list, and you can get the values with the values property. (with c++ I would have used map<string, string> but this doesn't exist in Pascal)

{ TAttributeList }

TAttributeList = class(TStringList)
public
  constructor Create;
  function getAttribute(i: integer): TTreeAttribute;
  function getAttributeIgnoringNS(const name: string; const cmpFunction: TStringComparisonFunc): TTreeAttribute;
  function getAttributeWithNSPrefix(const namespaceprefix, localname: string; const cmpFunction: TStringComparisonFunc): TTreeAttribute;
  function getValue(i: integer): string;

  procedure add(const name, value: string; const namespace: INamespace = nil);
  procedure add(att: TTreeElement);

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
//**Attributes should be accessed with the getAttribute or getAttributeTry method. @br
//**But if you are brave or need to modify the attributes, you can access the internal attribute storage with the attributes field.@br
//**All attributes are stored in the same structure as the other elements as quadro-linked list. The names and values are stored in two lists, which are connected with the connecting @code(reverse) field.@br
//**You can use the second getAttributeTry method to get a pointer to value node for a certain name.
//**
//** @code(<foo name1="value1" name2="value2" name3="value3>/>)
//** @longCode(#
//**      /--------------\                                     pointer to first attribute (attributes)
//**      |              |
//**      |      /-->  <foo>  <-----------------------\
//**      |       |             |           |          |       single linked list to parent (parent)
//**      |       |             |           |          |
//**      \-->  name1  <-->  name2  <-->  name3        |       double linked list of attributes (previous and next)
//**             /|\          /|\          /|\         |
//**              |            |            |          |       single linked list of corresponding node/thing (reverse)
//**             \|/          \|/          \|/         |
//**            value1 <-->  value2 <-->  value3       |       (again) double linked list of attributes (previous and next)
//**              |            |            |          |
//**              \------------------------------------/       (again) single linked list to parent
//** #)
TTreeElement = class
//use the fields if you know what you're doing
  typ: TTreeElementType; //**<open, close, text or comment node
  value: string; //**< tag name for open/close nodes, text for text/comment nodes
  attributes: TAttributeList;  //**<nil if there are no attributes
  next: TTreeElement; //**<next element as in the file (first child if there are childs, else next on lowest level), so elements form a linked list
  previous: TTreeElement; //**< previous element (self.next.previous = self)
  parent: TTreeElement;
  document: TTreeElement;
  reverse: TTreeElement; //**<element paired by open/closing, or corresponding attributes
  namespace: INamespace; //**< Currently local namespace prefix. Might be changed to a pointer to a namespace map in future. (so use getNamespacePrefix and getNamespaceURL instead)

  offset: longint; //**<count of characters in the document before this element (so document_pchar + offset begins with value)

//otherwise use the functions
  //procedure deleteNext(); //**<delete the next node (you have to delete the reverse tag manually)
  procedure deleteAll(); //**<deletes the tree
  procedure changeEncoding(from,toe: TEncoding; substituteEntities: boolean; trimText: boolean); //**<converts the tree encoding from encoding from to toe, and substitutes entities (e.g &auml;)


  //Complex search functions.
  //**Returns the element with the given type and text which occurs before sequenceEnd.@br
  //**This function is nil-safe, so if you call TTreeElement(nil).findNext(...) it will return nil
  function findNext(withTyp: TTreeElementType; withText:string; findOptions: TTreeElementFindOptions=[]; sequenceEnd: TTreeElement = nil):TTreeElement;
  //**Find a matching direct child (equivalent to findNext with certain parameters, but easier to use)@br
  //**A direct child of X is a node Y with Y.parent = X. @br
  //**The options tefoNoChildren, tefoNoGrandChildren have of course no effect. (former is set to false, latter to true)
  function findChild(withTyp: TTreeElementType; withText:string; findOptions: TTreeElementFindOptions=[]): TTreeElement;

  function deepNodeText(separator: string=''):string; //**< concatenates the text of all (including indirect) text children
  function outerXML(insertLineBreaks: boolean = false):string;
  function innerXML(insertLineBreaks: boolean = false):string;

  function getValue(): string; //**< get the value of this element
  function getValueTry(out valueout:string): boolean; //**< get the value of this element if the element exists
  function hasAttribute(const a: string; const cmpFunction: TStringComparisonFunc = nil): boolean; //**< returns if an attribute with that name exists. cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttribute(const a: string):string; //**< get the value of an attribute of this element or '' if this attribute doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttribute(const a: string; const cmpFunction: TStringComparisonFunc):string; //**< get the value of an attribute of this element or '' if this attribute doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttribute(const a: string; const def: string; const cmpFunction: TStringComparisonFunc = nil):string; //**< get the value of an attribute of this element or '' if this attribute doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttributeTry(const a: string; out valueout: string; const cmpFunction: TStringComparisonFunc = nil):boolean; //**< get the value of an attribute of this element and returns false if it doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttributeTry(a: string; out valueout: TTreeAttribute; cmpFunction: TStringComparisonFunc = nil):boolean; //**< get the value of an attribute of this element and returns false if it doesn't exist cmpFunction controls is used to compare the attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getAttributeCount(): integer;

  function getNextSibling(): TTreeElement; //**< Get the next element on the same level or nil if there is none
  function getFirstChild(): TTreeElement; //**< Get the first child, or nil if there is none
  function getParent(): TTreeElement; //**< Searchs the parent, notice that this is a slow function (neither the parent nor previous elements are stored in the tree, so it has to search the last sibling)
  function getPrevious(): TTreeElement; //**< Searchs the previous, notice that this is a slow function (neither the parent nor previous elements are stored in the tree, so it has to search the last sibling)
  function getRootHighest(): TTreeElement;    //**< Returns the highest node ancestor
  function getRootElement(): TTreeElement;    //**< Returns the highest element node ancestor
  function getDocument(): TTreeDocument; //**< Returns the document node containing this node

  function getNodeName(): string;        //**< Returns the name as namespaceprefix:name if a namespace exists, or name otherwise. Only attributes, elements and PIs have names.
  function getNamespacePrefix(): string; //**< Returns the namespace prefix. (i.e. 'a' for 'a:b', '' for 'b')
  function getNamespaceURL(): string;    //**< Returns the namespace url. (very slow, it searches the parents for a matching xmlns attribute) cmpFunction controls is used to compare the xmlns: attribute name the searched string. (can be used to switch between case/in/sensitive)
  function getNamespaceURL(prefixOverride: string; cmpFunction: TStringComparisonFunc = nil): string; //**< Returns the url of a namespace prefix, defined in this element or one of his parents cmpFunction controls is used to compare the xmlns: attribute name the searched string. (can be used to switch between case/in/sensitive)
  procedure getOwnNamespaces(var list: TNamespaceList);
  procedure getAllNamespaces(var list: TNamespaceList; first: boolean = true);
  function isNamespaceUsed(const n: INamespace): boolean;

  property defaultProperty[name: string]: string read getAttribute; default;

  function isDeepEqual(cmpTo: TTreeElement; ignoredTypes: TTreeElementTypes = [tetComment, tetProcessingInstruction]; cmpFunction: TStringComparisonFunc = nil): boolean;

  procedure insert(el: TTreeElement); //**< inserts el after the current element (does only change next+previous, not reverse+parent)
  procedure insertSurrounding(before, after: TTreeElement); //**< Surrounds self by before and after, i.e. inserts "before" directly before the element and "after" directly after its closing tag (slow)
  procedure insertSurrounding(basetag: TTreeElement); //**< inserts basetag before the current tag, and creates a matching closing tag after the closing tag of self (slow)

  procedure addAttribute(const aname, avalue: string; const anamespace: TNamespace = nil); inline;
  procedure addAttributes(const props: array of THTMLProperty);
  procedure addNamespaceDeclaration(n: INamespace; overridens: boolean );
  procedure addChild(child: TTreeElement);

  procedure removeElementFromDoubleLinkedList; //removes the element from the double linked list (only updates previous/next)
  function deleteElementFromDoubleLinkedList: TTreeElement; //removes the element from the double linked list (only updates previous/next), frees it and returns next  (mostly useful for attribute nodes)

  function clone: TTreeElement;
protected
  function serializeXML(nodeSelf: boolean; insertLineBreaks: boolean): string;
  function cloneShallow: TTreeElement;

  procedure removeAndFreeNext(); //**< removes the next element (the one following self). (ATTENTION: looks like there is a memory leak for opened elements)
  procedure removeElementKeepChildren; //**< removes/frees the current element, but keeps the children (i.e. removes self and possible self.reverse. Will not remove the opening tag, if called on a closing tag)


public
  function toString(): string; reintroduce; //**< converts the element to a string (not recursive)

  constructor create();
  constructor create(atyp: TTreeElementType; avalue: string = '');
  destructor destroy();override;
  procedure initialized; virtual; //**<is called after an element is read, before the next one is read (therefore all fields are valid except next (and reverse for opening tags))

  function caseInsensitiveCompare(const a,b: string): boolean; //**< returns true if a=b case insensitive. Can be passed to getAttribute
  function caseSensitiveCompare(const a,b: string): boolean;   //**< returns true if a=b case sensitive. Can be passed to getAttribute

  class function compareInDocumentOrder(const a,b: TTreeElement): integer; static;
end;
TTreeElementClass = class of TTreeElement;

{ TTreeAttribute }

TTreeAttribute = class(TTreeElement)
  realvalue: string;
  function isNamespaceNode: boolean;
  function toNamespace: INamespace;
  constructor create(const aname, avalue: string; const anamespace: INamespace = nil);
end;

{ TTreeDocument }

TTreeDocument = class(TTreeElement)
protected
  FEncoding: TEncoding;
  FBaseURI, FDocumentURI: string;
  FCreator: TTreeParser;

public
  property baseURI: string read FBaseURI write FBaseURI;
  property documentURI: string read FDocumentURI write FDocumentURI;

  function getCreator: TTreeParser;

  //**Returns the current encoding of the tree. After the parseTree-call it is the detected encoding, but it can be overriden with setEncoding.
  function getEncoding: TEncoding;
  //**Changes the tree encoding
  //**If convertExistingTree is true, the strings of the tree are actually converted, otherwise only the meta encoding information is changed
  //**If convertEntities is true, entities like &ouml; are replaced (which is only possible if the encoding is known)
  procedure setEncoding(new: TEncoding; convertFromOldToNew: Boolean; convertEntities: boolean);

  destructor destroy; override;
end;

ETreeParseException = Exception;
{ TTreeParser }

//**Parsing model used to interpret the document
//**pmStrict: every tag must be closed explicitely (otherwise an exception is raised)
//**pmHtml: accept everything, tries to create the best fitting tree using a heuristic to recover from faulty documents (no exceptions are raised), detect encoding
TParsingModel = (pmStrict, pmHTML);
//**@abstract This parses a html/sgml/xml file to a tree like structure
//**To use it, you have to call @code(parseTree) with a string containing the document. Afterwards you can call @code(getTree) to get the document root node.@br
//**
//**The data structure is like a stream of annotated tokens with back links (so you can traverse it like a tree).@br
//**After tree parsing the tree contains the text as byte strings, without encoding or entity conversions. But in the case of html, the meta/http-equiv encoding is detected
//**and you can call setEncoding to change the tree to the encoding you need. (this will also convert the entities)@br
//**You can change the class used for the elements in the tree with the field treeElementClass.
TTreeParser = class
private
  FAutoDetectHTMLEncoding: boolean;
  FReadProcessingInstructions: boolean;
//  FConvertEntities: boolean;
  FCurrentElement: TTreeElement;
  FTemplateCount: Integer;
  FElementStack: TList;
  FAutoCloseTag: boolean;
  FCurrentFile: string;
  FParsingModel: TParsingModel;
  FTrimText, FReadComments: boolean;
  FTrees: TList;
  FCurrentTree: TTreeDocument;
  FXmlHeaderEncoding: TEncoding;


  function newTreeElement(typ:TTreeElementType; text: pchar; len:longint):TTreeElement;
  function newTreeElement(typ:TTreeElementType; s: string):TTreeElement;
  procedure autoCloseLastTag();

  function enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):TParsingResult;
  function leaveTag(tagName: pchar; tagNameLen: longint):TParsingResult;
  function readText(text: pchar; textLen: longint):TParsingResult;
  function readComment(text: pchar; textLen: longint):TParsingResult;

private
  FCurrentNamespace: INamespace;
  FCurrentNamespaces: TNamespaceList;
  FCurrentNamespaceDefinitions: TList;
  FTargetEncoding: TEncoding;
  procedure pushNamespace(const url, prefix: string);
  function findNamespace(const prefix: string): INamespace;

  function htmlTagWeight(s:string): integer;
  function htmlTagAutoClosed(s:string): boolean;
public
  treeElementClass: TTreeElementClass; //**< Class of the tree nodes. You can subclass TTreeElement if you need to store additional data at every node
  globalNamespaces: TNamespaceList;

  constructor Create;
  destructor destroy;override;
  procedure clearTrees;
  //** Creates a new tree from a html document contained in html. @br
  //** The uri parameter is just stored and returned for you by baseURI, not actually used within this class. @br
  //** contentType is used to detect the encoding
  function parseTree(html: string; uri: string = ''; contentType: string = ''): TTreeDocument;
  function parseTreeFromFile(filename: string): TTreeDocument;

  function getLastTree: TTreeDocument; //**< Returns the last created tree

  procedure removeEmptyTextNodes(const whenTrimmed: boolean);
published
  //** Parsing model, see TParsingModel
  property parsingModel: TParsingModel read FParsingModel write FParsingModel;
  //** If this is true (default), white space is removed from text nodes
  property trimText: boolean read FTrimText write FTrimText;
  //** If this is true (default is false) comments are included in the generated tree
  property readComments: boolean read FReadComments write FReadComments;
  //** If this is true (default is false) processing instructions are included in the generated tree
  property readProcessingInstructions: boolean read FReadProcessingInstructions write FReadProcessingInstructions;
  property autoDetectHTMLEncoding: boolean read FAutoDetectHTMLEncoding write fautoDetectHTMLEncoding;
//  property convertEntities: boolean read FConvertEntities write FConvertEntities;
  property TargetEncoding: TEncoding read FTargetEncoding write FTargetEncoding;
end;


function xmlStrEscape(s: string):string;

const XMLNamespaceUrl_XML = 'http://www.w3.org/XML/1998/namespace';
      XMLNamespaceUrl_XMLNS = 'http://www.w3.org/2000/xmlns/';
const TreeNodesWithChildren = [tetOpen, tetDocument];

var
   XMLNamespace_XMLNS, XMLNamespace_XML: INamespace;

function equalNamespaces(const ans, bns: INamespace): boolean; inline;
implementation
uses xquery;


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
  if namespace = nil then result := TNamespace.Create(realvalue, '') //TODO: reuse
  else result := TNamespace.Create(realvalue, value);
end;

constructor TTreeAttribute.create(const aname, avalue: string; const anamespace: INamespace = nil);
begin
  inherited create(tetAttribute, aname);
  realvalue := avalue;
  namespace := anamespace;
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

procedure TAttributeList.add(att: TTreeElement);
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
var
  i: Integer;
begin
  for i:= 0 to count - 1 do Items[i].Free;
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


function TNamespaceList.clone: TNamespaceList;
var
  i: Integer;
begin
  result := TNamespaceList.Create;
  for i := 0 to count - 1 do
    result.Add(items[i]);
end;

{ TNamespace }

constructor TNamespace.create(const aurl: string; aprefix: string);
begin
  url := aurl;
  prefix := aprefix;
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
  if prefix = '' then result := 'xmlns="'+xmlStrEscape(url)+'"'
  else result := 'xmlns:'+prefix+'="'+xmlStrEscape(url)+'"'
end;

destructor TNamespace.Destroy;
begin
  inherited Destroy;
end;

{ TTreeDocument }

function TTreeDocument.getCreator: TTreeParser;
begin
  result := FCreator;
end;

function TTreeDocument.getEncoding: TEncoding;
begin
  if self = nil then exit(eUnknown);
  result := FEncoding;
end;

procedure TTreeDocument.setEncoding(new: TEncoding; convertFromOldToNew: Boolean; convertEntities: boolean);
begin
  if self = nil then exit;
  if (FEncoding = eUnknown) or not convertFromOldToNew then FEncoding:= new;
  if convertFromOldToNew or convertEntities then changeEncoding(FEncoding, new, convertEntities, FCreator.FTrimText);
  FEncoding := new;
end;

destructor TTreeDocument.destroy;
begin
  inherited destroy;
end;

{ TTreeElement }

{procedure TTreeElement.deleteNext();
var
  temp: TTreeElement;
begin
  if next = nil then exit;
  temp := next;
  next := next.next;
  temp.Free;
end;}

procedure TTreeElement.deleteAll();
begin
  if next <> nil then next.deleteAll();
  next:=nil;
  Free;
end;

procedure TTreeElement.changeEncoding(from, toe: TEncoding; substituteEntities: boolean; trimText: boolean);
  function change(s: string): string;
  begin
    result := strChangeEncoding(s, from, toe);
    result := strNormalizeLineEndings(result);
    if substituteEntities then result := strDecodeHTMLEntities(result, toe, false);
    if trimText then result := trim(result); //retrim because &nbsp; replacements could have introduced new spaces
  end;

var tree: TTreeElement;
  attrib: TTreeAttribute;
begin
  if (from = eUnknown) or (toe = eUnknown) then exit;
  if (from = toe) and not substituteEntities then exit;
  tree := self;
  while tree <> nil do begin
    case tree.typ of
      tetText, tetProcessingInstruction: tree.value := change(tree.value);
      tetComment: tree.value:=strChangeEncoding(tree.value, from, toe);
      tetDocument, tetOpen, tetClose: begin
        tree.value := change(tree.value);
        if tree.attributes <> nil then
          for attrib in tree.attributes do begin
            attrib.value := change(attrib.value);
            attrib.realvalue := change(attrib.realvalue);
          end;
      end;
      else raise Exception.Create('Unkown tree element: '+tree.outerXML());
    end;
    tree := tree.next;
  end;
end;

function TTreeElement.findNext(withTyp: TTreeElementType; withText: string; findOptions: TTreeElementFindOptions =[]; sequenceEnd: TTreeElement = nil): TTreeElement;
var cur: TTreeElement;
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

function TTreeElement.findChild(withTyp: TTreeElementType; withText: string;
  findOptions: TTreeElementFindOptions): TTreeElement;
begin
  result := nil;
  if self = nil then exit;
  if not (typ in TreeNodesWithChildren) then exit;
  if reverse = nil then exit;
  result:=findNext(withTyp, withText, findOptions + [tefoNoGrandChildren] - [tefoNoChildren], reverse);
end;

function TTreeElement.deepNodeText(separator: string): string;
var cur:TTreeElement;
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

function TTreeElement.outerXML(insertLineBreaks: boolean = false): string;
begin
  result := serializeXML(true, insertLineBreaks);
end;

function TTreeElement.innerXML(insertLineBreaks: boolean = false): string;
begin
  result := serializeXML(false, insertLineBreaks);
end;

function TTreeElement.getValue(): string;
begin
  if self = nil then exit('');
  result := value;
end;

function TTreeElement.getValueTry(out valueout:string): boolean;
begin
  if self = nil then exit(false);
  valueout := self.value;
  result := true;
end;

function TTreeElement.hasAttribute(const a: string; const cmpFunction: TStringComparisonFunc = nil): boolean;
var temp: TTreeAttribute;
begin
  exit(getAttributeTry(a, temp, cmpFunction));
end;

function TTreeElement.getAttribute(const a: string): string;
begin
  result := getAttribute(a, @caseInsensitiveCompare);
end;

function TTreeElement.getAttribute(const a: string; const cmpFunction: TStringComparisonFunc = nil): string;
begin
  if not getAttributeTry(a, result, cmpFunction) then
    result:='';
end;

function TTreeElement.getAttribute(const a: string; const def: string; const cmpFunction: TStringComparisonFunc = nil):string;
begin
  if not getAttributeTry(a, result, cmpFunction) then
    result:=def;
end;

function TTreeElement.getAttributeTry(const a: string; out valueout: string; const cmpFunction: TStringComparisonFunc = nil): boolean;
var temp: TTreeAttribute;
begin
  result := getAttributeTry(a, temp, cmpFunction);
  if not result then exit;
  valueout := temp.realvalue;
end;

function TTreeElement.getAttributeTry(a: string; out valueout: TTreeAttribute; cmpFunction: TStringComparisonFunc = nil): boolean;
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

function TTreeElement.getAttributeCount: integer;
begin
  if (self = nil) or (attributes = nil) then exit(0);
  result := attributes.Count;
end;

function TTreeElement.getNextSibling(): TTreeElement;
begin
  case typ of
    tetOpen, tetDocument: result:=reverse.next;
    tetText, tetClose, tetComment, tetProcessingInstruction: result := next;
    else raise Exception.Create('Invalid tree element type');
  end;
  if result = nil then exit;
  if result.typ = tetClose then exit(nil);
end;

function TTreeElement.getFirstChild(): TTreeElement;
begin
  if not (typ in TreeNodesWithChildren) then exit(nil);
  if next = reverse then exit(nil);
  exit(next);
end;

function TTreeElement.getParent(): TTreeElement;
begin
  if (self = nil) then exit(nil);
  exit(parent);
end;

function TTreeElement.getPrevious: TTreeElement;
begin
  if self = nil then exit;
  result := previous
end;

function TTreeElement.getRootHighest: TTreeElement;
begin
  if self = nil then exit(nil);
  result := document;
end;

function TTreeElement.getRootElement: TTreeElement;
begin
  result := document;
  if (result = nil) or (result.typ = tetOpen) then exit;
  exit(result.findChild(tetOpen,'',[tefoIgnoreText]));
end;

function TTreeElement.getDocument: TTreeDocument;
begin
  result := document as TTreeDocument;
end;

function TTreeElement.getNodeName: string;
begin
  case typ of
    tetOpen, tetAttribute, tetClose, tetProcessingInstruction: begin
      if (namespace = nil) or (namespace.getPrefix = '') then exit(value);
      exit(getNamespacePrefix() + ':' + value);
    end;
    else result := '';
  end;
end;

function TTreeElement.getNamespacePrefix: string;
begin
  if namespace = nil then exit('');
  result := namespace.getPrefix;
end;

function TTreeElement.getNamespaceURL(): string;
begin
  if namespace = nil then exit('');
  result := namespace.getURL;
end;

function TTreeElement.getNamespaceURL(prefixOverride: string; cmpFunction: TStringComparisonFunc = nil): string;
var
  n: TTreeElement;
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
  exit('');
end;

procedure TTreeElement.getOwnNamespaces(var list: TNamespaceList);
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

procedure TTreeElement.getAllNamespaces(var list: TNamespaceList; first: boolean);
var attrib: TTreeAttribute;
begin
  if first then getOwnNamespaces(list)
  else begin
    if attributes <> nil then
      for attrib in attributes do
        if attrib.isNamespaceNode then
          list.addIfNewPrefix(attrib.toNamespace);
    list.addIfNewPrefix(namespace);
    if attributes <> nil then
      for attrib in attributes do
        if not attrib.isNamespaceNode then
          list.addIfNewPrefix(attrib.namespace);
  end;
  if parent <> nil then parent.getAllNamespaces(list, false);
end;

function TTreeElement.isNamespaceUsed(const n: INamespace): boolean;
var attrib: TTreeAttribute;
  temp: TTreeElement;
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

function TTreeElement.isDeepEqual(cmpTo: TTreeElement; ignoredTypes: TTreeElementTypes; cmpFunction: TStringComparisonFunc): boolean;
var
  attrib, tempattrib: TTreeAttribute;
  temp1, temp2: TTreeElement;
begin
  //this follows the XPath deep-equal function
  result := false;
  if typ <> cmpTo.typ then exit();
  if not cmpFunction(value, cmpTo.value) then exit;
  if getNamespaceURL() <> cmpto.getNamespaceURL() then exit;
  case typ of
    tetAttribute:
      if    not cmpFunction(value, TTreeAttribute(cmpTo).value)
         or not cmpFunction(TTreeAttribute(self).realvalue, TTreeAttribute(cmpTo).realvalue) then exit;
    tetProcessingInstruction: if getAttribute('') <> cmpTo.getAttribute('') then exit;
    tetOpen, tetDocument: begin
      if getAttributeCount <> cmpTo.getAttributeCount then exit;
      if attributes <> nil then
        for attrib in attributes do begin
          if not cmpTo.getAttributeTry(attrib.value, tempattrib, cmpFunction) then exit;
          if not cmpFunction(attrib.realvalue, tempattrib.realvalue) then exit;
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
    tetComment, tetText, tetClose: ;
    else raise ETreeParseException.Create('Invalid node type');
  end;
  result := true;
end;

procedure TTreeElement.insert(el: TTreeElement);
begin
  // self  self.next  => self el self.next
  if self = nil then exit;
  el.next := self.next;
  self.next := el;
  el.offset := offset;
  el.previous:=self;
  if el.next <> nil then el.next.previous := el;
end;

procedure TTreeElement.insertSurrounding(before, after: TTreeElement);
var surroundee, prev: TTreeElement;
  el: TTreeElement;
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

procedure TTreeElement.insertSurrounding(basetag: TTreeElement);
var closing: TTreeElement;
begin
  if not (basetag.typ in TreeNodesWithChildren) then raise Exception.Create('Need an opening tag to surround another tag');
  closing := TTreeElement(basetag.ClassType.Create);
  closing.typ := tetClose;
  closing.value := basetag.value;
  insertSurrounding(basetag, closing);
end;

procedure TTreeElement.addAttribute(const aname, avalue: string; const anamespace: TNamespace = nil);
begin
  if attributes = nil then attributes := TAttributeList.Create;
  attributes.add(aname, avalue, anamespace);
  attributes.Items[attributes.count - 1].parent := self;
  attributes.Items[attributes.count - 1].offset := offset + 1;
  attributes.Items[attributes.count - 1].document := document;
end;

procedure TTreeElement.addAttributes(const props: array of THTMLProperty);
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

procedure TTreeElement.addNamespaceDeclaration(n: INamespace; overridens: boolean );
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

procedure TTreeElement.addChild(child: TTreeElement);
var
  oldprev: TTreeElement;
begin
  child.parent := self;
  oldprev := reverse.previous;
  oldprev.next := child;
  child.previous := oldprev;
  if child.reverse = nil then begin
    reverse.previous := child;
    child.next := reverse;
  end else begin
    reverse.previous := child.reverse;
    child.reverse.next := reverse;
  end;
end;

procedure TTreeElement.removeElementFromDoubleLinkedList;
begin
  if previous <> nil then previous.next := next;
  if next <> nil then next.previous := nil;
end;

function TTreeElement.deleteElementFromDoubleLinkedList: TTreeElement;
begin
  result := next;
  removeElementFromDoubleLinkedList;
  free;
end;

function namespaceUsedByNodeOrChild(n: ttreeelement; const url, prefix: string): boolean;
var
  m: TTreeElement;
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

function serializeXMLWrapper(base: TTreeElement; nodeSelf: boolean; insertLineBreaks: boolean): string;
var known: TNamespaceList;
  function requireNamespace(n: INamespace): string;
  begin //that function is useless the namespace should always be in known. But just for safety...
    if (n = nil) or (n.getURL = XMLNamespaceUrl_XML) or (n.getURL = XMLNamespaceUrl_XMLNS) or (known.hasNamespace(n)) then exit('');
    known.add(n);
    result := ' ' + n.serialize;
  end;

  function inner(n: TTreeElement): string; forward;

  function outer(n: TTreeElement): string;
  var attrib: TTreeAttribute;
      oldnamespacecount: integer;
      i: Integer;
      temp: INamespace;
  begin
    with n do
    case typ of
      tetText: result := xmlStrEscape(value);
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
            known.add(tNamespace.create('', ''));
            result += ' xmlns=""';
          end;
        if attributes <> nil then
          for attrib in attributes do
            result += requireNamespace(attrib.namespace);


        if attributes <> nil then
          for attrib in attributes do
            if not attrib.isNamespaceNode then
              result += ' ' + attrib.getNodeName()+'="'+xmlStrEscape(attrib.realvalue)+'"';

        if next = reverse then begin
          result += '/>';
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
      tetDocument: result := innerXML(insertLineBreaks);
    end;
  end;

  function inner(n: TTreeElement): string;
  var sub: TTreeElement;
  begin
    result := '';
    if not (n.typ in TreeNodesWithChildren) then exit;
    sub := n.next;
    while sub <> n.reverse do begin
      result += outer(sub);
      if not (sub.typ in TreeNodesWithChildren) then sub:=sub.next
      else sub := sub.reverse.next;
    end;
  end;
begin
  known := TNamespaceList.Create;
  if nodeSelf then result := outer(base)
  else result := inner(base);
  known.free;
end;

function TTreeElement.serializeXML(nodeSelf: boolean; insertLineBreaks: boolean): string;

begin
  if self = nil then exit('');
  result := serializeXMLWrapper(self, nodeSelf, insertLineBreaks);
end;

function TTreeElement.cloneShallow: TTreeElement;
begin
  case typ of
    tetAttribute: begin
      result := TTreeAttribute.create(value, TTreeAttribute(self).realvalue);
    end;
    tetDocument: begin
      result := TTreeDocument.create();
      TTreeDocument(result).FEncoding:=TTreeDocument(self).FEncoding;
      TTreeDocument(result).FBaseURI:=TTreeDocument(self).FBaseURI;
      TTreeDocument(result).FDocumentURI:=TTreeDocument(self).FDocumentURI;
      TTreeDocument(result).FCreator:=TTreeDocument(self).FCreator;
    end
    else result := TTreeElement.create();
  end;
  result.typ := typ;
  result.value := value;
  result.attributes := attributes;
  result.next := nil;
  result.previous := nil;
  result.parent := nil;
  result.reverse := nil;
  result.namespace := namespace;
  result.offset := offset;
end;

function TTreeElement.clone: TTreeElement;
var
  kid: TTreeElement;
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

procedure TTreeElement.removeAndFreeNext();
var
  toFree: TTreeElement;
  temp: TTreeElement;
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
    raise Exception.Create('Cannot remove single closing tag')
  else
    next := toFree.next;
  next.previous := self;
  tofree.free;
end;

procedure TTreeElement.removeElementKeepChildren;
var
  temp: TTreeElement;
begin
  if previous = nil then raise Exception.Create('Cannot remove first tag');
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

function TTreeElement.caseInsensitiveCompare(const a, b: string): boolean;
begin
  result := striEqual(a, b);
end;

function TTreeElement.caseSensitiveCompare(const a, b: string): boolean;
begin
  result := a = b;
end;

function TTreeElement.toString(): string;
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
          result += ' '+attrib.value + '="'+attrib.reverse.value+'"';
      result+='>';
    end;
    tetDocument: result := innerXML();
    tetComment: exit('<!--'+value+'-->');
    else exit('??');
  end;
end;

constructor TTreeElement.create();
begin
end;

constructor TTreeElement.create(atyp: TTreeElementType; avalue: string);
begin
  self.typ := atyp;
  self.value := avalue;
end;

destructor TTreeElement.destroy();
begin
  attributes.Free;
  inherited destroy();
end;

procedure TTreeElement.initialized;
begin

end;

class function TTreeElement.compareInDocumentOrder(const a,b: TTreeElement): integer;
begin
  if a.document = b.document then
    exit(a.offset - b.offset);
  if pointer(a.document) < pointer(b.document) then exit(-1)
  else exit(1);
end;




{ THTMLTreeParser }

function TTreeParser.newTreeElement(typ:TTreeElementType; text: pchar; len: longint): TTreeElement;
begin
  result := newTreeElement(typ, strFromPchar(text, len));
  result.offset:=longint(text - @FCurrentFile[1]);
end;

function TTreeParser.newTreeElement(typ: TTreeElementType; s: string
  ): TTreeElement;
begin
  result:=treeElementClass.Create;
  result.typ := typ;
  result.value := s;
  result.document := FCurrentTree;
  FTemplateCount+=1;

  FCurrentElement.next := result;
  result.previous := FCurrentElement;
  FCurrentElement := result;

  if typ <> tetClose then result.parent := TTreeElement(FElementStack.Last)
  else result.parent := TTreeElement(FElementStack.Last).getParent();
  //FCurrentElement.id:=FTemplateCount;
end;

procedure TTreeParser.autoCloseLastTag();
var
  last: TTreeElement;
  new: TTreeElement;
begin
  last := TTreeElement(FElementStack.Last);
  Assert(last<>nil);
  if last.typ = tetOpen then begin
    new := newTreeElement(tetClose, last.value);
    //new := treeElementClass.create();
    //new.typ:=tetClose;
    //new.value:=last.value;
    new.offset:=last.offset;
    //new.next:=last.next;
    //last.next:=new;
    last.reverse:=new; new.reverse:=last;
  end;
  FElementStack.Delete(FElementStack.Count-1);
  FAutoCloseTag:=false;
end;

function TTreeParser.enterTag(tagName: pchar; tagNameLen: longint;
  properties: THTMLProperties): TParsingResult;
var
  new,temp: TTreeElement;
  i: Integer;
  j: Integer;
  enc: String;
  first,last: PChar;
  attrib: TTreeAttribute;
begin
  result:=prContinue;

  if tagName^ = '?' then begin //processing instruction
    if strlEqual(tagName, '?xml', tagNameLen) then begin
      enc := lowercase(getProperty('encoding', properties));
      if enc = 'utf-8' then FXmlHeaderEncoding:=eUTF8
      else if (enc = 'windows-1252') or (enc = 'iso-8859-1') or (enc = 'iso-8859-15') or (enc = 'latin1') then
        FXmlHeaderEncoding:=eWindows1252;
      exit;
    end;
    if not FReadProcessingInstructions then exit;
    new := newTreeElement(tetProcessingInstruction, tagName + 1, tagNameLen - 1);
    if length(properties)>0 then begin
      first := properties[0].name;
      first-=1;
      while first^ in [' ',#9] do first-=1;
      first+=2;
      last := properties[high(properties)].value + properties[high(properties)].valueLen;
      while ((last+1)^ <> #0) and ((last^ <> '?') or ((last+1)^ <> '>'))  do last+=1;

      new.addAttribute('', strNormalizeLineEndings(strFromPchar(first, last-first)));
      new.addAttributes(properties);
    end;
    new.initialized;
    exit;
  end;

  if FAutoCloseTag then autoCloseLastTag();
  if (FParsingModel = pmHTML) then begin
    //table hack (don't allow two open td/tr unless separated by tr/table)
    if strliEqual(tagName,'td',tagNameLen) then begin
      for i:=FElementStack.Count-1 downto 0 do begin
        temp :=TTreeElement(FElementStack[i]);
        if not (temp.typ in  [tetDocument, tetOpen, tetClose]) then continue;
        if (temp.value<>'tr') and (temp.value<>'td') and (temp.value<>'table') then continue;
        if (temp.typ = tetClose) then break;
        if (temp.typ = tetOpen) and (temp.value='td') then begin
          for j:=FElementStack.count-1 downto i do
            autoCloseLastTag();
          break;
        end;
        (*if (temp.typ = [tetOpen]) and ((temp.value='tr') or (temp.value='table')) then *)break;
      end;
    end else if strliEqual(tagName,'tr',tagNameLen) then begin
      for i:=FElementStack.Count-1 downto 0 do begin
        temp :=TTreeElement(FElementStack[i]);
        if not (temp.typ in  [tetDocument, tetOpen, tetClose]) then continue;
        if (temp.value<>'tr') and (temp.value<>'td') and (temp.value<>'table') then continue;
        if (temp.typ = tetClose) and ((temp.value='tr') or (temp.value='table')) then break;
        if (temp.typ = tetOpen) and (temp.value='tr') then begin
          for j:=FElementStack.count-1 downto i do
            autoCloseLastTag();
          break;
        end;
        if (temp.typ = tetOpen) and (temp.value='table') then break;
      end;
    end;
  end;
  new := newTreeElement(tetOpen, tagName, tagNameLen);
  if (FParsingModel = pmHTML) then //normal auto close
    FAutoCloseTag:=htmlTagAutoClosed(new.value);

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
      end else if pos(':', attrib.value) > 0 then
        attrib.namespace := findNamespace(strSplitGet(':', attrib.value));
  end;
  if (pos(':', new.value) > 0) then
    new.namespace := findNamespace(strSplitGet(':', new.value))
   else
    new.namespace := FCurrentNamespace;;

  new.initialized;
end;

function TTreeParser.leaveTag(tagName: pchar; tagNameLen: longint): TParsingResult;
var
  new,last,temp: TTreeElement;
  match: longint;
  i: Integer;
  weight: LongInt;
  parenDelta: integer;
  name: String;
  removedCurrentNamespace: Boolean;
begin
  result:=prContinue;

  last := TTreeElement(FElementStack.Last);
  if (FParsingModel = pmStrict) and (last = nil) then
    raise ETreeParseException.create('The tag <'+strFromPchar(tagName,tagNameLen)+'> was closed, but none was open');

  if last = nil then exit;

  if FAutoCloseTag and (not strliequal(tagName, last.value, tagNameLen)) then autoCloseLastTag();
  FAutoCloseTag:=false;

  new := nil;
  if (strliequal(tagName, last.getNodeName, tagNameLen)) then begin
    new := newTreeElement(tetClose, tagName, tagNameLen);
    new.reverse := last; last.reverse := new;
    FElementStack.Delete(FElementStack.Count-1);
    new.initialized;
  end else if FParsingModel = pmStrict then
    raise ETreeParseException.Create('The tag <'+strFromPchar(tagName,tagNameLen)+'> was closed, but the latest opened was <'+last.value+'>  (url: '+FCurrentTree.FBaseURI+')')
  else if FParsingModel = pmHTML then begin
    //try to auto detect unclosed tags
    match:=-1;
    for i:=FElementStack.Count-1 downto 0 do
      if strliequal(tagName, TTreeElement(FElementStack[i]).value, tagNameLen) then begin
        match:=i;
        break;
      end;
    if match > -1 then begin
      //there are unclosed tags, but a tag opening the currently closed exist, close all in between
      weight := htmlTagWeight(strFromPchar(tagName, tagNameLen));
      for i:=match+1 to FElementStack.Count-1 do
        if htmlTagWeight(TTreeElement(FElementStack[i]).value) > weight then
            exit;
      for i:=match+1 to FElementStack.Count-1 do
        autoCloseLastTag();
      new := newTreeElement(tetClose, tagName, tagNameLen);
      last := TTreeElement(FElementStack[match]);
      last.reverse := new; new.reverse := last;
      FElementStack.Count:=match;
      new.initialized;
    end;

    name := strFromPchar(tagName, tagNameLen);
    if htmlTagAutoClosed(name) then begin
      parenDelta := 0;
      last := FCurrentElement;
      while last <> nil do begin
        if last.typ = tetClose then parenDelta -= 1
        else if (last.typ = tetOpen) then begin
          parenDelta+=1;
          if (last.value = name) then begin
            if (last.reverse <> last.next) or (parenDelta <> 0) then break; //do not allow nested auto closed elements (reasonable?)
            //remove old closing tag, and insert new one at the end
            new := newTreeElement(tetClose, tagName, tagNameLen);
            last.reverse.removeElementKeepChildren;
            last.reverse := new; new.reverse := last;

            new.parent := last.parent;

            //update parents
            temp := last.getFirstChild();
            while (temp <> nil) and (last <> new) do begin
              if temp.parent = last.parent then temp.parent := last;
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

  if new <> nil then begin
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
  end;
end;

function TTreeParser.readText(text: pchar; textLen: longint): TParsingResult;
begin
  result:=prContinue;

  if (FParsingModel = pmStrict) and (FElementStack.Count < 2) then begin
    strlTrimLeft(text, textLen);
    if textLen = 0 then exit;
    if strBeginsWith(text, #239#187#191) or strBeginsWith(text,#254#255) or strBeginsWith(text, #255#254) or
       strBeginsWith(text, #43#47#118) then raise Exception.Create('xml ' + FCurrentTree.FBaseURI + ' starts with unicode BOM. That is not supported');
    raise ETreeParseException.Create('Data not allowed at root level: '+strFromPchar(text,textLen));
  end;

  if FAutoCloseTag then
    autoCloseLastTag();

  if FTrimText then
    strlTrim(text, textLen, [' ',#0,#9,#10,#13]);

  if textLen = 0 then
    exit;

  newTreeElement(tetText, text, textLen).initialized;
end;

function TTreeParser.readComment(text: pchar; textLen: longint): TParsingResult;
begin
  result:=prContinue;
  if not FReadComments then
    exit;
  if textLen <= 0 then
    exit;
  newTreeElement(tetComment, text, textLen).initialized;
end;

procedure TTreeParser.pushNamespace(const url, prefix: string);
var
  ns: INamespace;
begin
  ns := TNamespace.Create(url, prefix);
  FCurrentNamespaces.Add(ns);
  FCurrentNamespaceDefinitions.Add(FCurrentElement);
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
    else if parsingModel = pmStrict then raise ETreeParseException.Create('Unknown namespace: '+prefix);
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
  if striequal(s, 'td') or striequal(s, 'ul') or striequal(s, 'ol') or striequal(s, 'dd') or striequal(s, 'dt') then exit(3);
  if striequal(s, 'tr') or striequal(s, 'li') or striequal(s, 'dl') then exit(4);
  if striequal(s, 'body') or striequal(s, 'html') or striequal(s, 'div') or striequal(s, 'table') then exit(5);

  if striequal(s, '') then exit(100); //force closing of root element
end;

function TTreeParser.htmlTagAutoClosed(s: string): boolean;
begin
  //elements that should/must not have children
  result:=striequal(s,'meta') or
          striequal(s,'br') or
          striequal(s,'input') or
          striequal(s,'frame') or
          striequal(s,'hr')or
          striequal(s,'img');//or strliequal(s,'p');
end;

constructor TTreeParser.Create;
begin
  FElementStack := TList.Create;
  treeElementClass := TTreeElement;
  FTrimText:=true;
  FReadComments:=false;
  FReadProcessingInstructions:=false;
  FAutoDetectHTMLEncoding:=true;
  FTrees := TList.Create;

  FCurrentNamespaceDefinitions := TList.Create;
  FCurrentNamespaces := TNamespaceList.Create;
  globalNamespaces := TNamespaceList.Create;
  FTargetEncoding:=eUTF8;
  //FConvertEntities := true;
end;

destructor TTreeParser.destroy;
begin
  clearTrees;
  FElementStack.free;
  ftrees.Free;
  FCurrentNamespaces.Free;
  FCurrentNamespaceDefinitions.Free;
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
      else if (prev and $80) = $80 then bad += 1;
    end else begin
      if (prev and $C0) = $C0 then bad+=1
    end;
    prev := cur;
  end;
  result := good < 10 * bad;
end;

function TTreeParser.parseTree(html: string; uri: string; contentType: string): TTreeDocument;
  function encodingFromContentType(encoding: string): TEncoding;
  begin
    encoding := lowercase(encoding);
    if pos('charset=utf-8', encoding) > 0 then exit(eUTF8);
    if (pos('charset=windows-1252',encoding) > 0) or
       (pos('charset=latin1',encoding) > 0) or
       (pos('charset=iso-8859-1',encoding) > 0) then //also -15
        exit(eWindows1252);
    exit(eUnknown);
  end;

var
  el: TTreeElement;
  attrib: TTreeAttribute;
  encMeta, encHeader: TEncoding;
begin
  FTemplateCount:=0;
  FElementStack.Clear;
  FCurrentTree:=nil;

  //FVariables.clear;
  if html='' then exit(nil);

  FCurrentFile:=html;
  FAutoCloseTag:=false;
  FCurrentNamespace := nil;

  //initialize root element
  //there are two reasons for an empty root element which doesn't exists in the file
  //1. it is necessary for the correct interpretion of xpath expressions html/... assumes
  //   that the current element is a parent of html
  //2. it serves as parent for multiple top level elements (althought they aren't allowed)
  FCurrentTree:=TTreeDocument.create;
  FCurrentTree.FCreator:=self;
  FCurrentTree.typ := tetDocument;
  FCurrentTree.FBaseURI:=uri;
  FCurrentTree.FDocumentURI:=uri;
  FCurrentTree.document := FCurrentTree;
  FCurrentElement:=FCurrentTree;
  FElementStack.Clear;
  FElementStack.Add(FCurrentElement);
  FTemplateCount:=1;
  FXmlHeaderEncoding := eUnknown;

  //parse
  if FParsingModel = pmHTML then simplehtmlparser.parseHTML(FCurrentFile,@enterTag, @leaveTag, @readText, @readComment)
  else simplehtmlparser.parseML(FCurrentFile,[],@enterTag, @leaveTag, @readText, @readComment);

  //close root element
  leaveTag('',0);

  if FAutoDetectHTMLEncoding  then begin
    FCurrentTree.FEncoding:=eUnknown;
    encHeader := encodingFromContentType(contentType);
    if parsingModel = pmHTML then
      encMeta := encodingFromContentType(TXQueryEngine.evaluateStaticXPath2('html/head/meta[@http-equiv=''content-type'']/@content', FCurrentTree).toString)
     else
      encMeta := encHeader;

    if encHeader = eUnknown then encHeader := FXmlHeaderEncoding;
    if encHeader = eUnknown then encHeader := encMeta;
    if encMeta  = eUnknown then encMeta := encHeader;
    if FXmlHeaderEncoding = eUnknown then FXmlHeaderEncoding := encHeader;
    if (encMeta = encHeader) and (encMeta = FXmlHeaderEncoding) and (encMeta <> eUnknown) then
      FCurrentTree.FEncoding := encMeta
    else begin //if in doubt, detect encoding and ignore meta/header data
      FCurrentTree.FEncoding:=eUTF8;
      el := FCurrentTree.next;
      while el <> nil do begin
        case el.typ of
          tetText: if isInvalidUTF8(el.value) then begin
            FCurrentTree.FEncoding:=eWindows1252;
            break;
          end;
          tetOpen, tetDocument: if el.attributes <> nil then begin
            for attrib in el.attributes do
              if isInvalidUTF8(attrib.value) or isInvalidUTF8(attrib.realvalue) then begin
                FCurrentTree.FEncoding:=eWindows1252;
                break;
              end;
            if FCurrentTree.FEncoding <> eUTF8 then break;
          end;
        end;
        el := el.next;
      end;
    end;

  end;

  FTrees.Add(FCurrentTree);
  result := FCurrentTree;
  FCurrentNamespaces.clear;
  FCurrentNamespaceDefinitions.Clear;
  if FTargetEncoding <> eUnknown then
    FCurrentTree.setEncoding(FTargetEncoding, true, true);
//  if FRootElement = nil then
//    raise ETemplateParseException.Create('Ungültiges/Leeres Template');
end;

function TTreeParser.parseTreeFromFile(filename: string): TTreeDocument;
begin
  result := parseTree(strLoadFromFile(filename), filename);
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
  temp: TTreeElement;
begin
  temp := getLastTree;
  if temp = nil then exit;
  while temp.next <> nil do begin
    while (temp.next <> nil) and (temp.next.typ = tetText) and ( (temp.next.value = '') or (whenTrimmed and (strIsEmpty(temp.next.value)))) do
      temp.removeAndFreeNext();
    temp := temp.next;
  end;
end;


function xmlStrEscape(s: string):string;
var
  i, p: Integer;
  procedure push(const t:string); inline;
  begin
    if p + length(t) > length(result) + 1 then setlength(result, length(result) + 64);
    move(t[1], result[p], length(t));
    p+=length(t);
  end;

begin
  setlength(result, length(s));
  p:=1;
  for i := 1 to length(s) do begin
    case s[i] of
      '<': push('&lt;');
      '>': push('&gt;');
      '&': push('&amp;');
      '''': push('&apos;');
      '"': push('&quot;');
      #13: push('&#xD;');
      else begin
        if p > length(result) then setlength(result, length(result) + 64);
        result[p] := s[i];
        p+=1;
      end;
    end;
  end;
  setlength(result, p - 1);
end;

function equalNamespaces(const ans, bns: INamespace): boolean;
begin
  result := (ans = bns) or ((ans <> nil) and (bns <> nil) and (ans.getURL = bns.getURL));
end;

initialization
  XMLNamespace_XML := TNamespace.Create(XMLNamespaceUrl_XML, 'xml');
  XMLNamespace_XMLNS := TNamespace.Create(XMLNamespaceUrl_XMLNS, 'xmlns');
finalization
  XMLNamespace_XML := nil;  //prevent heaptrc warning
  XMLNamespace_XMLNS := nil;
end.

