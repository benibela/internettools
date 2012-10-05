{**
  @abstract This unit contains a html/xml -> tree converter

  @author Benito van der Zander (http://www.benibela.de)
}
unit simplehtmltreeparser;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, simplehtmlparser, bbutils;

type

{ TAttributeMap }

//**@abstract A list of attributes.
//**Currently this is a simple string list, and you can get the values with the values property. (with c++ I would have used map<string, string> but this doesn't exist in Pascal)
TAttributeList = TStringList; //TODO: use a map

//**The type of a tree element. <Open>, text, or </close>
TTreeElementType = (tetOpen, tetClose, tetText, tetComment, tetProcessingInstruction, tetAttributeValue, tetAttributeName);
//**Controls the search for a tree element.@br
//**ignore type: do not check for a matching type, ignore text: do not check for a matching text,
//**case sensitive: do not ignore the case, no descend: only check elements that direct children of the current node
TTreeElementFindOptions = set of (tefoIgnoreType, tefoIgnoreText, tefoCaseSensitive, tefoNoChildren, tefoNoGrandChildren);

TTreeParser = class;
TTreeDocument = class;

{ TTreeElement }

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
  attributes: TTreeElement;  //**<nil if there are no attributes
  next: TTreeElement; //**<next element as in the file (first child if there are childs, else next on lowest level), so elements form a linked list
  previous: TTreeElement; //**< previous element (self.next.previous = self)
  parent: TTreeElement;
  reverse: TTreeElement; //**<element paired by open/closing, or corresponding attributes

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
  function hasAttribute(const a: string): boolean; //**< returns if an attribute with that name exists
  function getAttribute(const a: string):string; //**< get the value of an attribute of this element or '' if this attribute doesn't exist
  function getAttribute(const a: string; const def: string):string; //**< get the value of an attribute of this element or '' if this attribute doesn't exist
  function getAttributeTry(const a: string; out valueout: string):boolean; //**< get the value of an attribute of this element and returns false if it doesn't exist
  function getAttributeTry(const a: string; out valueout: TTreeElement):boolean; //**< get the value of an attribute of this element and returns false if it doesn't exist
  function getNextSibling(): TTreeElement; //**< Get the next element on the same level or nil if there is none
  function getFirstChild(): TTreeElement; //**< Get the first child, or nil if there is none
  function getParent(): TTreeElement; //**< Searchs the parent, notice that this is a slow function (neither the parent nor previous elements are stored in the tree, so it has to search the last sibling)
  function getPrevious(): TTreeElement; //**< Searchs the previous, notice that this is a slow function (neither the parent nor previous elements are stored in the tree, so it has to search the last sibling)
  function getRoot(): TTreeElement;
  function getDocument(): TTreeDocument;

  property defaultProperty[name: string]: string read getAttribute; default;


  procedure insert(el: TTreeElement); //**< inserts el after the current element (does only change next+previous, not reverse+parent)
  procedure insertSurrounding(before, after: TTreeElement); //**< Surrounds self by before and after, i.e. inserts "before" directly before the element and "after" directly after its closing tag (slow)
  procedure insertSurrounding(basetag: TTreeElement); //**< inserts basetag before the current tag, and creates a matching closing tag after the closing tag of self (slow)

  function addAttribute(aname, avalue: string): TTreeElement; //adds a single attribute. Returns the element of the last inserted attribute. (runs in O(|attributes|) => do not use for multiple attributes)
  function addAttributes(const props: array of THTMLProperty): TTreeElement; //adds an array of properties to the attributes. Returns the element of the last inserted attribute.

  procedure removeElementFromDoubleLinkedList; //removes the element from the double linked list (only updates previous/next)
  function deleteElementFromDoubleLinkedList: TTreeElement; //removes the element from the double linked list (only updates previous/next), frees it and returns next  (mostly useful for attribute nodes)

protected
  procedure removeAndFreeNext(); //**< removes the next element (the one following self). (ATTENTION: looks like there is a memory leak for opened elements)
  procedure removeElementKeepChildren; //**< removes/frees the current element, but keeps the children (i.e. removes self and possible self.reverse. Will not remove the opening tag, if called on a closing tag)

public
  function toString(): string; reintroduce; //**< converts the element to a string (not recursive)

  constructor create();
  constructor create(atyp: TTreeElementType; avalue: string = '');
  destructor destroy();override;
  procedure initialized; virtual; //**<is called after an element is read, before the next one is read (therefore all fields are valid except next (and reverse for opening tags))


  class function compareInDocumentOrder(p1, p2: Pointer): integer;
end;
TTreeElementClass = class of TTreeElement;

{ TTreeDocument }

TTreeDocument = class(TTreeElement)
protected
  FEncoding: TEncoding;
  FBaseURI: string;
  FCreator: TTreeParser;

public
  property baseURI: string read FBaseURI;

  function getCreator: TTreeParser;

  //**Returns the current encoding of the tree. After the parseTree-call it is the detected encoding, but it can be overriden with setEncoding.
  function getEncoding: TEncoding;
  //**Changes the tree encoding
  //**If convertExistingTree is true, the strings of the tree are actually converted, otherwise only the meta encoding information is changed
  //**If convertEntities is true, entities like &ouml; are replaced (which is only possible if the encoding is known)
  procedure setEncoding(new: TEncoding; convertFromOldToNew: Boolean; convertEntities: boolean);

  destructor destroy; override;
end;

TreeParseException = Exception;
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


  function htmlTagWeight(s:string): integer;
  function htmlTagAutoClosed(s:string): boolean;
public
  treeElementClass: TTreeElementClass; //**< Class of the tree nodes. You can subclass TTreeElement if you need to store additional data at every node

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
end;


function xmlStrEscape(s: string):string;
implementation
uses pseudoxpath;

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
    if substituteEntities then result := strDecodeHTMLEntities(result, toe, false);
    if trimText then result := trim(result); //retrim because &nbsp; replacements could have introduced new spaces
  end;

var tree: TTreeElement;
  s: String;
  attrib: TTreeElement;
begin
  if (from = eUnknown) or (toe = eUnknown) then exit;
  if (from = toe) and not substituteEntities then exit;
  tree := self;
  while tree <> nil do begin
    case tree.typ of
      tetText, tetProcessingInstruction: tree.value := change(tree.value);
      tetComment: tree.value:=strChangeEncoding(tree.value, from, toe);
      tetOpen, tetClose: begin
        tree.value := change(tree.value);
        attrib := tree.attributes;
        while attrib <> nil do begin
          attrib.value := change(attrib.value);
          assert(attrib.reverse <> nil);
          attrib.reverse.value := change(attrib.reverse.value);
          attrib := attrib.next;
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
  if (tefoNoChildren in findOptions) and (self.typ = tetOpen) then cur := self.reverse
  else cur := self.next;
  while (cur <> nil) and (cur <> sequenceEnd) do begin
    if ((cur.typ = withTyp) or (tefoIgnoreType in findOptions)) and
       ((tefoIgnoreText in findOptions) or
           ( (tefoCaseSensitive in findOptions) and (cur.value = withText) ) or
           ( not (tefoCaseSensitive in findOptions) and (striequal(cur.value, withText) ) ) ) then
             exit(cur);
    if (tefoNoGrandChildren in findOptions) and (cur.typ = tetOpen) then cur := cur.reverse
    else cur := cur.next;
  end;
  result := nil;
end;

function TTreeElement.findChild(withTyp: TTreeElementType; withText: string;
  findOptions: TTreeElementFindOptions): TTreeElement;
begin
  result := nil;
  if self = nil then exit;
  if typ <> tetOpen then exit;
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
var i: Integer;
  attrib: TTreeElement;
begin
  if self = nil then exit;
  case typ of
    tetText: result := xmlStrEscape(value);
    tetClose: result := '</'+value+'>';
    tetComment: result := '<!--'+value+'-->';
    tetProcessingInstruction: begin
      result := '<?'+value;
      if attributes <> nil then result += ' '+attributes.reverse.value;
      result += '?>';
    end;
    tetOpen: begin
      if (value = '') and (self is TTreeDocument) then exit(innerXML());
      result := '<'+value;
      attrib := attributes;
      while attrib <> nil do begin
        assert(attrib.reverse <> nil);
        result += ' ' + attrib.value+'="'+attrib.reverse.value+'"'; //todo fix escaping & < >
        attrib := attrib.next;
      end;

      if next = reverse then begin
        result += '/>';
        if insertLineBreaks then Result+=LineEnding;
        exit();
      end;
      result +='>';
      if insertLineBreaks then Result+=LineEnding;
      result += innerXML(insertLineBreaks);
      result+='</'+value+'>';
      if insertLineBreaks then Result+=LineEnding;
    end;
  end;
end;

function TTreeElement.innerXML(insertLineBreaks: boolean = false): string;
var
  sub: TTreeElement;
begin
  result := '';
  if (self = nil) or (typ <> tetOpen) then exit;
  sub := next;
  while sub <> reverse do begin
    result += sub.outerXML(insertLineBreaks);
    if sub.typ <> tetOpen then sub:=sub.next
    else sub := sub.reverse.next;
  end;
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

function TTreeElement.hasAttribute(const a: string): boolean;
var temp: TTreeElement;
begin
  exit(getAttributeTry(a, temp));
end;

function TTreeElement.getAttribute(const a: string): string;
begin
  if not getAttributeTry(a, result) then
    result:='';
end;

function TTreeElement.getAttribute(const a: string; const def: string):string;
begin
  if not getAttributeTry(a, result) then
    result:=def;
end;

function TTreeElement.getAttributeTry(const a: string; out valueout: string): boolean;
var temp: TTreeElement;
begin
  result := getAttributeTry(a, temp);
  if not result then exit;
  valueout := temp.value;
end;

function TTreeElement.getAttributeTry(const a: string; out valueout: TTreeElement): boolean;
var
  attrib: TTreeElement;
begin
  result := false;
  if self = nil then exit;
  attrib := attributes;
  while attrib <> nil do begin
    if striEqual(attrib.value, a) then begin //Todo: case sensitive or insensitive??
      valueout := attrib.reverse;
      exit(true);
    end;
    attrib := attrib.next;
  end;
end;

function TTreeElement.getNextSibling(): TTreeElement;
begin
  case typ of
    tetOpen: result:=reverse.next;
    tetText, tetClose, tetComment, tetProcessingInstruction: result := next;
    else raise Exception.Create('Invalid tree element type');
  end;
  if result.typ = tetClose then exit(nil);
end;

function TTreeElement.getFirstChild(): TTreeElement;
begin
  if typ <> tetOpen then exit(nil);
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

function TTreeElement.getRoot: TTreeElement;
begin
  result := self;
  if result = nil then exit;
  if result.parent = nil then //document node
    exit(findChild(tetOpen,'',[tefoIgnoreText]));

  while (result <> nil) and (result.previous <> nil) and (result.parent.parent <> nil) do
    result := result.parent;
end;

function TTreeElement.getDocument: TTreeDocument;
begin
  result := TTreeDocument(getRoot().getParent());
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

  if surroundee.typ = tetOpen then surroundee.reverse.insert(after)
  else surroundee.insert(after);

  before.reverse := after;
  after.reverse := before;

  if (before.typ = tetOpen) and (before.reverse = after) then begin
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
  if basetag.typ <> tetOpen then raise Exception.Create('Need an opening tag to surround another tag');
  closing := TTreeElement(basetag.ClassType.Create);
  closing.typ := tetClose;
  closing.value := basetag.value;
  insertSurrounding(basetag, closing);
end;

function TTreeElement.addAttribute(aname, avalue: string): TTreeElement;
var temp: THTMLProperties;
begin
  SetLength(temp, 1);
  temp[0].name:=pchar(aname);
  temp[0].nameLen:=length(aname);
  temp[0].value:=pchar(avalue);
  temp[0].valueLen:=length(avalue);
  addAttributes(temp);
end;

function TTreeElement.addAttributes(const props: array of THTMLProperty): TTreeElement;
var
  prev: TTreeElement;
  attrib: TTreeElement;
  i: Integer;
  newoffset: Integer;
begin
  if length(props) = 0 then exit(nil);

  newoffset := offset + 1;

  prev := attributes;
  if prev <> nil then begin
    while prev.next <> nil do
      prev := prev.next;
    newoffset := prev.offset + 1;
  end;


  for i := 0 to high(props) do begin
    attrib := TTreeElement.create(tetAttributeName, strFromPchar(props[i].name, props[i].nameLen));
    attrib.parent := self;
    if prev <> nil then prev.next := attrib
    else attributes := attrib;
    attrib.previous := prev;
    attrib.offset:=newoffset; //offset hack to sort attributes after their parent elements in result sequence


    attrib.reverse := TTreeElement.create(tetAttributeValue, strFromPchar(props[i].value, props[i].valueLen));
    attrib.reverse.parent := self;
    attrib.reverse.reverse := attrib;
    if prev <> nil then begin
      prev.reverse.next := attrib.reverse ;
      attrib.reverse.previous := prev.reverse;
    end;
    attrib.reverse.offset:=newoffset;

    newoffset+=1;
    prev := attrib;
  end;

  exit(attrib);


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

procedure TTreeElement.removeAndFreeNext();
var
  toFree: TTreeElement;
  temp: TTreeElement;
begin
  if (self = nil) or (next = nil) then exit;
  toFree := next;
  if toFree.typ = tetOpen then begin
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
  if typ = tetOpen then begin
    temp := next;
    while temp <> reverse do begin
      if temp.parent = self then temp.parent := parent;
      temp := temp.getNextSibling();
    end;
    reverse.removeElementKeepChildren;
  end;
  free;
end;

function TTreeElement.toString(): string;
var
  i: Integer;
  attrib: TTreeElement;
begin
  if self = nil then exit('');
  case typ of
    tetText: exit(value);
    tetClose: exit('</'+value+'>');
    tetOpen: begin
      if (value = '') and (self is TTreeDocument) then exit(innerXML());
      result := '<'+value;
      attrib := attributes;
      while attrib <> nil do begin
        result += ' '+attrib.value + '="'+attrib.reverse.value+'"';
        attrib := attrib.next;
      end;
      result+='>';
    end;
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

class function TTreeElement.compareInDocumentOrder(p1, p2: Pointer): integer;
begin
  if TTreeElement(p1).offset < TTreeElement(p2).offset then exit(-1)
  else if TTreeElement(p1).offset > TTreeElement(p2).offset then exit(1)
  else if p1 = p2 then exit(0)
  else raise Exception.Create('invalid comparison');
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

      new.addAttribute('', strFromPchar(first, last-first));
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
        if not (temp.typ in  [tetOpen, tetClose]) then continue;
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
        if not (temp.typ in  [tetOpen, tetClose]) then continue;
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
  if length(properties)>0 then new.addAttributes(properties);
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
begin
  result:=prContinue;

  last := TTreeElement(FElementStack.Last);
  if (FParsingModel = pmStrict) and (last = nil) then
    raise TreeParseException.create('The tag <'+strFromPchar(tagName,tagNameLen)+'> was closed, but none was open');

  if last = nil then exit;

  if FAutoCloseTag and (not strliequal(tagName, last.value, tagNameLen)) then autoCloseLastTag();
  FAutoCloseTag:=false;

  if (strliequal(tagName, last.value, tagNameLen)) then begin
    new := newTreeElement(tetClose, tagName, tagNameLen);
    new.reverse := last; last.reverse := new;
    FElementStack.Delete(FElementStack.Count-1);
    new.initialized;
  end else if FParsingModel = pmStrict then
    raise TreeParseException.Create('The tag <'+strFromPchar(tagName,tagNameLen)+'> was closed, but the latest opened was <'+last.value+'>  (url: '+FCurrentTree.FBaseURI+')')
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
end;

function TTreeParser.readText(text: pchar; textLen: longint): TParsingResult;
begin
  result:=prContinue;

  if (FParsingModel = pmStrict) and (FElementStack.Count < 2) then begin
    strlTrimLeft(text, textLen);
    if textLen = 0 then exit;
    if strBeginsWith(text, #239#187#191) or strBeginsWith(text,#254#255) or strBeginsWith(text, #255#254) or
       strBeginsWith(text, #43#47#118) then raise Exception.Create('xml ' + FCurrentTree.FBaseURI + ' starts with unicode BOM. That is not supported');
    raise Exception.Create('Data not allowed at root level: '+strFromPchar(text,textLen));
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
  //FConvertEntities := true;
end;

destructor TTreeParser.destroy;
begin
  clearTrees;
  FElementStack.free;
  ftrees.Free;
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
  el, attrib: TTreeElement;
  encMeta, encHeader: TEncoding;
begin
  FTemplateCount:=0;
  FElementStack.Clear;
  FCurrentTree:=nil;

  //FVariables.clear;
  if html='' then exit(nil);

  FCurrentFile:=html;
  FAutoCloseTag:=false;

  //initialize root element
  //there are two reasons for an empty root element which doesn't exists in the file
  //1. it is necessary for the correct interpretion of xpath expressions html/... assumes
  //   that the current element is a parent of html
  //2. it serves as parent for multiple top level elements (althought they aren't allowed)
  FCurrentTree:=TTreeDocument.create;
  FCurrentTree.FCreator:=self;
  FCurrentTree.typ := tetOpen;
  FCurrentTree.FBaseURI:=uri;
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
      encMeta := encodingFromContentType(TPseudoXPathParser.EvaluateToString('html/head/meta[@http-equiv=''content-type'']/@content', FCurrentTree))
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
          tetOpen: begin
            attrib := el.attributes;
            while attrib <> nil do begin
              if isInvalidUTF8(attrib.value) or isInvalidUTF8(attrib.reverse.value) then begin
                FCurrentTree.FEncoding:=eWindows1252;
                break;
              end;
              attrib := attrib.next;
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
  i: Integer;
begin
  result := StringReplace(s, '<', '&lt;', [rfReplaceAll]); //TODO: escape all
end;

end.

