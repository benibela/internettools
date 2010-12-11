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
TTreeElementType = (tetOpen, tetClose, tetText);
//**Controls the search for a tree element.@br
//**ignore type: do not check for a matching type, ignore text: do not check for a matching text,
//**case sensitive: do not ignore the case, no descend: only check elements that direct children of the current node
TTreeElementFindOptions = set of (tefoIgnoreType, tefoIgnoreText, tefoCaseSensitive, tefoNoDescend);

{ TTreeElement }

//**@abstract This class representates an element of the html file
//**It is stored in an unusual  tree representation: All elements form a linked list and the next element is the first children, or if there is none, the next node on the same level, or if there is none, the closing tag of the current parent.@br
//**There are functions (getNextSibling, getFirstChild, findNext, ...) to access it like a regular tree, but it is easier and faster to work directly with the list.@br
//**Some invariants: (SO: set of opening tags in sequence)@br
//**∀a \in SO: a < a.reverse@br
//**∀a,b \in SO: a < b < a.reverse => a < b.reverse < a.reverse@br
TTreeElement = class
//use the fields if you know what you're doing
  typ: TTreeElementType; //**<open, close or text node
  value: string; //**< tag name for open/close nodes, text for text nodes
  attributes: TAttributeList;  //**<nil für tetText
  next: TTreeElement; //**<next element as in the file (first child if there are childs, else next on lowest level), so elements form a linked list
  reverse: TTreeElement; //**<element paired by open/closing

  offset: longint; //**<count of characters in the document before this element (so document_pchar + offset begins with value)

//otherwise use the functions
  procedure deleteNext(); //**<delete the next node (you have to delete the reverse tag manually)
  procedure deleteAll(); //**<deletes the tree
  procedure changeEncoding(from,toe: TEncoding; substituteEntities: boolean); //**<converts the tree encoding from encoding from to toe, and substitutes entities (e.g &auml;)


  //Complex search functions.
  //**Returns the element with the given type and text which occurs before sequenceEnd.@br
  //**This function is nil-safe, so if you call TTreeElement(nil).findNext(...) it will return nil
  function findNext(withTyp: TTreeElementType; withText:string; findOptions: TTreeElementFindOptions=[]; sequenceEnd: TTreeElement = nil):TTreeElement;
  //**Find a matching direct child (equivalent to findNext with certain parameters, but easier to use)
  function findChild(withTyp: TTreeElementType; withText:string; findOptions: TTreeElementFindOptions=[]): TTreeElement;

  function deepNodeText(separator: string=''):string; //**< concatenates the text of all (including indirect) text children

  function getValue(): string; //**< get the value of this element
  function getAttribute(a: string):string; //**< get the value of an attribute of this element or '' if this attribute doesn't exists
  function getNextSibling(): TTreeElement; //**< Get the next element on the same level or nil if there is none
  function getFirstChild(): TTreeElement; //**< Get the first child, or nil if there is none
  function getParent(): TTreeElement; //**< Searchs the parent, notice that this is a slow function (neither the parent nor previous elements are stored in the tree, so it has to search the last sibling)

  function toString(): string; //**< converts the element to a string (not recursive)

  constructor create();
  destructor destroy();override;
  procedure initialized; virtual; //**<is called after an element is read, before the next one is read (therefore all fields are valid except next (and reverse for opening tags))
end;
TTreeElementClass = class of TTreeElement;

TreeParseException = Exception;
{ TTreeParser }

//**Parsing model used to interpret the document
//**pmStrict: every tag must be closed explicitely (otherwise an exception is raised)
//**pmHtml: accept everything, tries to create the best fitting tree using a heuristic to recover from faulty documents (no exceptions are raised), detect encoding
TParsingModel = (pmStrict, pmHTML);
//**@abstract This parses a html/sgml/xml file to a tree like structure
//**The data structure is like a stream of annotated tokens with back links (so you can traverse it like a tree).@br
//**After tree parsing the tree contains the text as byte strings, without encoding or entity conversions. But in the case of html, the meta/http-equiv encoding is detected
//**and you can call setEncoding to change the tree to the encoding you need. (this will also convert the entities)
TTreeParser = class
private
//  FConvertEntities: boolean;
  FRootElement: TTreeElement;
  FCurrentElement: TTreeElement;
  FTemplateCount: Integer;
  FElementStack: TList;
  FAutoCloseTag: boolean;
  FCurrentFile: string;
  FParsingModel: TParsingModel;
  FTrimText: boolean;
  FEncoding: TEncoding;

  function newTreeElement(typ:TTreeElementType; text: pchar; len:longint):TTreeElement;
  function newTreeElement(typ:TTreeElementType; s: string):TTreeElement;
  procedure autoCloseLastTag();

  function enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
  function leaveTag(tagName: pchar; tagNameLen: longint):boolean;
  function readText(text: pchar; textLen: longint):boolean;


  function htmlTagWeight(s:string): integer;
  function htmlTagAutoClosed(s:string): boolean;
public
  treeElementClass: TTreeElementClass; //**< Class of the tree nodes. You can subclass TTreeElement if you need to store additional data at every node

  constructor Create;
  destructor destroy;override;
  procedure clearTree; //**< Deletes the current tree
  procedure parseTree(html: string); //**< Creates a new tree from a html file

  function getTree: TTreeElement; //**< Returns the current tree


  //**Returns the current encoding of the tree. After the parseTree-call it is the detected encoding, but it can be overriden with setEncoding.
  function getEncoding: TEncoding;
  //**Changes the tree encoding
  //**If convertExistingTree is true, the strings of the tree are actually converted, otherwise only the meta encoding information is changed
  //**If convertEntities is true, entities like &ouml; are replaced (which is only possible if the encoding is known)
  procedure setEncoding(new: TEncoding; convertExistingTree: Boolean = true; convertEntities: boolean =true);
published
  //** Parsing model, see TParsingModel
  property parsingModel: TParsingModel read FParsingModel write FParsingModel;
  //** If this is true (default), white space is removed from text node
  property trimText: boolean read FTrimText write FTrimText;
//  property convertEntities: boolean read FConvertEntities write FConvertEntities;
end;
implementation
uses pseudoxpath;

{ TTreeElement }

procedure TTreeElement.deleteNext();
var
  temp: TTreeElement;
begin
  if next = nil then exit;
  temp := next;
  next := next.next;
  temp.Free;
end;

procedure TTreeElement.deleteAll();
begin
  if next <> nil then next.deleteAll();
  next:=nil;
  Free;
end;

procedure TTreeElement.changeEncoding(from, toe: TEncoding; substituteEntities: boolean);
var tree: TTreeElement;
  s: String;
begin
  if (from = eUnknown) or (toe = eUnknown) then exit;
  if (from = toe) and not substituteEntities then exit;
  tree := self;
  while tree <> nil do begin
    if tree.typ = tetText then begin
      tree.value:=strChangeEncoding(tree.value, from, toe);
      if substituteEntities then tree.value:=strDecodeHTMLEntities(tree.value, toe, false);
    end
    else if tree.attributes <> nil then begin
      s :=strChangeEncoding(tree.attributes.text,from,toe);
      if substituteEntities then s:=strDecodeHTMLEntities(s, toe, false);
      tree.attributes.text:=s;
    end;
    //TODO: convert tree tag names (but this is not necessary as long as only latin1 and utf8 is supported)
    tree := tree.next;
  end;
end;

function TTreeElement.findNext(withTyp: TTreeElementType; withText: string; findOptions: TTreeElementFindOptions =[]; sequenceEnd: TTreeElement = nil): TTreeElement;
var cur: TTreeElement;
  splitted: array of string;
begin
  if self = nil then exit;
  {if (tefoSplitSlashes in findOptions) and not (tefoIgnoreType in findOptions) and not (tefoIgnoreText in findOptions) and (withTyp = tetOpen) and (pos('/'.withText) > 0) then begin
    result := findNext(tetOpen, strSplitGet('/', withText), findOptions - [tefoSplitSlashes], sequenceEnd);
    while (result <> nil) and (withText <> '') do
      result := result.findNext(tetOpen, strSplitGet('/', withText), findOptions - [tefoSplitSlashes], result.reverse);
    exit();
  end;}
  cur := self.next;
  while (cur <> nil) and (cur <> sequenceEnd) do begin
    if ((cur.typ = withTyp) or (tefoIgnoreType in findOptions)) and
       ((tefoIgnoreText in findOptions) or
           ( (tefoCaseSensitive in findOptions) and (cur.value = withText) ) or
           ( not (tefoCaseSensitive in findOptions) and (striequal(cur.value, withText) ) ) ) then
             exit(cur);
    if (tefoNoDescend in findOptions) and (cur.typ = tetOpen) then cur := cur.reverse
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
  result:=findNext(withTyp, withText, findOptions + [tefoNoDescend], reverse);
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

function TTreeElement.getValue(): string;
begin
  if self = nil then exit('');
  result := value;
end;

function TTreeElement.getAttribute(a: string):string;
begin
  if attributes = nil then exit('');
  exit(attributes.Values[a]);
end;

function TTreeElement.getNextSibling(): TTreeElement;
begin
  case typ of
    tetText: result:=next;
    tetOpen: result:=reverse.next;
    tetClose: result:=next;
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
var
  cur: TTreeElement;
  open: longint;
begin
  cur := self;
  open := 1;
  while cur <> nil do begin
    if cur.typ = tetOpen then open+=1
    else if cur.typ = tetClose then begin
      open-=1;
      if open = 0 then exit(cur.reverse);
    end;
    cur := cur.next;
  end;
  exit(nil);
end;

function TTreeElement.toString(): string;
var
  i: Integer;
begin
  if self = nil then exit('');
  case typ of
    tetText: exit(value);
    tetClose: exit('</'+value+'>');
    tetOpen: begin
        result := '<'+value;
        if attributes <> nil then
          for i:=0 to attributes.Count-1 do
            result += ' '+attributes[i];
        result+='>';
    end;
  end;
end;

constructor TTreeElement.create();
begin

end;

destructor TTreeElement.destroy();
begin
  attributes.Free;
  inherited destroy();
end;

procedure TTreeElement.initialized;
begin

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
  FCurrentElement := result;
  //FCurrentElement.id:=FTemplateCount;
end;

procedure TTreeParser.autoCloseLastTag();
var
  last: TTreeElement;
  new: TTreeElement;
begin
  last := TTreeElement(FElementStack.Last);
  Assert(last<>nil);
  new := newTreeElement(tetClose, last.value);
  //new := treeElementClass.create();
  //new.typ:=tetClose;
  //new.value:=last.value;
  new.offset:=last.offset;
  //new.next:=last.next;
  //last.next:=new;
  last.reverse:=new; new.reverse:=last;
  FElementStack.Delete(FElementStack.Count-1);
  FAutoCloseTag:=false;
end;

function TTreeParser.enterTag(tagName: pchar; tagNameLen: longint;
  properties: THTMLProperties): boolean;
var
  new: TTreeElement;
  i: Integer;
begin
  result:=true;

  if FAutoCloseTag then autoCloseLastTag();
  new := newTreeElement(tetOpen, tagName, tagNameLen);
  if (FParsingModel = pmHTML) then
    FAutoCloseTag:=htmlTagAutoClosed(new.value);
  FElementStack.Add(new);
  if length(properties)>0 then begin
    new.attributes:=TAttributeList.Create;
    for i:=0 to high(properties) do
      with properties[i] do
        new.attributes.Add(trim(strFromPchar(name,nameLen))+'='+
                                               trim(strFromPchar(value,valueLen)));
  end;
  new.initialized;
end;

function TTreeParser.leaveTag(tagName: pchar; tagNameLen: longint): boolean;
var
  new,last: TTreeElement;
  match: longint;
  i: Integer;
  weight: LongInt;
begin
  result:=true;

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
    raise TreeParseException.Create('The tag <'+strFromPchar(tagName,tagNameLen)+'> was closed, but the latest opened was <'+last.value+'>')
  else if FParsingModel = pmHTML then begin
    //try to auto detect unclosed tags
    match:=-1;
    for i:=FElementStack.Count-1 downto 0 do
      if strliequal(tagName, TTreeElement(FElementStack[i]).value, tagNameLen) then begin
        match:=i;
        break;
      end;
    if match > -1 then begin
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
    //if no opening tag can be found the closing tag is ignored (not contained in tree)
  end;
end;

function TTreeParser.readText(text: pchar; textLen: longint): boolean;
begin
  result:=true;

  if FAutoCloseTag then
    autoCloseLastTag();

  if FTrimText then
    strlTrim(text, textLen, [' ',#0,#9,#10,#13]);

  if textLen = 0 then
    exit;

  newTreeElement(tetText, text, textLen).initialized;
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
  //FConvertEntities := true;
end;

destructor TTreeParser.destroy;
begin
  clearTree;
  FElementStack.free;
  inherited destroy;
end;

procedure TTreeParser.clearTree;
begin
  FTemplateCount:=0;
  if FRootElement<>nil then FRootElement.deleteAll();
  FRootElement:=nil;
  FElementStack.Clear;
end;


procedure TTreeParser.parseTree(html: string);
var
  encoding: String;
begin
  //FVariables.clear;
  clearTree;
  if html='' then exit;

  FCurrentFile:=html;
  FAutoCloseTag:=false;

  //initialize root element
  //there are two reasons for an empty root element which doesn't exists in the file
  //1. it is necessary for the correct interpretion of xpath expressions html/... assumes
  //   that the current element is a parent of html
  //2. it serves as parent for multiple top level elements (althought they aren't allowed)
  FRootElement:=treeElementClass.create;
  FRootElement.typ := tetOpen;
  FCurrentElement:=FRootElement;
  FElementStack.Clear;
  FElementStack.Add(FCurrentElement);
  FTemplateCount:=1;

  //parse
  if FParsingModel = pmHTML then simplehtmlparser.parseHTML(FCurrentFile,@enterTag, @leaveTag, @readText)
  else simplehtmlparser.parseML(FCurrentFile,[],@enterTag, @leaveTag, @readText);

  //close root element
  leaveTag('',0);

  if parsingModel = pmHTML then begin
    FEncoding:=eUnknown;
    encoding := lowercase(TPseudoXPathParser.Evaluate('html/head/meta[@http-equiv=''content-type'']/@content', FRootElement));
    if encoding <> '' then begin
      if pos('charset=utf-8', encoding) > 0 then FEncoding:=eUTF8
      else if (pos('charset=windows-1252',encoding) > 0) or
              (pos('charset=iso-8859-1',encoding) > 0) then
        FEncoding:=eWindows1252;
    end;

  end;
//  if FRootElement = nil then
//    raise ETemplateParseException.Create('Ungültiges/Leeres Template');
end;

function TTreeParser.getTree: TTreeElement;
begin
  result := FRootElement;
end;

function TTreeParser.getEncoding: TEncoding;
begin
  exit(FEncoding);
end;

procedure TTreeParser.setEncoding(new: TEncoding; convertExistingTree,
  convertEntities: boolean);
begin
  if FRootElement = nil then exit;
  if (FEncoding = eUnknown) or not convertExistingTree then FEncoding:= new;
  if convertExistingTree or convertEntities then FRootElement.changeEncoding(FEncoding, new, convertEntities);
  FEncoding := new;
end;

end.

