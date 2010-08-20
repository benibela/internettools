unit simplehtmltreeparser;

{$mode objfpc}

interface

uses
  Classes, SysUtils, simplehtmlparser, bbutils;

type
TTreeElementType = (tetOpen, tetClose, tetText);

{ TTreeElement }
TTreeElement = class
  typ: TTreeElementType;
  text: string;
  attributes: TStringList;  //nil für tetText
  next: TTreeElement; //next element as in the file (first child if there are childs, else next on lowest level) so elements form a linked list
  reverse: TTreeElement; //element paired by open/closing

  offset: longint;

  procedure deleteNext();
  procedure deleteAll();

  function toString(): string;

  constructor create();
  destructor destroy();override;
end;
TTreeElementClass = class of TTreeElement;

TreeParseException = Exception;
{ TTreeParser }

//**Parsing model used to interpret the document
//**pmStrict: every tag must be closed explicitely (otherwise an exception is raised)
//**pmHtml: accept everything, tries to create the best fitting tree using a heuristic to recover from faulty documents (no exceptions are raised)
TParsingModel = (pmStrict, pmHTML);
//**This parses a html/sgml/xml file to a tree like structure
//**The data structure is like a stream of annotated tokens with back links (so you can traverse it like a tree)
TTreeParser = class
private
  FRootElement: TTreeElement;
  FCurrentElement: TTreeElement;
  FTemplateCount: Integer;
  FElementStack: TList;
  FAutoCloseTag: boolean;
  FCurrentFile, FCurrentFileName: string;
  FParsingModel: TParsingModel;
  FTrimText: boolean;

  function newTreeElement(typ:TTreeElementType; text: pchar; len:longint):TTreeElement;
  procedure autoCloseLastTag();

  function enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
  function leaveTag(tagName: pchar; tagNameLen: longint):boolean;
  function readText(text: pchar; textLen: longint):boolean;


  function htmlTagWeight(s:string): integer;
  function htmlTagAutoClosed(s:string): boolean;
public
  treeElementClass: TTreeElementClass;

  constructor Create;
  destructor destroy;override;
  procedure clearTree;
  procedure parseTree(html: string; fileName:string='');

  function getTree: TTreeElement;

published
  property parsingModel: TParsingModel read FParsingModel write FParsingModel;
  property trimText: boolean read FTrimText write FTrimText;
end;
implementation


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

function TTreeElement.toString(): string;
var
  i: Integer;
begin
  case typ of
    tetText: exit(text);
    tetClose: exit('</'+text+'>');
    tetOpen: begin
        result := '<'+text;
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



{ THTMLTreeParser }

function TTreeParser.newTreeElement(typ:TTreeElementType; text: pchar; len: longint): TTreeElement;
begin
  if FRootElement = nil then begin
    FRootElement:=treeElementClass.create;
    result:=FRootElement;
  end else begin
    result:=treeElementClass.Create;
  end;
  result.typ := typ;
  result.text:=strFromPchar(text,len);
  FTemplateCount+=1;
  result.offset:=longint(text - @FCurrentFile[1]);

  if FCurrentElement <> nil then
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
  new := treeElementClass.create();
  new.typ:=tetClose;
  new.text:=last.text;
  new.next:=last.next;
  last.next:=new;
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
    FAutoCloseTag:=htmlTagAutoClosed(new.text);
  FElementStack.Add(new);
  if length(properties)>0 then begin
    new.attributes:=TStringList.Create;
    for i:=0 to high(properties) do
      with properties[i] do
        new.attributes.Add(trim(strFromPchar(name,nameLen))+'='+
                                               trim(strFromPchar(value,valueLen)));
  end;
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

  if FAutoCloseTag and (not strliequal(tagName, last.text, tagNameLen)) then autoCloseLastTag();

  if (strliequal(tagName, last.text, tagNameLen)) then begin
    new := newTreeElement(tetClose, tagName, tagNameLen);
    new.reverse := last; last.reverse := new;
    FElementStack.Delete(FElementStack.Count-1);
  end else if FParsingModel = pmStrict then
    raise TreeParseException.Create('The tag <'+new.text+'> was closed, but the latest opened was <'+last.text+'>')
  else if FParsingModel = pmHTML then begin
    //try to auto detect unclosed tags
    match:=-1;
    for i:=FElementStack.Count-1 downto 0 do
      if strliequal(tagName, TTreeElement(FElementStack[i]).text, tagNameLen) then begin
        match:=i;
        break;
      end;
    if match > -1 then begin
      weight := htmlTagWeight(strFromPchar(tagName, tagNameLen));
      for i:=match+1 to FElementStack.Count-1 do
        if htmlTagWeight(TTreeElement(FElementStack[i]).text) > weight then
            exit;
      for i:=match+1 to FElementStack.Count-1 do
        autoCloseLastTag();
      new := newTreeElement(tetClose, tagName, tagNameLen);
      last := TTreeElement(FElementStack[match]);
      last.reverse := new; new.reverse := last;
      FElementStack.Count:=match;
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

  newTreeElement(tetText, text, textLen);
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


procedure TTreeParser.parseTree(html: string; fileName: string);
begin
  //FVariables.clear;
  clearTree;
  if html='' then exit;

  FCurrentFile:=html;
  FCurrentFileName:=fileName;
  FAutoCloseTag:=false;
  simplehtmlparser.parseHTML(FCurrentFile,@enterTag, @leaveTag, @readText);

//  if FRootElement = nil then
//    raise ETemplateParseException.Create('Ungültiges/Leeres Template');
end;

function TTreeParser.getTree: TTreeElement;
begin
  result := FRootElement;
end;

end.

