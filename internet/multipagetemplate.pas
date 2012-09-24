{Copyright (C) 2008-2012  Benito van der Zander

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit multipagetemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,bbutils,extendedhtmlparser,simplehtmlparser,simplehtmltreeparser,simplexmlparser, pseudoxpath,dRegExpr,internetaccess;

type

  { TTemplateAction }

  TTemplateReader = class;
  TTemplateAction = class
  protected
    procedure addChildFromTree(t: TTreeElement);
    procedure performChildren(reader: TTemplateReader);
  public
    children: array of TTemplateAction;
    procedure initFromTree(t: TTreeElement); virtual;
    procedure addChildrenFromTree(t: TTreeElement);
    procedure perform(reader: TTemplateReader); virtual; abstract;
    procedure clear;
    destructor Destroy; override;
  end;
  TTemplateActionClass = class of TTemplateAction;

  { TTemplateActionMain }

  TTemplateActionMain = class(TTemplateAction)
    name: string;
    procedure initFromTree(t: TTreeElement); override;
    procedure perform(reader: TTemplateReader); override;
  end;

  { TTemplateActionVariable }

  TTemplateActionVariable = class(TTemplateAction)
    name, value, valuex: string;
    hasValueStr: boolean;
    procedure initFromTree(t: TTreeElement); override;
    procedure perform(reader: TTemplateReader); override;
  end;

  { TTemplateActionLoadPage }

  TTemplateActionLoadPage = class(TTemplateAction)
    url:string;
    templateFile:string;
    template:string;
    postparams:array of TProperty;
    condition: string;
    procedure initFromTree(t: TTreeElement); override;
    procedure perform(reader: TTemplateReader); override;
  end;

  { TTemplateActionCallAction }

  TTemplateActionCallAction = class(TTemplateAction)
    action: string;
    procedure initFromTree(t: TTreeElement); override;
    procedure perform(reader: TTemplateReader); override;
  end;

  { TTemplateActionLoop }

  TTemplateActionLoop = class(TTemplateAction)
    varname, list, test: string;
    procedure initFromTree(t: TTreeElement); override;
    procedure perform(reader: TTemplateReader); override;
  end;

  { TMultiPageTemplate }

  TMultiPageTemplate=class
  protected
    procedure readTree(t: TTreeElement);
  public
    baseActions: TTemplateAction;
    path,name:string;

    //variables: TStringList;

    constructor create();
    procedure loadTemplateFromDirectory(_dataPath: string; aname: string = 'unknown');
    procedure loadTemplateFromString(template: string; aname: string = 'unknown');
    destructor destroy;override;

    function findAction(_name:string): TTemplateAction;
    function findVariableValue(aname: string): string;
    //function getAccountObject():TCustomAccountAccess;override;
  end;

  { TTemplateReader }

  { ETemplateReader }

  ETemplateReader=class(Exception)
    details:string;
    constructor create;
    constructor create(s:string;more_details:string='');
  end;
  TLogEvent = procedure (sender: TTemplateReader; logged: string; debugLevel: integer = 0) of object;
  TPageProcessed = procedure (sender: TTemplateReader; parser: THtmlTemplateParser) of object;

  TTemplateReader = class
  protected
    template:TMultiPageTemplate;
    lastURL: string;
    procedure setTemplate(atemplate: TMultiPageTemplate);
    procedure processPage(page, cururl, contenttype: string); virtual;


  public
    internet:TInternetAccess;
    parser: THtmlTemplateParser;
    onLog: TLogEvent;
    onPageProcessed: TPageProcessed;

    constructor create(atemplate:TMultiPageTemplate; ainternet: TInternetAccess);
    destructor destroy();override;

    function findAction(name:string):TTemplateAction;
    procedure performAction(action:string);
    procedure performAction(const action:TTemplateAction);

  end;

implementation

{ TTemplateActionLoop }

type

{ THtmlTemplateParserBreaker }

 THtmlTemplateParserBreaker = class(THtmlTemplateParser)
  function getVariable(name: string): TPXPValue;
end;

function THtmlTemplateParserBreaker.getVariable(name: string): TPXPValue;
begin
  result := pxpvalue();
  evaluatePXPVariable(self, name, result);
end;

procedure TTemplateActionLoop.initFromTree(t: TTreeElement);
begin
  varname:=t['var'];
  list:=t['list'];
  test:=t['test'];
  addChildrenFromTree(t);
end;

procedure TTemplateActionLoop.perform(reader: TTemplateReader);
var
  listx: TPXPValue;
  testx: TPseudoXPathParser;
  i: Integer;
  j: Integer;
  pxp: TPseudoXPathParser;
begin
  if list <> '' then begin
    if varname = '' then raise Exception.Create('A list attribute at a loop node requires a var attribute');
    pxp := reader.parser.createPseudoXPathParser(list);
    listx := pxp.evaluate();
    pxp.free;
  end else listx := nil;
  if test <> '' then testx := reader.parser.createPseudoXPathParser(test)
  else testx := nil;

  if listx = nil then begin
    if testx <> nil then
      while testx.evaluateToBoolean do
        performChildren(reader);
  end else if listx.kind <> pvkSequence then begin
    reader.parser.variableChangeLog.addVariable(varname, listx);
    if (testx = nil) or (testx.evaluateToBoolean) then
      performChildren(reader);
  end else begin
    for i := 0 to TPXPValueSequence(listx).seq.Count-1 do begin
      reader.parser.variableChangeLog.addVariable(varname, TPXPValueSequence(listx).seq[i]);
      if (testx <> nil) and (not testx.evaluateToBoolean()) then begin
        for j:=i+1 to TPXPValueSequence(listx).seq.Count-1 do
          TPXPValueSequence(listx).seq[j].free;
        break;
      end;
      performChildren(reader);
    end;
    TPXPValueSequence(listx).freeNonRecursive;
  end;
  testx.free;
end;

{ TTemplateActionCallAction }

procedure TTemplateActionCallAction.initFromTree(t: TTreeElement);
begin
  action := t['action'];
end;

procedure TTemplateActionCallAction.perform(reader: TTemplateReader);
var
  act: TTemplateAction;
begin
  act := reader.findAction(action);
  if act = nil then raise Exception.Create('Could not find action: '+action);
  act.perform(reader);
end;

{ TTemplateActionLoadPage }

procedure TTemplateActionLoadPage.initFromTree(t: TTreeElement);
begin
  SetLength(postparams, 0);
  url := t.getAttribute('url', url);
  templateFile := t.getAttribute('templateFile', templateFile);
  condition := t['test'];

  t := t.getFirstChild();
  while t <> nil do begin
    if t.typ = tetOpen then begin
      if SameText(t.value, 'post') then begin
        setlength(postparams, length(postparams)+1);
        postparams[high(postparams)].name:=t['name'];
        if t.hasAttribute('value') then postparams[high(postparams)].value:=t['value']
        else postparams[high(postparams)].value:=t.deepNodeText(); //support old for a while
      end else if SameText(t.value, 'template') then
        template:=t.innerXML();
    end;
    t := t.getNextSibling();
  end;
end;

procedure TTemplateActionLoadPage.perform(reader: TTemplateReader);
var
  cachedCondition: TPseudoXPathParser;
  cururl: String;
  post: String;
  page: String;
  tempname: String;
  j: Integer;
  tempvalue: TPXPValue;
begin
  if condition <> '' then begin
    cachedCondition := reader.parser.createPseudoXPathParser(condition); //TODO: long term cache
    try
      if not cachedCondition.evaluateToBoolean() then
        exit;
    finally
      cachedCondition.clear;
    end;
  end;

  if template<>'' then begin
    if Assigned(reader.onLog) then reader.onLog(reader, 'Parse Template From File: '+reader.template.path+templateFile, 2);
    reader.parser.parseTemplate(template,templateFile);
  end;

  cururl := url;
  post := '';

  if cururl <> '' then begin
    if (url[1] = '$') and (url[length(url)] = ';') and (pos('$', copy(url, 2, length(url) - 1)) <= 0) and (pos(';', copy(url, 1, length(url) - 1)) <= 0) then begin;
      tempvalue := THtmlTemplateParserBreaker(reader.parser).getVariable(copy(url, 2, length(url)-2));
      if tempvalue is TPXPValueObject then begin
        cururl := TPXPValueObject(tempvalue).getAsString('url');
        post := TPXPValueObject(tempvalue).getAsString('post');
      end else cururl:=tempvalue.asString;
      tempvalue.free;
    end else
      cururl := reader.parser.replaceVars(url);
    if cururl = '' then exit;
    //allow pages without url to set variables.
  end else begin
    reader.parser.parseHTML('<html></html>'); //apply template to empty "page"
    if Assigned(reader.onPageProcessed) then reader.onPageProcessed(reader, reader.parser);
    exit;
  end;

  for j:=0 to high(postparams) do begin
    if post <> '' then post += '&';
    tempname := reader.parser.replaceVars(postparams[j].name);
    if tempname = '' then
      post += reader.parser.replaceVars(postparams[j].value) //no urlencode! parameter passes multiple values
     else
      post += TInternetAccess.urlEncodeData(tempname)+'='+ TInternetAccess.urlEncodeData(reader.parser.replaceVars(postparams[j].value));
  end;

  if Assigned(reader.onLog) then reader.onLog(reader, 'Get/Post internet page '+cururl+#13#10'Post: '+post);

  if guessType(cururl) = rtFile then
    cururl := strResolveURI(cururl, reader.lastURL);


  case guessType(cururl) of
    rtRemoteURL:
      if post='' then page:=reader.internet.get(cururl)
      else page:=reader.internet.post(cururl, post);
    rtFile:
      page := strLoadFromFileUTF8(cururl);
    rtXML: begin
      page := cururl;
      cururl:='';
    end
    else raise ETemplateReader.create('Unknown url type: '+cururl);
  end;
  reader.lastURL:=cururl;

  if Assigned(reader.onLog) then reader.onLog(reader, 'downloaded: '+inttostr(length(page))+' bytes', 1);

  if page='' then raise EInternetException.Create(url +' konnte nicht geladen werden');

  if template<>'' then begin
    if Assigned(reader.onLog) then reader.onLog(reader, 'parse page: '+reader.parser.replaceVars(url), 1);

    reader.processPage(page, cururl, reader.internet.getLastHTTPHeader('Content-Type'));
  end;
  if Assigned(reader.onLog) then reader.onLog(reader, 'page finished', 2);
end;

{ TTemplateActionVariable }

procedure TTemplateActionVariable.initFromTree(t: TTreeElement);
begin
  name := t['name'];
  hasValueStr :=  t.getAttributeTry('value', value);
  valuex := t.deepNodeText();
end;

procedure TTemplateActionVariable.perform(reader: TTemplateReader);
var
  pxp: TPseudoXPathParser;
begin
  if hasValueStr then
    reader.parser.variableChangeLog.ValuesString[name] := reader.parser.replaceVars(value);
  if valuex <> '' then begin
    pxp := reader.parser.createPseudoXPathParser(valuex);
    if name <> '' then reader.parser.variableChangeLog.addVariable(name, pxp.evaluate())
    else pxp.evaluate();
    pxp.free;
  end;

end;

{ TTemplateActionMain }

procedure TTemplateActionMain.initFromTree(t: TTreeElement);
begin
  name := t['id'];
  addChildrenFromTree(t);
end;

procedure TTemplateActionMain.perform(reader: TTemplateReader);
begin
  performChildren(reader);
end;

{ TTemplateAction }


procedure TTemplateAction.initFromTree(t: TTreeElement);
begin

end;

procedure TTemplateAction.addChildFromTree(t: TTreeElement);
  procedure addChild(c: TTemplateActionClass);
  begin
    SetLength(children, length(children)+1);
    children[high(children)] := c.create();
    children[high(children)].initFromTree(t);
  end;

begin
  if t.typ <> tetOpen then exit;
  if SameText(t.value, 'variable') then addChild(TTemplateActionVariable)
  else if SameText(t.value, 'action') then addChild(TTemplateActionMain)
  else if SameText(t.value, 'actions') then addChildrenFromTree(t)
  else if SameText(t.value, 'page') then addChild(TTemplateActionLoadPage)
  else if SameText(t.value, 'call') then addChild(TTemplateActionCallAction)
  else if SameText(t.value, 'loop') then addChild(TTemplateActionLoop)
  else raise Exception.Create('Unknown template node: '+t.outerXML);
end;

procedure TTemplateAction.performChildren(reader: TTemplateReader);
var
  i: Integer;
begin
  for i:=0 to high(children) do children[i].perform(reader);
end;

procedure TTemplateAction.addChildrenFromTree(t: TTreeElement);
begin
  t := t.getFirstChild();
  while t <> nil do begin
    addChildFromTree(t);
    t := t.getNextSibling();
  end;
end;

procedure TTemplateAction.clear;
var
  i: Integer;
begin
  for i:=0 to high(children) do
    children[i].free;
  SetLength(children,0);
end;

destructor TTemplateAction.Destroy;
begin
  clear;
  inherited Destroy;
end;

{ TMultiPageTemplate }


procedure TMultiPageTemplate.readTree(t: TTreeElement);
var tagName:string;
  u: TTreeElement;
begin
  baseActions.clear;

  (t as TTreeDocument).setEncoding(eUTF8,true,true);
  if t.typ <> tetOpen then raise Exception.Create('Empty template');
  u := t.findChild(tetOpen,'action',[tefoIgnoreText]);
  if u = nil then raise Exception.Create('Empty template');
  baseActions.addChildrenFromTree(u.getParent());
end;

constructor TMultiPageTemplate.create();
begin
  baseActions:=TTemplateAction.Create;
end;

procedure TMultiPageTemplate.loadTemplateFromDirectory(_dataPath: string; aname: string);
  procedure loadTemplates(a: TTemplateAction);
  var i:longint;
    b: TTemplateActionLoadPage;
  begin
    for i:=0 to high(a.children) do
      loadTemplates(a.children[i]);
    if a is TTemplateActionLoadPage then begin
      b := TTemplateActionLoadPage(a);
      if b.templateFile = '' then exit;
      b.template:=strLoadFromFile(self.path+b.templateFile);
      if b.template='' then
        raise ETemplateReader.create('Template-Datei "'+self.path+b.templateFile+'" konnte nicht geladen werden');
    end;
  end;
var
  tree: TTreeParser;
begin
  IncludeTrailingPathDelimiter(_dataPath);
  self.path:=_dataPath;
  self.name:=aname;
  if not FileExists(_dataPath+'template') then
    raise Exception.Create('Template '+_dataPath+' nicht gefunden');


  tree := TTreeParser.Create;
  readTree(tree.parseTreeFromFile(_dataPath+'template'));
  loadTemplates(baseActions);
  tree.free;
end;

procedure TMultiPageTemplate.loadTemplateFromString(template: string; aname: string);
var
  tree: TTreeParser;
begin
  self.path:='';
  self.name:=aname;
  tree := TTreeParser.Create;
  readTree(tree.parseTree(template));
  tree.Free;
end;



destructor TMultiPageTemplate.destroy;
begin
  baseActions.Free;
  inherited destroy;
end;

function TMultiPageTemplate.findAction(_name: string): TTemplateAction;
  function find(a: TTemplateAction): TTemplateAction;
  var
    i: Integer;
  begin
    for i:=0 to high(a.children) do begin
      if a.children[i] is TTemplateActionMain then
        if TTemplateActionMain(a.children[i]).name = _name then exit(a.children[i]);
      result := find(a.children[i]);
      if result <> nil then exit;
    end;
    result := nil;
  end;

begin
  result:=find(baseActions);
end;

function TMultiPageTemplate.findVariableValue(aname: string): string;
function find(a: TTemplateAction): string;
var
  i: Integer;
begin
  for i:=0 to high(a.children) do begin
    if a.children[i] is TTemplateActionVariable then
      if TTemplateActionVariable(a.children[i]).name = aname then exit(TTemplateActionVariable(a.children[i]).value);
    result := find(a.children[i]);
    if result <> '' then exit;
  end;
  result := '';
end;

begin
  result:=find(baseActions);
end;

procedure TTemplateReader.setTemplate(atemplate: TMultiPageTemplate);
var
  i: Integer;
begin
  template:=atemplate;
  for i:=0 to high(atemplate.baseActions.children) do
    if atemplate.baseActions.children[i] is TTemplateActionVariable then
      atemplate.baseActions.children[i].perform(self);
end;

procedure TTemplateReader.processPage(page, cururl, contenttype: string);
begin
  parser.parseHTML(page, cururl, contenttype);

  if Assigned(onPageProcessed) then
    onPageProcessed(self, parser);
end;

constructor TTemplateReader.create(atemplate:TMultiPageTemplate; ainternet: TInternetAccess);
var
  i: Integer;
begin
  internet:=ainternet;
  parser:=THtmlTemplateParser.create;
  parser.KeepPreviousVariables:=kpvKeepValues;
  parser.variableChangeLog.caseSensitive:=false;
  setTemplate(atemplate);
end;

destructor TTemplateReader.destroy();
begin
  parser.free;
  inherited destroy();
end;

function TTemplateReader.findAction(name:string): TTemplateAction;
begin
  result:=template.findAction(name);
end;

procedure TTemplateReader.performAction(action: string);
var act: TTemplateAction;
begin
  act:=findAction(action);
  if act=nil then raise ETemplateReader.Create('Aktion '+action+' konnte nicht ausgeführt werden, da sie nicht gefunden wurde.');
  performAction(act);
end;

procedure TTemplateReader.performAction(const action:TTemplateAction);
begin
  if Assigned(onLog) then onLog(self, 'Enter performAction, finternet:', 5); //TODO: parser log

  //OutputDebugString(pchar(lib.defaultVariables.Text));
  Assert(internet<>nil,'Internet nicht initialisiert');

  action.perform(self);

  if Assigned(onLog) then onLog(self, 'Leave performAction', 5);
end;

{ ETemplateReader }

constructor ETemplateReader.create;
begin

end;

constructor ETemplateReader.create(s: string; more_details: string);
begin
  Message:=s;
  details:=more_details;
end;

end.


