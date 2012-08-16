unit multipagetemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,bbutils,extendedhtmlparser,simplehtmlparser,simplehtmltreeparser,simplexmlparser, pseudoxpath,dRegExpr,internetaccess;

type
  TTemplateRealActionType = (tratLoadPage,tratCallAction);
  TTemplateRealAction=record
    typ: TTemplateRealActionType;
    //load page
    url:string;
    templateFile:string;
    template:string;
    postparams:array of TProperty;
    //call action
    action: string;
      //callOnce: boolean;
  end;

  TTemplateAction=record
    name: string;
    actions:array of TTemplateRealAction;
  end;
  PTemplateAction=^TTemplateAction;

  { TMultiPageTemplate }

  TMultiPageTemplate=class
  protected
    currentAction:PTemplateAction;
    currentTag: string;
    procedure readTree(t: TTreeElement);
    function textRead(text: string):TParsingResult;
    function leaveTag(tagName: string):TParsingResult;
  public
    earMarkedRegEx,maxLimitRegEx,accountExpiredRegEx:TRegExpr;
    propertyAuthorRegEx, propertyTitleRegEx, propertyISBNRegEx, propertyYearRegEx:TRegExpr;

    path,name:string;
    actions:array of TTemplateAction;

    variables: TStringList;

    constructor create();
    procedure loadTemplateFromDirectory(_dataPath: string; aname: string = 'unknown');
    procedure loadTemplateFromString(template: string; aname: string = 'unknown');
    destructor destroy;override;

    function findAction(_name:string):PTemplateAction;
    //function getAccountObject():TCustomAccountAccess;override;
  end;

  { TTemplateReader }

  { ETemplateReader }

  ETemplateReader=class(Exception)
    details:string;
    constructor create;
    constructor create(s:string;more_details:string='');
  end;
  TTemplateReader = class;
  TLogEvent = procedure (sender: TTemplateReader; logged: string; debugLevel: integer = 0) of object;
  TPageProcessed = procedure (sender: TTemplateReader; parser: THtmlTemplateParser) of object;

  TTemplateReader = class
  protected
    template:TMultiPageTemplate;
    procedure setTemplate(atemplate: TMultiPageTemplate);
  public
    internet:TInternetAccess;
    parser: THtmlTemplateParser;
    onLog: TLogEvent;
    onPageProcessed: TPageProcessed;

    constructor create(atemplate:TMultiPageTemplate; ainternet: TInternetAccess);
    destructor destroy();override;

    function findAction(name:string):PTemplateAction;
    procedure performAction(action:string);
    procedure performAction(const action:TTemplateAction);

  end;

implementation

uses simpleinternet;
{ TMultiPageTemplate }


procedure TMultiPageTemplate.readTree(t: TTreeElement);
var tagName:string;
begin
  while t <> nil do begin
    case t.typ of
      tetOpen: begin
        tagName:=t.value;
        if SameText(tagName,'variable') then begin
          variables.Values[t['name']]:=t['value']
        end else if SameText(tagName,'page') then begin
          SetLength(currentAction^.actions,length(currentAction^.actions)+1);
          with currentAction^.actions[high(currentAction^.actions)] do begin
            typ:=tratLoadPage;
            setlength(postparams,0);
            url := t.getAttribute('url', url);
            templateFile := t.getAttribute('templateFile', templateFile);
          end;
        end else if SameText(tagName,'post') then begin
          if (currentAction=nil) then raise ETemplateReader.create('Template ungültig: post-Tag gefunden, ohne dass eine Aktion definiert wurde');
          if (length(currentAction^.actions)=0)or(currentAction^.actions[high(currentAction^.actions)].typ<>tratLoadPage) then raise ETemplateReader.create('Template ungültig: post-Tag nicht innerhalb eines page-Tags');
          setlength(currentAction^.actions[high(currentAction^.actions)].postparams, length(currentAction^.actions[high(currentAction^.actions)].postparams)+1);
          currentAction^.actions[high(currentAction^.actions)].postparams[high(currentAction^.actions[high(currentAction^.actions)].postparams)].name:=t['name']
          //for value see textread
        end else if SameText(tagName,'call') then begin
          SetLength(currentAction^.actions,length(currentAction^.actions)+1);
          with currentAction^.actions[high(currentAction^.actions)] do begin
            typ:=tratCallAction;
            action:=t['action'];
          end;
        end else if SameText(tagName,'action') then begin
          SetLength(actions,length(actions)+1);
          FillChar(actions[high(actions)],sizeof(actions[high(actions)]),0);
          actions[high(actions)].name:=LowerCase(t['id']);
          currentAction:=@actions[high(actions)];
        end else if SameText(tagName, 'template') then begin
          if (currentAction=nil) then raise ETemplateReader.create('Template ungültig: html template gefunden, ohne dass eine Aktion definiert wurde');
          currentAction^.actions[high(currentAction^.actions)].template:=t.innerXML();
          if t.typ = tetOpen then t := t.reverse;
        end;
        currentTag:=tagName;
      end;
      tetClose: currentTag := '';
      tetText: textRead(t.value);
    end;
    t := t.next;
  end;
end;

function TMultiPageTemplate.textRead(text: string): TParsingResult;
var page: ^TTemplateRealAction;
begin
  if currentTag = 'post' then begin
    if (currentAction=nil) then raise ETemplateReader.create('Template ungültig: post-Tag gefunden, ohne dass eine Aktion definiert wurde');
    if (length(currentAction^.actions)=0)or(currentAction^.actions[high(currentAction^.actions)].typ<>tratLoadPage) then raise ETemplateReader.create('Template ungültig: post-Tag nicht innerhalb eines page-Tags');
    page:=@currentAction^.actions[high(currentAction^.actions)];
    if length(page^.postparams) = 0 then raise ETemplateReader.create('internal invalid post params');
    page^.postparams[high(page^.postparams)].value:=text
  end;
  Result:=prContinue;
end;

function TMultiPageTemplate.leaveTag(tagName: string): TParsingResult;
begin
  currentTag:='';
  Result:=prContinue;
end;

constructor TMultiPageTemplate.create();
begin
  variables:=TStringList.Create;
end;

procedure TMultiPageTemplate.loadTemplateFromDirectory(_dataPath: string; aname: string);
  procedure loadTemplates;
    procedure load(var action:TTemplateAction);
    var i:longint;
    begin
      for i:=0 to high(action.actions) do begin
        if action.actions[i].templateFile='' then continue;
        action.actions[i].template:=strLoadFromFile(self.path+action.actions[i].templateFile);
        if action.actions[i].template='' then
          raise ETemplateReader.create('Template-Datei "'+self.path+action.actions[i].templateFile+'" konnte nicht geladen werden');
      end;
    end;
  var i:longint;
  begin
    for i:=0 to high(actions) do
      load(actions[i]);
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
  earMarkedRegEx.free;
  maxLimitRegEx.free;
  accountExpiredRegEx.free;
  variables.Free;

  propertyAuthorRegEx.Free; //nil.free is okay
  propertyTitleRegEx.Free;
  propertyYearRegEx.Free;
  propertyISBNRegEx.Free;
  inherited destroy;
end;

function TMultiPageTemplate.findAction(_name: string): PTemplateAction;
var i:longint;
begin
  result:=nil;
  for i:=0 to high(actions) do
    if actions[i].name=_name then exit(@actions[i]);;
end;

procedure TTemplateReader.setTemplate(atemplate: TMultiPageTemplate);
var
  i: Integer;
begin
  template:=atemplate;
  for i:=0 to atemplate.variables.count-1 do
    parser.variableChangeLog.ValuesString[atemplate.variables.Names[i]]:=atemplate.variables.ValueFromIndex[i];
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

function TTemplateReader.findAction(name:string): PTemplateAction;
begin
  result:=template.findAction(name);
end;

procedure TTemplateReader.performAction(action: string);
var act: PTemplateAction;
begin
  act:=findAction(action);
  if act=nil then raise ETemplateReader.Create('Aktion '+action+' konnte nicht ausgeführt werden, da sie nicht gefunden wurde.');
  performAction(act^);
end;

procedure TTemplateReader.performAction(const action:TTemplateAction);
var i:longint;
    page:string;
    j: Integer;
    postparams: String;
    tempname: String;
    cururl: String;
    a: ^TTemplateRealAction;
begin
  if Assigned(onLog) then onLog(self, 'Enter performAction, finternet:', 5); //TODO: parser log

  //OutputDebugString(pchar(lib.defaultVariables.Text));
  Assert(internet<>nil,'Internet nicht initialisiert');

  for i:=0 to high(action.actions) do begin
    a := @action.actions[i];
    case a^.typ of
      tratCallAction: begin
        if Assigned(onLog) then onLog(self, 'Action call: '+a^.action, 2);
        performAction(a^.action);
      end;
      tratLoadPage: begin
        if a^.template<>'' then begin
          if Assigned(onLog) then onLog(self, 'Parse Template From File: '+template.path+a^.templateFile, 2);
          parser.parseTemplate(a^.template,a^.templateFile);
        end;

        cururl := a^.url;
        if cururl <> '' then begin
          cururl := parser.replaceVars(a^.url);
          if cururl = '' then continue;
          //allow pages without url to set variables.
        end else begin
          parser.parseHTML('<html></html>'); //apply template to empty "page"
          if Assigned(onPageProcessed) then onPageProcessed(self, parser);
          continue;
        end;

        postparams := '';
        for j:=0 to high(a^.postparams) do begin
          if j <> 0 then postparams += '&';
          tempname := parser.replaceVars(a^.postparams[j].name);
          if tempname = '' then
            postparams += parser.replaceVars(a^.postparams[j].value) //no urlencode! parameter passes multiple values
           else
            postparams += TInternetAccess.urlEncodeData(tempname)+'='+
                          TInternetAccess.urlEncodeData(parser.replaceVars(a^.postparams[j].value));
        end;

        if Assigned(onLog) then onLog(self, 'Get/Post internet page '+cururl+#13#10'Post: '+postparams);

        case guessType(cururl) of
          rtRemoteURL:
            if postparams='' then page:=internet.get(cururl)
            else page:=internet.post(cururl, postparams);
          rtFile:
            page := strLoadFromFileUTF8(cururl);
          rtXML:
            page := cururl;
          else raise ETemplateReader.create('Unknown url type: '+cururl);
        end;

        if Assigned(onLog) then onLog(self, 'downloaded: '+inttostr(length(page))+' bytes', 1);

        if page='' then raise EInternetException.Create(a^.url +' konnte nicht geladen werden');

        if a^.template<>'' then begin
          if Assigned(onLog) then onLog(self, 'parse page: '+parser.replaceVars(a^.url), 1);

          parser.parseHTML(page);

          if Assigned(onPageProcessed) then onPageProcessed(self, parser);
        end;
        if Assigned(onLog) then onLog(self, 'page finished', 2);
      end;
    end;
    if Assigned(onLog) then onLog(self, 'pages finished', 3);
  end;

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


