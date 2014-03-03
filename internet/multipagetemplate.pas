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

(***
  @abstract(This unit contains classes to handle multi-page templates. A collection of single-page templates that can be applied to different pages.)@br@br

  The class TMultiPageTemplate can be used to load a template (and there is also the documentation of the template syntax/semantic).

  The class TMultipageTemplateReader can be used to run the template.
*)
unit multipagetemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,bbutils,extendedhtmlparser, simplehtmltreeparser,simplexmlparser, xquery,dRegExpr,internetaccess;

type

  { TTemplateAction }

  TMultipageTemplateReader = class;
  //**@abstract(Internal used base class for an action within the multi page template)
  TTemplateAction = class
  protected
    procedure addChildFromTree(t: TTreeNode);
    procedure performChildren(reader: TMultipageTemplateReader);
    function cloneChildren(theResult: TTemplateAction): TTemplateAction;
    function parseQuery(reader: TMultipageTemplateReader; query: string): IXQuery;
  public
    children: array of TTemplateAction;
    procedure initFromTree(t: TTreeNode); virtual;
    procedure addChildrenFromTree(t: TTreeNode);
    procedure perform(reader: TMultipageTemplateReader); virtual; abstract;
    function clone: TTemplateAction; virtual;
    procedure clear;
    destructor Destroy; override;
  end;
  TTemplateActionClass = class of TTemplateAction;

  { TTemplateActionMeta }

  TTemplateActionMeta = class(TTemplateAction)
    description: string;
    variables: array of record
      name: string;
      hasDef: boolean;
      def: string;
      description: string;
    end;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TMultiPageTemplate }

  type TLoadTemplateFile = function(name: string): string;

  (***@abstract(A multi page template, which defines which and how pages are processed. @br )

    A multi page template defines a list of actions, each listing variables to set as well as pages to download and
    process with single-page templates. @br
    You can then call an action, let it process the elements defined in the template and then read the resulting variables.

   (Notice: Although both the multi-page and single-page templates are called templates, they are actually something
    *entirely* different:
       A multi-page template is a list of explicit actions that are performed in order, like an algorithm;
       A single-page template is an implicit pattern that is matched against the page, like a regular expression)

    The xml file of such a multi-page template looks like this:
   @longCode(
   <actions>
   <action id="action-1">
     <variable name="foobar" value="xyz"/>

     <page url="url to send the request to"
           templateFile="File containing a single page template (optional)"  >
       <header name="header name">value (optional) </header>
       <post name="post variable name"> value (optional) </post>
       ...
       <template>A single page template (optional) </template>
     </page>

     ...

   </action>
   <action id="action-2">

     ...
   </action>
    ...
   </actions>)

    <actions> contains a list/map of named actions, each <action> can contain:

    @unorderedList(
      @item(@code(<variable>)  Sets an variable, either to a string value or to an evaluated XPath expression )
      @item(@code(<page>)     Downloads a page and processes it with a single-page template )
      @item(@code(<loop>)      Repeats the children of the loop element )
      @item(@code(<call>)      Calls another action )
    )

    Details for each element:

    @definitionList(

    @itemLabel(@code(<variable name="name" value="str value">xpath expression</variable>))
    @item(

      This sets the value of the variable with name $name.
      If the value attribute is given, it is set to the string value of the attribute, otherwise the xpath expression
      is evaluated its result is used.
      (there is no document loaded for node reading, but the xpath expression is still useful for computations on the other
       variables.)

    )
    @itemLabel(@code(<page url="request url" templateFile="single page template file">))
    @item(

      A page to download and process. @br
      You can use @code(<post name="..name.." value="..value..">..value..</post>) elements in the <page> to add
      variables for a post request to send to the url. @br
      If the name attribute exists, the content is url encoded, otherwise not. @br @code()
      (currently the value attribute and the contained text are treated as string to send.
       In future versions, the contained text will be evaluated as xpath expression.)  @br
      If no <post> children exist, a GET request is send.

      The template that should be applied to the downloaded page, can be given directly in a <template> element, or
      in a separate file linked by the templateFile attribute.
      (see THtmlTemplateParser for a description of the pattern-matching single-page template.)

      There is also a @code(test="xpath") attribute that can define a condition, which will skip a page,
      if the condition evaluates to false().

    )
    @itemLabel(@code(<loop var="variable name" list="list (xpath)" test="condition (xpath)">))
    @item(

      Repeats the children of this element.@br
      It can be used like a foreach loop by giving the var/list attributes, like a while loop by using test,
      or like a combination of both.@br
      In the first case the expression in list is evaluated, each element of the resulting sequence is assigned
      once to the variable with the name var, and the loop body is evaluated each time.@br
      In the second case, the loop is simply repeated forever, until the expression in the test attributes evaluates to false.

    )
    @itemLabel(@code(<call action="name">))
    @item(

      Calls the action of the given name.

    )
   )

   Within all string attributes you can access the previously defined variables by writing @code($variable;)  (with colon).@br
   Within a xpath expression you can access the variables as usually with @code($variable)
  *)
  TMultiPageTemplate=class
  protected
    procedure readTree(t: TTreeNode);
  public
    //**The primary <actions> element (or the first <action> element, if only one exists)
    baseActions: TTemplateAction;
    //**The path of the xml file containing this template
    path,name:string;

    //variables: TStringList;

    constructor create();
    //**Loads this template from a directory. @br The multipage template is read from the file template, and
    //**additional single page, pattern-matching templates given by templateFile attributes are read from their relative file
    procedure loadTemplateFromDirectory(_dataPath: string; aname: string = 'unknown');
    //**Loads the template directly from a string. @br Loading pattern-matching templates with the templateFile attribute is not supported
    procedure loadTemplateFromString(template: string; aname: string = 'unknown');
    //**Loads this template from a directory. @br The multipage template is read from the file template, and
    //**additional single page, pattern-matching templates given by templateFile attributes are read from their relative file
    procedure loadTemplateWithCallback(loadSomething: TLoadTemplateFile; _dataPath: string; aname: string = 'unknown');
    destructor destroy;override;

    //**Returns a <action> element with the given id
    function findAction(_name:string): TTemplateAction;
    //**Find the first <variable> element definining a variable with the given name. @br
    //**Only returns the value of the value attribute, ignoring any contained xpath expression
    function findVariableValue(aname: string): string;

    function clone: TMultiPageTemplate;
    //function getAccountObject():TCustomAccountAccess;override;
  end;

  { TMultipageTemplateReader }

  { ETemplateReader }

  ETemplateReader=class(Exception)
    details:string;
    constructor create;
    constructor create(s:string;more_details:string='');
  end;
  //**Event you can use to log, what the template is doing .@br
  //**Arguments: logged contains the message, debugLevel the importance of this event
  TLogEvent = procedure (sender: TMultipageTemplateReader; logged: string; debugLevel: integer = 0) of object;
  //**Event that is called after every <page> element is processed. @br
  //**You can use parser to read the variables changed by the template applied to the page
  TPageProcessed = procedure (sender: TMultipageTemplateReader; parser: THtmlTemplateParser) of object;
  //**@abstract(Class to process a multi page template)
  //**see TMultiPageTemplate for a documentation of the allowed xml elements
  TMultipageTemplateReader = class
  protected
    template:TMultiPageTemplate;
    lastURL: string;
    procedure setTemplate(atemplate: TMultiPageTemplate);
    procedure processPage(page, cururl, contenttype: string); virtual;
  public
    //** Object used to send requests and download pages
    internet:TInternetAccess;
    //** Parser used to apply the given single page templates to a downloaded page
    parser: THtmlTemplateParser;
    //** Log event
    onLog: TLogEvent;
    //** Event to access the changed variable state after each processed <page> element
    onPageProcessed: TPageProcessed;

    //** Creates a reader using a certain template (atemplate is mandatory, ainternet optional)
    constructor create(atemplate:TMultiPageTemplate; ainternet: TInternetAccess);
    destructor destroy();override;

    //** Searches the action element with the given id (equivalent to TMultiPageTemplate.findAction)
    function findAction(name:string):TTemplateAction;
    //** Executes the action with the given id @br(e.g. setting all variables, downloading all pages defined there) @br
    //** This does not modify the action, so you can use the same template with multiple readers (even in other threads)
    procedure callAction(action:string);
    //** Executes the given action @br(e.g. setting all variables, downloading all pages defined there)
    //** This does not modify the action, so you can use the same template with multiple readers (even in other threads)
    procedure callAction(const action:TTemplateAction);

  end;

implementation


{ TTemplateActionLoop }

type

  { TTemplateActionMain }

  TTemplateActionMain = class(TTemplateAction)
    name: string;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionVariable }

  TTemplateActionVariable = class(TTemplateAction)
    name, value, valuex: string;
    hasValueStr: boolean;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionLoadPage }

  TTemplateActionLoadPage = class(TTemplateAction)
    url:string;
    templateFile:string;
    template:string;
    headers, postparams:array of TProperty;
    condition, method: string;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  private
    templateName: string;
  end;

  { TTemplateActionCallAction }

  TTemplateActionCallAction = class(TTemplateAction)
    action, test: string;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionIf }

  TTemplateActionIf = class(TTemplateAction)
    test: string;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionChoose }

  TTemplateActionChoose = class(TTemplateAction)
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionChooseWhen }

  TTemplateActionChooseWhen = class(TTemplateAction)
    test: string;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionChooseOtherwise }

  TTemplateActionChooseOtherwise = class(TTemplateAction)
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;
  { TTemplateActionLoop }

  TTemplateActionLoop = class(TTemplateAction)
    varname, list, test: string;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionShort }

  TTemplateActionShort = class(TTemplateAction)
    test, query: string;
    procedure initFromTree(t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

{ THtmlTemplateParserBreaker }

 THtmlTemplateParserBreaker = class(THtmlTemplateParser)
  function getVariable(name: string): IXQValue;
end;

{ TTemplateActionShort }

procedure TTemplateActionShort.initFromTree(t: TTreeNode);
begin
  query := t.deepNodeText();
  test := t['test'];
end;

procedure TTemplateActionShort.perform(reader: TMultipageTemplateReader);
begin
  if test <> '' then
    if not parseQuery(reader, test).evaluate().toBooleanEffective then
      exit;
  parseQuery(reader, query).evaluate();
end;

function TTemplateActionShort.clone: TTemplateAction;
begin
  Result:=TTemplateActionShort.Create;
  TTemplateActionShort(result).query:=query;
end;

procedure TTemplateActionIf.initFromTree(t: TTreeNode);
begin
  inherited initFromTree(t);
  test := t['test'];
  addChildrenFromTree(t);
end;

procedure TTemplateActionIf.perform(reader: TMultipageTemplateReader);
var
  i: Integer;
begin
  if parseQuery(reader, test).evaluate().toBooleanEffective then
    for i := 0 to high(children) do
      children[i].perform(reader);
end;

function TTemplateActionIf.clone: TTemplateAction;
begin
  result := cloneChildren(TTemplateActionIf.Create);
  TTemplateActionIf(result).test := test;
end;

{ TTemplateActionMeta }

procedure TTemplateActionMeta.initFromTree(t: TTreeNode);
var
  e: TTreeNode;
begin
  e := t.next;
  while e <> nil do begin
    if (e.typ = tetOpen) then
      case e.value of
        'description': description:=e.deepNodeText();
        'variable': begin
          SetLength(variables, length(variables)+1);
          with variables[high(variables)] do begin
            name := e['name'];
            hasDef:= e.hasAttribute('default');
            if hasDef then def := e['default'];
            description := e.findChild(tetOpen, 'description',  []).deepNodeText();;
          end;
        end;
      end;
    e := e.getNextSibling();
  end;
end;

procedure TTemplateActionMeta.perform(reader: TMultipageTemplateReader);
begin

end;

function TTemplateActionMeta.clone: TTemplateAction;
begin
  Result:=TTemplateActionMeta.Create;
  TTemplateActionMeta(result).Description := description;
  TTemplateActionMeta(result).variables:=variables;
end;

{ TTemplateActionChooseOtherwise }

procedure TTemplateActionChooseOtherwise.initFromTree(t: TTreeNode);
begin
  inherited initFromTree(t);
  addChildrenFromTree(t);
end;

procedure TTemplateActionChooseOtherwise.perform(reader: TMultipageTemplateReader);
begin
  raise ETemplateReader.create('when is only allowed within a choose element');
end;

function TTemplateActionChooseOtherwise.clone: TTemplateAction;
begin
  result := cloneChildren(TTemplateActionChooseOtherwise.Create);
end;

{ TTemplateActionChooseWhen }

procedure TTemplateActionChooseWhen.initFromTree(t: TTreeNode);
begin
  inherited initFromTree(t);
  addChildrenFromTree(t);
  test := t['test'];
end;

procedure TTemplateActionChooseWhen.perform(reader: TMultipageTemplateReader);
begin
  raise ETemplateReader.create('when is only allowed within a choose element');
end;

function TTemplateActionChooseWhen.clone: TTemplateAction;
begin
  result := cloneChildren(TTemplateActionChooseWhen.Create);
  TTemplateActionChooseWhen(result).test:=test;
end;

{ TTemplateActionChoose }

procedure TTemplateActionChoose.initFromTree(t: TTreeNode);
begin
  inherited initFromTree(t);
  addChildrenFromTree(t);
end;

procedure TTemplateActionChoose.perform(reader: TMultipageTemplateReader);
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to high(children) do
    if (children[i] is TTemplateActionChooseWhen) then begin
       if (TTemplateActionChooseWhen(children[i]).parseQuery(reader, TTemplateActionChooseWhen(children[i]).test).evaluate().toBoolean) then begin
         for j := 0 to high(children[i].children) do
           children[i].children[j].perform(reader);
         exit;
       end;
    end else if children[i] is TTemplateActionChooseOtherwise then begin
      for j := 0 to high(children[i].children) do
        children[i].children[j].perform(reader);
      exit;
    end else raise ETemplateReader.create('Only when and otherwise are allowed in choose. Got: '+children[i].ClassName);

end;

function TTemplateActionChoose.clone: TTemplateAction;
begin
  result := cloneChildren(TTemplateActionChoose.Create);
end;

function THtmlTemplateParserBreaker.getVariable(name: string): IXQValue;
begin
  result := variableChangeLog.get(name);
end;

procedure TTemplateActionLoop.initFromTree(t: TTreeNode);
begin
  varname:=t['var'];
  list:=t['list'];
  test:=t['test'];
  addChildrenFromTree(t);
end;

procedure TTemplateActionLoop.perform(reader: TMultipageTemplateReader);
var
  listx, x: IXQValue;
  testx: IXQuery;
begin
  if list <> '' then begin
    if varname = '' then raise Exception.Create('A list attribute at a loop node requires a var attribute');
    listx := reader.parser.parseQuery(list).evaluate(); //TODO: parse only once
  end else listx := nil;
  if test <> '' then testx := reader.parser.parseQuery(test)
  else testx := nil;

  if listx = nil then begin
    if testx <> nil then
      while testx.evaluate().toBoolean do
        performChildren(reader);
  end else for x in listx do begin
    reader.parser.variableChangeLog.add(varname, x);
    if (testx <> nil) and (not testx.evaluate().toBoolean) then
      break;
    performChildren(reader);
  end;
end;

function TTemplateActionLoop.clone: TTemplateAction;
begin
  Result:=cloneChildren(TTemplateActionLoop.Create);
  TTemplateActionLoop(result).varname:=varname;
  TTemplateActionLoop(result).list:=list;
  TTemplateActionLoop(result).test:=test;
end;

{ TTemplateActionCallAction }

procedure TTemplateActionCallAction.initFromTree(t: TTreeNode);
begin
  action := t['action'];
  test := t['test']
end;

procedure TTemplateActionCallAction.perform(reader: TMultipageTemplateReader);
var
  act: TTemplateAction;
begin
  if test <> '' then
    if not parseQuery(reader, test).evaluate().toBooleanEffective then
      exit;
  act := reader.findAction(action);
  if act = nil then raise Exception.Create('Could not find action: '+action);
  act.perform(reader);
end;

function TTemplateActionCallAction.clone: TTemplateAction;
begin
  Result:=cloneChildren(TTemplateActionCallAction.Create);
  TTemplateActionCallAction(result).action:=action;
end;

{ TTemplateActionLoadPage }

procedure TTemplateActionLoadPage.initFromTree(t: TTreeNode);
begin
  SetLength(postparams, 0);
  url := t.getAttribute('url', url);
  templateFile := t.getAttribute('templateFile', templateFile);
  templateName := templateFile;
  condition := t['test'];
  method:='';

  t := t.getFirstChild();
  while t <> nil do begin
    if t.typ = tetOpen then begin
      if SameText(t.value, 'post') then begin
        setlength(postparams, length(postparams)+1);
        postparams[high(postparams)].name:=t['name'];
        if t.hasAttribute('value') then postparams[high(postparams)].value:=t['value']
        else postparams[high(postparams)].value:=t.deepNodeText(); //support old for a while
      end else if SameText(t.value, 'template') then
        template:=t.innerXML()
      else if SameText(t.value, 'header') then begin
        setlength(headers, length(headers)+1);
        if t.hasAttribute('value') then headers[high(headers)].value:=t['value']
        else headers[high(headers)].value:=t.deepNodeText();
        if t.hasAttribute('name') then headers[high(headers)].name:=t['name']
        else if pos(':', headers[high(headers)].value) > 0 then begin
          headers[high(headers)].name := strSplitGet(':', headers[high(headers)].value);
          headers[high(headers)].name := trim(headers[high(headers)].name);
          headers[high(headers)].value := trim(headers[high(headers)].value);
        end;
      end else if SameText(t.value, 'method') then begin
        if t.hasAttribute('value') then method:=t['value']
        else method:=t.deepNodeText();
      end;
    end;
    t := t.getNextSibling();
  end;
end;

procedure TTemplateActionLoadPage.perform(reader: TMultipageTemplateReader);
var
  cachedCondition: IXQuery;
  cururl: String;
  post: String;
  page: String;
  tempname: String;
  j: Integer;
  tempvalue: TXQValue;
  curmethod: String;
  tempvi: IXQValue;
begin
  if condition <> '' then begin
    cachedCondition := reader.parser.parseQuery(condition); //TODO: long term cache
    if not cachedCondition.evaluate().toBoolean then exit;
  end;

  if template<>'' then begin
    if Assigned(reader.onLog) then reader.onLog(reader, 'Parse Template From File: '+reader.template.path+templateFile, 2);
    reader.parser.parseTemplate(template, templateName);
  end;

  cururl := url;
  curmethod := method;
  post := '';

  if cururl <> '' then begin
    if (pos('"', url) = 0) and (pos('{', url) = 0) and (pos('}', url) = 0) then cururl := url
    else if (url[1] = '{') and (url[length(url)] = '}') and (pos('$', url) > 0) and (trim(copy(url, 2, length(url)-2))[1] = '$') and
      reader.parser.variableChangeLog.hasVariable(trim(copy(url, pos('$', url)+1, length(url) - pos('$', url) - 1)), @tempvalue) then begin
      tempvi := reader.parser.QueryEngine.evaluateXPath3('pxp:resolve-html(., pxp:get("url"))', tempvalue).getChild(1);
      if tempvi.kind = pvkObject then begin
        cururl := tempvi.getProperty('url').toString;
        curmethod := tempvi.getProperty('method').toString;
        post := tempvi.getProperty('post').toString;
      end else cururl := tempvi.toString;
    end else cururl := reader.parser.replaceEnclosedExpressions(url);
    if cururl = '' then exit;
  end else begin
    //allow pages without url to set variables.
    reader.parser.parseHTML('<html></html>'); //apply template to empty "page"
    if Assigned(reader.onPageProcessed) then reader.onPageProcessed(reader, reader.parser);
    exit;
  end;

  for j:=0 to high(postparams) do begin
    if post <> '' then post += '&';
    tempname := reader.parser.replaceEnclosedExpressions(postparams[j].name);
    if tempname = '' then
      post += reader.parser.replaceEnclosedExpressions(postparams[j].value) //no urlencode! parameter passes multiple values
     else
      post += TInternetAccess.urlEncodeData(tempname)+'='+ TInternetAccess.urlEncodeData(reader.parser.replaceEnclosedExpressions(postparams[j].value));
  end;

  if curmethod = '' then begin
    if (Length(postparams) = 0) and (post = '') then curmethod:='GET'
    else curmethod:='POST';
  end;

  if Assigned(reader.onLog) then reader.onLog(reader, 'Get/Post ('+curmethod+') internet page '+cururl+#13#10'Post: '+post);

  if guessType(cururl) = rtFile then
    cururl := strResolveURI(cururl, reader.lastUrl);


  case guessType(cururl) of
    rtRemoteURL: begin
      for j := 0 to high(headers) do
        reader.internet.additionalHeaders.Add(headers[j].name + ': ' + reader.parser.replaceEnclosedExpressions(headers[j].value));

      page:=reader.internet.request(curmethod, cururl, post);
      reader.lastURL:=reader.internet.lastURL;

      if length(headers) > 0 then reader.internet.additionalHeaders.Clear;
    end;
    rtFile: begin
      page := strLoadFromFileUTF8(cururl);
      reader.lastURL:=cururl;
    end;
    rtXML: begin
      page := cururl;
      cururl:='';
      reader.lastURL:=cururl;
    end
    else raise ETemplateReader.create('Unknown url type: '+cururl);
  end;

  if Assigned(reader.onLog) then reader.onLog(reader, 'downloaded: '+inttostr(length(page))+' bytes', 1);

  if page='' then raise EInternetException.Create(url +' konnte nicht geladen werden');

  if template<>'' then begin
    if Assigned(reader.onLog) then reader.onLog(reader, 'parse page: '+reader.lastURL, 1);

    reader.processPage(page, reader.lastURL, reader.internet.getLastHTTPHeader('Content-Type'));
  end;
  if Assigned(reader.onLog) then reader.onLog(reader, 'page finished', 2);
end;

function TTemplateActionLoadPage.clone: TTemplateAction;
begin
  Result:=cloneChildren(TTemplateActionLoadPage.Create);
  TTemplateActionLoadPage(result).url := url;
  TTemplateActionLoadPage(result).templateFile := templateFile;
  TTemplateActionLoadPage(result).template:=template;
  TTemplateActionLoadPage(result).headers := headers;
  SetLength(TTemplateActionLoadPage(result).headers, length(headers));
  TTemplateActionLoadPage(result).postparams := postparams;
  SetLength(TTemplateActionLoadPage(result).postparams, length(postparams));
  TTemplateActionLoadPage(result).condition:=condition;
  TTemplateActionLoadPage(result).method:=method;
end;

{ TTemplateActionVariable }

procedure TTemplateActionVariable.initFromTree(t: TTreeNode);
begin
  name := t['name'];
  hasValueStr :=  t.getAttributeTry('value', value);
  valuex := t.deepNodeText();
end;

procedure TTemplateActionVariable.perform(reader: TMultipageTemplateReader);
var
  pxp: IXQuery;
begin
  if hasValueStr then
    reader.parser. variableChangeLog.ValuesString[name] := reader.parser.replaceEnclosedExpressions(value);
  if valuex <> '' then begin
    pxp := reader.parser.parseQuery(valuex);
    if name <> '' then reader.parser.variableChangeLog.add(name, pxp.evaluate())
    else pxp.evaluate();
  end;

end;

function TTemplateActionVariable.clone: TTemplateAction;
begin
  Result:=cloneChildren(TTemplateActionVariable.Create);
  TTemplateActionVariable(result).name:=name;
  TTemplateActionVariable(result).value:=value;
  TTemplateActionVariable(result).valuex:=valuex;
  TTemplateActionVariable(result).hasValueStr:=hasValueStr;
end;

{ TTemplateActionMain }

procedure TTemplateActionMain.initFromTree(t: TTreeNode);
begin
  name := t['id'];
  addChildrenFromTree(t);
end;

procedure TTemplateActionMain.perform(reader: TMultipageTemplateReader);
begin
  performChildren(reader);
end;

function TTemplateActionMain.clone: TTemplateAction;
begin
  Result:=cloneChildren(TTemplateActionMain.Create);
  TTemplateActionMain(result).name:=name;
end;

{ TTemplateAction }


procedure TTemplateAction.initFromTree(t: TTreeNode);
begin

end;

procedure TTemplateAction.addChildFromTree(t: TTreeNode);
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
  else if SameText(t.value, 'choose') then addChild(TTemplateActionChoose)
  else if SameText(t.value, 'when') then addChild(TTemplateActionChooseWhen)
  else if SameText(t.value, 'otherwise') then addChild(TTemplateActionChooseOtherwise)
  else if SameText(t.value, 'loop') then addChild(TTemplateActionLoop)
  else if SameText(t.value, 'meta') then addChild(TTemplateActionMeta)
  else if SameText(t.value, 'if') then addChild(TTemplateActionIf)
  else if SameText(t.value, 's') then addChild(TTemplateActionShort)
  else raise Exception.Create('Unknown template node: '+t.outerXML);
end;

procedure TTemplateAction.performChildren(reader: TMultipageTemplateReader);
var
  i: Integer;
begin
  for i:=0 to high(children) do children[i].perform(reader);
end;

function TTemplateAction.cloneChildren(theResult: TTemplateAction): TTemplateAction;
var
  i: Integer;
begin
  result := theResult;
  setlength(result.children, length(children));
  for i := 0 to high(children) do
    result.children[i] := children[i].clone;
end;

function TTemplateAction.parseQuery(reader: TMultipageTemplateReader; query: string): IXQuery;
begin
  result := reader.parser.parseQuery(query);
end;

procedure TTemplateAction.addChildrenFromTree(t: TTreeNode);
begin
  t := t.getFirstChild();
  while t <> nil do begin
    addChildFromTree(t);
    t := t.getNextSibling();
  end;
end;

function TTemplateAction.clone: TTemplateAction;
begin
  result := TTemplateAction.Create;
  cloneChildren(result);
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


procedure TMultiPageTemplate.readTree(t: TTreeNode);
var  u: TTreeNode;
begin
  baseActions.clear;

  if not (t.typ in [tetOpen, tetDocument]) then raise Exception.Create('Empty template');
  u := t.findChild(tetOpen,'action',[tefoIgnoreText]);
  if u = nil then raise Exception.Create('Empty template');
  baseActions.addChildrenFromTree(u.getParent());
end;


constructor TMultiPageTemplate.create();
begin
  baseActions:=TTemplateAction.Create;
end;

procedure setTemplateNames(a: TTemplateAction; baseName: string='');
var
  i: Integer;
begin
  if a is TTemplateActionLoadPage then begin
    baseName+=' page:'+TTemplateActionLoadPage(a).url;
    if TTemplateActionLoadPage(a).templateName = '' then TTemplateActionLoadPage(a).templateName:='(template of'+baseName+')';
  end else if a is TTemplateActionMain then
      baseName+=' action:'+TTemplateActionMain(a).name;
  for i := 0 to high(a.children) do
    setTemplateNames(a.children[i], baseName);
end;

procedure TMultiPageTemplate.loadTemplateFromDirectory(_dataPath: string; aname: string);
begin
  if not FileExists(_dataPath+'template') then
    raise Exception.Create('Template '+_dataPath+' nicht gefunden');
  IncludeTrailingPathDelimiter(_dataPath);
  self.path:=_dataPath;
  loadTemplateWithCallback(@strLoadFromFileUTF8, aname);
end;

procedure TMultiPageTemplate.loadTemplateFromString(template: string; aname: string);
var
  tree: TTreeParser;
begin
  self.path:='';
  self.name:=aname;
  tree := TTreeParser.Create;
  tree.globalNamespaces.add(TNamespace.create(HTMLPARSER_NAMESPACE_URL, 't'));
  tree.globalNamespaces.add(TNamespace.create(HTMLPARSER_NAMESPACE_URL, 'template'));
  tree.TargetEncoding:=eUTF8;
  readTree(tree.parseTree(template));
  setTemplateNames(baseActions);
  tree.Free;
end;

procedure TMultiPageTemplate.loadTemplateWithCallback(loadSomething: TLoadTemplateFile; _dataPath: string; aname: string);
  procedure loadTemplates(a: TTemplateAction);
  var i:longint;
    b: TTemplateActionLoadPage;
  begin
    for i:=0 to high(a.children) do
      loadTemplates(a.children[i]);
    if a is TTemplateActionLoadPage then begin
      b := TTemplateActionLoadPage(a);
      if b.templateFile = '' then exit;
      b.template:=loadSomething(_dataPath+b.templateFile);
      if b.template='' then
        raise ETemplateReader.create('Template-Datei "'+self.path+b.templateFile+'" konnte nicht geladen werden');
    end
  end;
var
  tree: TTreeParser;
begin
  self.path:=_dataPath;
  self.name:=aname;


  tree := TTreeParser.Create;
  tree.globalNamespaces.add(TNamespace.create(HTMLPARSER_NAMESPACE_URL, 't'));
  tree.globalNamespaces.add(TNamespace.create(HTMLPARSER_NAMESPACE_URL, 'template'));
  tree.TargetEncoding:=eUTF8;
  readTree(tree.parseTree(loadSomething(_dataPath+'template'), 'template'));
  loadTemplates(baseActions);
  setTemplateNames(baseActions);
  tree.free;
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

function TMultiPageTemplate.clone: TMultiPageTemplate;
begin
  result := TMultiPageTemplate.create();
  result.path:=path;
  result.name:=name;
  result.baseActions:=baseActions.clone;
end;

procedure TMultipageTemplateReader.setTemplate(atemplate: TMultiPageTemplate);
var
  i: Integer;
begin
  template:=atemplate;
  for i:=0 to high(atemplate.baseActions.children) do
    if atemplate.baseActions.children[i] is TTemplateActionVariable then
      atemplate.baseActions.children[i].perform(self);
end;

procedure TMultipageTemplateReader.processPage(page, cururl, contenttype: string);
begin
  parser.variableChangeLog.add('url', cururl);
  parser.variableChangeLog.add('raw', page);

  if not strContains(cururl, '://') then
    cururl := strResolveURI(cururl, 'file://' + strPrependIfMissing(GetCurrentDir, '/'));

  parser.parseHTML(page, cururl, contenttype);

  if Assigned(onPageProcessed) then
    onPageProcessed(self, parser);
end;

constructor TMultipageTemplateReader.create(atemplate:TMultiPageTemplate; ainternet: TInternetAccess);
begin
  internet:=ainternet;
  parser:=THtmlTemplateParser.create;
  parser.KeepPreviousVariables:=kpvKeepValues;
  parser.variableChangeLog.caseSensitive:=false;
  setTemplate(atemplate);
end;

destructor TMultipageTemplateReader.destroy();
begin
  parser.free;
  inherited destroy();
end;

function TMultipageTemplateReader.findAction(name:string): TTemplateAction;
begin
  result:=template.findAction(name);
end;

procedure TMultipageTemplateReader.callAction(action: string);
var act: TTemplateAction;
begin
  act:=findAction(action);
  if act=nil then raise ETemplateReader.Create('Aktion '+action+' konnte nicht ausgeführt werden, da sie nicht gefunden wurde.');
  callAction(act);
end;

procedure TMultipageTemplateReader.callAction(const action:TTemplateAction);
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


