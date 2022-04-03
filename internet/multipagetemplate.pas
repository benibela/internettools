{Copyright (C) 2008-2021  Benito van der Zander

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
  @abstract(This unit contains classes to handle multi-page template scripts. A collection of single-page patterns that are applied to multiple webpages.)@br@br

  The class TMultiPageTemplate can be used to load a multi-page template script (and there is also the documentation of the template syntax/semantic).

  The class TMultipageTemplateReader can be used to run the template.

  TMultipageTemplateReader does not modify the TMultipageTemplate, so a single TMultipageTemplate can be used by arbitrary many TMultipageTemplateReader, even if the readers are in different threads.

*)
unit multipagetemplate;

{$mode objfpc}{$H+}
{$COperators on}{$goto on}{$inline on}

interface

uses
  Classes, SysUtils,bbutils,extendedhtmlparser, simplehtmltreeparser,simplexmlparser, xquery,internetaccess,bbutilsbeta;

type

  { TTemplateAction }

  TMultipageTemplateReader = class;
  TLoadTemplateFile = function(name: RawByteString): string;
  TTemplateLoadingContext = class
    path: string;
    loadFileCallback: TLoadTemplateFile;
    function createParser: TTreeParser;
  end;

  //**@abstract(Internal used base class for an action within the multi-page template)
  TTemplateAction = class
  protected
    procedure addChildFromTree(context: TTemplateLoadingContext; t: TTreeNode);
    procedure performChildren(reader: TMultipageTemplateReader);
    function cloneChildren(theResult: TTemplateAction): TTemplateAction;
    function parseQuery(reader: TMultipageTemplateReader; const query: string): IXQuery;
    function evaluateQuery(reader: TMultipageTemplateReader; const query: string): IXQValue;
  public
    children: array of TTemplateAction;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); virtual;
    procedure addChildrenFromTree(context: TTemplateLoadingContext; t: TTreeNode);
    procedure perform(reader: TMultipageTemplateReader); virtual;
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
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform({%H-}reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TMultiPageTemplate }


  (***@abstract(A multi-page template, which defines which and how web pages are processed. @br )

    A multi-page template defines a list of actions, each action listing webpages to download and queries to run on those webpages. @br
    You can then call an action, let it run its queries, and read the result as variables.

   (In the past patterns, were called templates, too, but they are very different from the multi-page template of this unit. @br
       A multi-page template is a list of explicit actions that are performed in order, like an algorithm or script; @br
       A pattern (single-page template) is an implicit pattern that is matched against the page, like a regular expression)

    The syntax of a multi-page template is inspired by the XSLT/XProc syntax and looks like this:
   @preformatted(
   <actions>
   <action id="action-1">
     <variable name="foobar" value="xyz"/>

     <page url="url to send the request to">
       <header name="header name">value...</header>
       <post name="post variable name"> value... </post>
     </page>
     <pattern> ...to apply to the previous page (inline)... </pattern>
     <pattern href="to apply to the previous page (from a file)"/>

     ...

   </action>
   <action id="action-2">

     ...
   </action>
    ...
   </actions>)

    <actions> contains a list/map of named actions, each <action> can contain:

    @unorderedList(
      @item(@code(<page>)        Downloads a webpage. )
      @item(@code(<json>)        Same as <page> but to download JSON data. )
      @item(@code(<pattern>)     Processes the last page with pattern matching. )
      @item(@code(<variable>)    Sets an variable, either to a string value or to an evaluated XPath expression. )
      @item(@code(<loop>)        Repeats the children of the loop element. )
      @item(@code(<call>)        Calls another action. )
      @item(@code(<if>)          Tests, if a condition is satisfied. )
      @item(@code(<choose><when><otherwise>)      Switches depending on a value.  )
      @item(@code(<s>)           Evaluates an XPath/XQuery expression. )
      @item(@code(<try><catch>)  Catch errors. )
      @item(@code(<include>)     Includes template elements from another file. )
    )

    Details for each element:

    @definitionList(

    @itemLabel(@code(<page url="request url">))
    @item(
      Specifies a page to download and process. @br
      You can use @code(<post name="..name.." value="..value..">..value..</post>) child elements under <page> to add
      variables for a post request to send to the url. @br
      If the name attribute exists, the content is url-encoded, otherwise not. @br
      (currently, the value attribute and the contained text are treated as a string to send.
       In future versions, the contained text will be evaluated as XPath expression.)  @br
      If no <post> children exist, a GET request is sent.

      The patterns that should be applied to the downloaded page, can be given directly in a <pattern> element, or
      in a separate file linked by the pattern-href attribute.
      (see THtmlTemplateParser for a description of the pattern-matching single-page template.)

      The attribute @code(test="xpath") can be used to skip a page if the condition in the attribute evaluates to false().

    )
    @itemLabel(@code(<pattern href="file" name="..">  inline pattern  </variable>))
    @item(
      This applies a pattern to the last page.

      The pattern can be given inline or loaded from a file in the href attribute.

      The name attribute is only used for debugging.

    )
    @itemLabel(@code(<variable name="name" value="str value">xpath expression</variable>))
    @item(
      This sets the value of the variable with name $name.

      If the value attribute is given, it is set to the string value of the attribute, otherwise, the xpath expression
      is evaluated and its result is used.

      The last downloaded webpage is available as the root element in the XPath expression.
    )
    @itemLabel(@code(<loop var="variable name" list="list (xpath)" test="condition (xpath)">))
    @item(

      Repeats the children of this element.@br
      It can be used like a foreach loop by giving the var/list attributes, like a while loop by using test,
      or like a combination of both.@br
      In the first case, the expression in list is evaluated, each element of the resulting sequence is assigned
      once to the variable with the name $var, and the loop body is evaluated each time.@br
      In the second case, the loop is simply repeated forever, until the expression in the test attributes evaluates to false.

    )
    @itemLabel(@code(<call action="name">))
    @item(

      Calls the action of the given name.
    )

    @itemLabel(@code(<if test="...">))
    @item(
      Evaluates the children of this element, if the test evaluates to true().
    )

    @itemLabel(@code(<choose> <when test="..."/> <otherwise/> </choose>))
    @item(

      Evaluates the tests of the when-elements and the children of the first <when> that is true. @br
      If no test evaluates to true(), the children of <otherwise> are evaluated.

    )

    @itemLabel(@code(<s>...</s>))
    @item(
      Evaluates an XPath/XQuery expression (which can set global variables with :=).
    )

    @itemLabel(@code(<try> ... <catch errors="...">...</catch> </s>))
    @item(
      Iff an error occurs during the evaluation of the non-<catch> children of the <try>-element, the children of matching <catch>-element are evaluated.
      This behaves similar to the try-except statement in Pascal and <try><catch> in XSLT. @br@br

      The errors attribute is a whitespace-separated list of error codes caught by that <catch> element. XPath/XQuery errors have the form @code( err:* ) with the value of * given in the XQuery standard.@br
      HTTP errors have the internal form @code( pxp:http123 ) where pxp: is the default prefix. Nevertheless, they can be matched using the namespace prefix http as @code(http:123). Partial wildcards are accepted like @code(http:4* ) to match the range 400 to 499. @br
      @code(pxp:pattern) is used for pattern matching failures.
    )
    @itemLabel(@code(<include href="filename">))
    @item(
      Includes another XML file. It behaves as if the elements of the other file were copy-pasted here.
    )
   )

   Within all string attributes, you can access the previously defined variables by writing @code({$variable}) .@br
   Within an XPath expression, you can access the variable with @code($variable).
  *)
  TMultiPageTemplate=class
  protected
    procedure readTemplateFromString(template: string; loadFileCallback: TLoadTemplateFile; path: string);
    procedure readTree(context: TTemplateLoadingContext; t: TTreeNode);
  public
    //**The primary <actions> element (or the first <action> element, if only one exists)
    baseActions: TTemplateAction;
    //**A name for the template, for debugging
    name:string;

    constructor create();
    //**Loads a template from a directory. @br
    //**The multipage template is read from the file @code(template).
    procedure loadTemplateFromDirectory(_dataPath: string; aname: string = 'unknown');
    //**Loads a template directly from a string. @br
    //**If the template loads additional files like include files, you need to give a path.
    procedure loadTemplateFromString(template: string; aname: string = 'unknown'; path: string = '');
    //**Loads a template using a callback function. The callback function is called with different files names to load the corresponding file.
    procedure loadTemplateWithCallback(loadSomething: TLoadTemplateFile; _dataPath: string; aname: string = 'unknown');
    destructor destroy;override;

    //**Returns the <action> element with the given id.
    function findAction(_name:string): TTemplateAction;
    //**Find the first <variable> element definining a variable with the given name. @br
    //**Only returns the value of the value attribute, ignoring any contained xpath expression
    function findVariableValue(aname: string): string;

    function clone: TMultiPageTemplate;
  end;

  { TMultipageTemplateReader }

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
  //**@abstract(Class to process a multi-page template)
  //**see TMultiPageTemplate for a documentation of the allowed XML elements.
  TMultipageTemplateReader = class
  protected
    template:TMultiPageTemplate;
    lastData, lastContentType: string;
    lastDataFormat: TInternetToolsFormat;
    dataLoaded: boolean;
    queryCache: TXQMapStringObject;
    procedure needLoadedData;
    procedure setTemplate(atemplate: TMultiPageTemplate);
    procedure applyPattern(pattern, name: string); virtual;
    procedure setVariable(name: string; value: IXQValue; namespace: string = ''); virtual;
    procedure setVariable(name: string; value: string; namespace: string = '');
    function parseQuery(const query: string): IXQuery;
    function evaluateQuery(const query: IXQuery): IXQValue; virtual;
  public
    //** Object used to send requests and download pages
    internet:TInternetAccess;
    //** Parser used to apply the given single page templates to a downloaded page
    parser: THtmlTemplateParser;
    //** Log event
    onLog: TLogEvent;
    //** Event to access the changed variable state after each processed <page> element
    onPageProcessed: TPageProcessed;


    retryOnConnectionFailures: boolean;
    actionTrace: TStringArrayList;

    //** Creates a reader using a certain template (atemplate is mandatory, ainternet optional)
    constructor create(atemplate:TMultiPageTemplate; ainternet: TInternetAccess; patternMatcher: THtmlTemplateParser = nil);
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

uses xquery_json, xquery.namespaces;

type

  { TTemplateActionMain }

  TTemplateActionMain = class(TTemplateAction)
    name: string;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionVariable }

  TTemplateActionVariable = class(TTemplateAction)
    name, value, valuex: string;
    hasValueStr: boolean;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  TTemplateActionPage = class(TTemplateAction)
    url:string;
    headers, postparams:array of TProperty;
    condition, method: string;
    inputFormat: TInternetToolsFormat;

    errorHandling: string;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  private
    procedure onTransferReact(sender: TInternetAccess; var {%H-}transfer: TTransfer; var reaction: TInternetAccessReaction);
  end;

  TTemplateActionPattern = class(TTemplateAction)
    pattern:string;
    constructor create;
    constructor createFromFile(context: TTemplateLoadingContext; href: string);
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  private
    name: string;
    procedure loadFile(context: TTemplateLoadingContext;href: string);
  end;

  { TTemplateActionCallAction }

  TTemplateActionCallAction = class(TTemplateAction)
    action, test: string;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionIf }

  TTemplateActionIf = class(TTemplateAction)
    test: string;
    &else: TTemplateAction;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
    destructor Destroy; override;
  end;

  { TTemplateActionChoose }

  TTemplateActionChoose = class(TTemplateAction)
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionChooseWhen }

  TTemplateActionChooseWhen = class(TTemplateAction)
    test: string;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform({%H-}reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionChooseOtherwise }

  TTemplateActionChooseOtherwise = class(TTemplateAction)
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform({%H-}reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;
  { TTemplateActionLoop }

  TTemplateActionLoop = class(TTemplateAction)
    varname, list, test: string;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionShort }

  TTemplateActionShort = class(TTemplateAction)
    test, query: string;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  { TTemplateActionShort }

  TTemplateActionTry = class(TTemplateAction)
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform(reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
  end;

  TTemplateActionCatch = class(TTemplateAction)
    errNamespaces, errCodes: TStringArray;
    procedure initFromTree(context: TTemplateLoadingContext; t: TTreeNode); override;
    procedure perform({%H-}reader: TMultipageTemplateReader); override;
    function clone: TTemplateAction; override;
    function checkError(reader: TMultipageTemplateReader; const namespace, prefix, code: string): boolean;
  end;

resourcestring
  rsActionNotFound = 'Action %s not found.';

function TTemplateLoadingContext.createParser: TTreeParser;
begin
  result := TTreeParser.Create;
  result.globalNamespaces.add(TNamespace.create(HTMLPARSER_NAMESPACE_URL, 't'));
  result.globalNamespaces.add(TNamespace.create(HTMLPARSER_NAMESPACE_URL, 'template'));
  result.TargetEncoding:=CP_UTF8;
end;



procedure TTemplateActionTry.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
var
  hadCatch: Boolean;
  i: Integer;
begin
  addChildrenFromTree(context, t);
  hadCatch := false;
  for i := 0 to high(children) do
    if objInheritsFrom(children[i], TTemplateActionCatch) then hadCatch := true
    else if hadCatch then raise ETemplateReader.create('Cannot have non-catch element after catch element');
end;

procedure TTemplateActionTry.perform(reader: TMultipageTemplateReader);
   function checkError(namespace, prefix, errCode: string): boolean;
   var
     i: Integer;
   begin
    for i := 0 to high(children) do
      if objInheritsFrom(children[i], TTemplateActionCatch) then
        if TTemplateActionCatch(children[i]).checkError(reader, namespace, prefix, errCode) then
          exit(true);
    exit(false);
   end;

var
  tempcode: String;
begin
  try
    performChildren(reader);
  except
    on e:EXQException do  begin
      if (e.namespace <> nil) and checkError(e.namespace.getURL, e.namespace.getPrefix, e.errorCode) then exit;
      if (e.namespace = nil) and checkError('', '', e.errorCode) then exit;
      raise;
    end;
    on e:EInternetException do begin
      if e.errorCode >= 0 then tempcode := IntToStr(e.errorCode)
      else tempcode := '000';
      while length(tempcode) < 3 do tempcode := '0' + tempcode;
      if checkError(XMLNamespaceURL_MyExtensionsNew, 'x', 'http' + tempcode)  then exit;
      raise;
    end;
    on e: EHTMLParseMatchingException do begin
      if checkError(XMLNamespaceURL_MyExtensionsNew, 'x', 'pattern') then exit;
      raise;
    end;
  end;
end;

function TTemplateActionTry.clone: TTemplateAction;
begin
  Result:=TTemplateActionTry.Create;
  result := cloneChildren(result);
end;

procedure TTemplateActionCatch.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
var
  errors: TStringArray;
  i: Integer;
  temp: TStringArray;
begin
  if not t.hasAttribute('errors') then begin
    SetLength(errNamespaces, 1);
    errNamespaces[0] := '*';
    errCodes := errNamespaces;
  end else begin
    errors := strSplit(strTrimAndNormalize(t.getAttribute('errors')), ' ', false);
    SetLength(errNamespaces, length(errors));
    SetLength(errCodes, length(errors));
    for i := 0 to high(errors) do begin
      if strBeginsWith(errors[i], 'Q{') then begin
        errNamespaces[i] := strDecodeHTMLEntities(strBetween(errors[i], 'Q{','}'),CP_UTF8);
        errCodes[i] := strAfter(errors[i], '{');
      end else if strContains(errors[i], ':') then begin
        temp := strSplit(errors[i], ':');
        errNamespaces[i] := temp[0];
        errCodes[i] := temp[1];
        case errNamespaces[i] of
          'err': errNamespaces[i] := XMLNamespaceURL_XQTErrors;
          'pxp', 'x': errNamespaces[i] := XMLNamespaceURL_MyExtensionsNew;
          'local': errNamespaces[i] := XMLNamespaceURL_XQueryLocalFunctions;
          'http': begin
            errNamespaces[i] := XMLNamespaceURL_MyExtensionsNew;
            errCodes[i] := 'http' + errCodes[i];
            case length(errCodes[i]) of
              length('http12*'): errCodes[i] := StringReplace(errCodes[i], '*', 'x', []);
              length('http1*'): errCodes[i] := StringReplace(errCodes[i], '*', 'xx', []);
              length('http*'): errCodes[i] := StringReplace(errCodes[i], '*', 'xxx', []);
            end;
          end;
          '*': errNamespaces[i] := '*';
          else begin
            raise ETemplateReader.create('Unknown namespace prefix: '+errNamespaces[i] + ' (only err, pxp and local are known)');
          end
        end;
      end else begin
        errNamespaces[i] := XMLNamespaceURL_MyExtensionsNew;
        errCodes[i] := errors[i];
        if errCodes[i] = '*' then errNamespaces[i] := '*';
      end;
      if errCodes[i] <> '*' then
        baseSchema.NCName.createValue(errCodes[i]); //ensure err code is valid
    end;
  end;
  addChildrenFromTree(context, t);
end;

procedure TTemplateActionCatch.perform(reader: TMultipageTemplateReader);
begin
  ;
end;

function TTemplateActionCatch.clone: TTemplateAction;
begin
  Result:=TTemplateActionCatch.Create;
  TTemplateActionCatch(result).errNamespaces := errNamespaces;
  TTemplateActionCatch(result).errCodes := errCodes;
  with TTemplateActionCatch(result) do begin
    SetLength(errNamespaces, length(errNamespaces));
    SetLength(errCodes, length(errCodes));
  end;
  cloneChildren(result);
end;

function TTemplateActionCatch.checkError(reader: TMultipageTemplateReader; const namespace, prefix, code: string): boolean;
  function check: boolean;
  var
    i, j: Integer;
    ok: Boolean;
  begin
    for i := 0 to high(errCodes) do begin
      if (errNamespaces[i] <> namespace) and (errNamespaces[i] <> '*') then continue;
      if (errCodes[i] = code) or (errCodes[i] = '*') then exit(true);
      if (errNamespaces[i] = XMLNamespaceURL_MyExtensionsNew) and (strBeginsWith(errCodes[i], 'http')) and (strBeginsWith(code, 'http')) then begin
        if length(errCodes[i]) = 4 { = 'http' } then exit(true);
        if length(errCodes[i]) <> length(code) then continue;
        ok := true;
        for j := 5 to length(code) do
          if (code[j] <> errCodes[i][j]) and not (errCodes[i][j] in ['X','x']) then begin ok := false; break; end;
        if ok then exit(true);
      end;
    end;
    result := false;
  end;
begin
  result := check;
  if result then begin
    reader.setVariable('code', TXQValueQName.create(namespace,prefix,code), XMLNamespaceURL_XQTErrors);
    //description, value, ...
    performChildren(reader);
  end;
end;

constructor TTemplateActionPattern.create;
begin

end;

constructor TTemplateActionPattern.createFromFile(context: TTemplateLoadingContext;href: string);
begin
  name := href;
  loadFile(context, href);
end;

procedure TTemplateActionPattern.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
var
  href: String;
begin
  ignore(context);
  href := t.getAttribute('href'); //Is loaded later
  name := t.getAttribute('name');
  pattern := t.innerXML();
  if (href <> '') then begin
    if pattern <> '' then raise ETemplateReader.create('Cannot mix href attribute with direct pattern text');
    loadFile(context, href);
  end;
end;

procedure TTemplateActionPattern.perform(reader: TMultipageTemplateReader);
begin
  if Assigned(reader.onLog) then reader.onLog(reader, 'Apply pattern: '+name + ' to '+reader.internet.lastUrl, 2);
  reader.applyPattern(pattern, name);
end;

function TTemplateActionPattern.clone: TTemplateAction;
begin
  Result:=TTemplateActionPattern.Create;
  TTemplateActionPattern(result).pattern := pattern;
  TTemplateActionPattern(result).name := name;
end;

procedure TTemplateActionPattern.loadFile(context: TTemplateLoadingContext; href: string);
begin
  pattern:=context.loadFileCallback(context.path+href);
  if pattern='' then
    raise ETemplateReader.create('Failed to load "'+context.path+href+'".');
end;

{ TTemplateActionShort }

procedure TTemplateActionShort.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  ignore(context);
  query := t.deepNodeText();
  test := t['test'];
end;

procedure TTemplateActionShort.perform(reader: TMultipageTemplateReader);
begin
  if test <> '' then
    if not evaluateQuery(reader, test).toBooleanEffective then
      exit;
  evaluateQuery(reader, query);
end;

function TTemplateActionShort.clone: TTemplateAction;
begin
  Result:=TTemplateActionShort.Create;
  TTemplateActionShort(result).query:=query;
end;

procedure TTemplateActionIf.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  inherited initFromTree(context, t);
  test := t['test'];
  addChildrenFromTree(context, t);
  //else is handled separately
end;

procedure TTemplateActionIf.perform(reader: TMultipageTemplateReader);
begin
  if evaluateQuery(reader, test).toBooleanEffective then performChildren(reader)
  else if &else <> nil then &else.performChildren(reader);
end;

function TTemplateActionIf.clone: TTemplateAction;
begin
  result := cloneChildren(TTemplateActionIf.Create);
  TTemplateActionIf(result).test := test;
  if &else <> nil then TTemplateActionIf(result).&else := &else.clone;

end;

destructor TTemplateActionIf.Destroy;
begin
  &else.free;
  inherited Destroy;
end;

{ TTemplateActionMeta }

procedure TTemplateActionMeta.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
var
  e: TTreeNode;
begin
  ignore(context);
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

procedure TTemplateActionChooseOtherwise.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  inherited initFromTree(context, t);
  addChildrenFromTree(context, t);
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

procedure TTemplateActionChooseWhen.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  inherited initFromTree(context, t);
  addChildrenFromTree(context, t);
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

procedure TTemplateActionChoose.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  inherited initFromTree(context, t);
  addChildrenFromTree(context, t);
end;

procedure TTemplateActionChoose.perform(reader: TMultipageTemplateReader);
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to high(children) do
    if objInheritsFrom(children[i], TTemplateActionChooseWhen) then begin
       if (TTemplateActionChooseWhen(children[i]).evaluateQuery(reader, TTemplateActionChooseWhen(children[i]).test).toBoolean) then begin
         for j := 0 to high(children[i].children) do
           children[i].children[j].perform(reader);
         exit;
       end;
    end else if objInheritsFrom(children[i], TTemplateActionChooseOtherwise) then begin
      for j := 0 to high(children[i].children) do
        children[i].children[j].perform(reader);
      exit;
    end else raise ETemplateReader.create('Only when and otherwise are allowed in choose. Got: '+children[i].ClassName);

end;

function TTemplateActionChoose.clone: TTemplateAction;
begin
  result := cloneChildren(TTemplateActionChoose.Create);
end;

procedure TTemplateActionLoop.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  varname:=t['var'];
  list:=t['list'];
  test:=t['test'];
  addChildrenFromTree(context, t);
end;

procedure TTemplateActionLoop.perform(reader: TMultipageTemplateReader);
var
  listx, x: IXQValue;
  testx: IXQuery;
  context: TXQEvaluationContext;
begin
  if list <> '' then begin
    if varname = '' then raise ETemplateReader.Create('A list attribute at a loop node requires a var attribute');
    listx := evaluateQuery(reader, list);
  end else listx := nil;
  if test <> '' then begin
    reader.needLoadedData;
    testx := parseQuery(reader, test);
    context := reader.parser.QueryContext;
  end else testx := nil;

  if listx = nil then begin
    if testx <> nil then
      while testx.evaluate(context).toBoolean do
        performChildren(reader);
  end else for x in listx do begin
    reader.setVariable(varname, x);
    if (testx <> nil) and (not testx.evaluate(context).toBoolean) then
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

procedure TTemplateActionCallAction.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  ignore(context);
  action := t['action'];
  test := t['test']
end;

procedure TTemplateActionCallAction.perform(reader: TMultipageTemplateReader);
var
  act: TTemplateAction;
  actualaction: String;
  actionTraceCount: SizeInt;
begin
  if test <> '' then
    if not evaluateQuery(reader, test).toBooleanEffective then
      exit;
  actualaction := reader.parser.replaceEnclosedExpressions(action);
  act := reader.findAction(actualaction);
  if act = nil then raise ETemplateReader.Create('Could not find action: '+action + ' ('+actualaction+')');
  actionTraceCount := reader.actionTrace.count;
  reader.actionTrace.add(actualaction + ' <call>');
  act.perform(reader);
  reader.actionTrace.count := actionTraceCount;
end;

function TTemplateActionCallAction.clone: TTemplateAction;
begin
  Result:=cloneChildren(TTemplateActionCallAction.Create);
  TTemplateActionCallAction(result).action:=action;
  TTemplateActionCallAction(result).test:=test;
end;

{ TTemplateActionLoadPage }

procedure TTemplateActionPage.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  SetLength(postparams, 0);
  url := t.getAttribute('url', url);
  condition := t['test'];
  method:='';

  if t.hasAttribute('templateFile') then begin //DEPRECATED pattern import syntax
    SetLength(children, length(children)+1);
    children[high(children)] := TTemplateActionPattern.createFromFile(context, t.getAttribute('templateFile'));
  end;
  if t.hasAttribute('pattern-href') then begin //alternative to templateFile, not recommended to use
    SetLength(children, length(children)+1);
    children[high(children)] := TTemplateActionPattern.createFromFile(context, t.getAttribute('pattern-href'));
  end;
  case LowerCase(t.value) of
    'json': inputFormat := itfJSON;
    else inputFormat := itfHTML;
  end;


  t := t.getFirstChild();
  while t <> nil do begin
    if t.typ = tetOpen then begin
      case LowerCase(t.value) of
        'post': begin
          setlength(postparams, length(postparams)+1);
          postparams[high(postparams)].name:=t['name'];
          if t.hasAttribute('value') then postparams[high(postparams)].value:=t['value']
          else postparams[high(postparams)].value:=t.deepNodeText(); //support old for a while
        end;
        'template': begin //DEPRECATED pattern direct syntax
          SetLength(children, length(children)+1);
          children[high(children)] := TTemplateActionPattern.create();
          TTemplateActionPattern(children[high(children)]).pattern := TrimLeft(t.innerXML());
        end;
        'header': begin
          setlength(headers, length(headers)+1);
          if t.hasAttribute('value') then headers[high(headers)].value:=t['value']
          else headers[high(headers)].value:=t.deepNodeText();
          if t.hasAttribute('name') then headers[high(headers)].name:=t['name']
          else if pos(':', headers[high(headers)].value) > 0 then begin
            headers[high(headers)].name := strSplitGet(':', headers[high(headers)].value);
            headers[high(headers)].name := trim(headers[high(headers)].name);
            headers[high(headers)].value := trim(headers[high(headers)].value);
          end;
        end;
        'method': begin
          if t.hasAttribute('value') then method:=t['value']
          else method:=t.deepNodeText();
        end;
        'error-handling': errorHandling := t['value'];
      end;
    end;
    t := t.getNextSibling();
  end;
end;

procedure TTemplateActionPage.onTransferReact(sender: TInternetAccess; var transfer: TTransfer; var reaction: TInternetAccessReaction);
begin
  TInternetAccess.reactFromCodeString(errorHandling, sender.lastHTTPResultCode, reaction);
end;

procedure TTemplateActionPage.perform(reader: TMultipageTemplateReader);
var
  cururl: String;
  post: String;
  page: String;
  tempname: String;
  j: Integer;
  tempvalue: IXQValue;
  curmethod: String;
  oldHeaders: String;
  oldReact: TTransferReactEvent;
begin
  if (condition <> '') and not evaluateQuery(reader, condition).toBoolean then
    exit;
  reader.dataLoaded := false;
  reader.lastData := '';

  cururl := url;
  curmethod := method;
  post := '';

  oldHeaders := reader.internet.additionalHeaders.Text;

  if cururl <> '' then begin
    if (pos('"', url) = 0) and (pos('{', url) = 0) and (pos('}', url) = 0) then cururl := url
    else if (url[1] = '{') and (url[length(url)] = '}') and (pos('$', url) > 0) and (trim(copy(url, 2, length(url)-2))[1] = '$') and
      reader.parser.variableChangeLog.hasVariable(trim(copy(url, pos('$', url)+1, length(url) - pos('$', url) - 1)), tempvalue) then begin
      with reader.parser.QueryEngine.evaluateXPath3('pxp:resolve-html(., pxp:get("url"))', tempvalue).get(1).tovalue do
        if kind = pvkObject then prepareInternetRequest(curmethod, cururl, post, reader.internet)
        else cururl := toString;
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

  if Assigned(reader.onLog) then reader.onLog(reader, curmethod+' internet page '+cururl+#13#10'Post: '+post);

  if guessType(cururl) = rtFile then  begin
    if (reader.internet.lastUrl = '') then
      reader.internet.lastUrl := IncludeTrailingPathDelimiter( 'file://' + strPrependIfMissing(GetCurrentDir, '/') );
    cururl := strResolveURI(cururl, reader.internet.lastUrl);
  end;


  reader.lastContentType := '';
  case guessType(cururl) of
    rtRemoteURL: begin
      for j := 0 to high(headers) do
        reader.internet.additionalHeaders.Values[trim(headers[j].name)] := trim (reader.parser.replaceEnclosedExpressions(headers[j].value));
      try
        if errorHandling <> '' then begin
          oldReact := reader.internet.OnTransferReact;
          reader.internet.OnTransferReact:=@onTransferReact;
        end;
        page := reader.internet.request(curmethod, cururl, post);
      except
        on e: EInternetException do begin
          reader.actionTrace.add(self.url);
          if reader.retryOnConnectionFailures and (e.errorCode <= 0) then begin
            if Assigned(reader.onLog) then reader.onLog(reader, 'Retry after error: ' + e.Message);
            Sleep(2500);
            page := reader.internet.request(curmethod, cururl, post);
          end else raise;
        end;
      end;
      if errorHandling <> '' then reader.internet.OnTransferReact := oldReact;

      reader.lastContentType := reader.internet.getLastContentType;

      reader.internet.additionalHeaders.Text := oldHeaders;
    end;
    rtFile: begin
      try
        page := strLoadFromFileUTF8(cururl);
      except
        on e: Exception do begin
          reader.actionTrace.add(self.url);
          raise
        end;
      end;
      reader.internet.lastURL:=cururl;
    end;
    rtXML: begin
      page := cururl;
      cururl:='';
      reader.internet.lastURL:=cururl;
    end
    else raise ETemplateReader.create('Unknown url type: '+cururl);
  end;

  if Assigned(reader.onLog) then reader.onLog(reader, 'downloaded: '+inttostr(length(page))+' bytes', 1);

  if page='' then raise EInternetException.Create(url +' konnte nicht geladen werden');

  reader.lastData := page;
  reader.lastDataFormat := inputFormat;

  performChildren(reader);

  if Assigned(reader.onLog) then reader.onLog(reader, 'page finished', 2);
end;

function TTemplateActionPage.clone: TTemplateAction;
begin
  Result:=cloneChildren(TTemplateActionPage.Create);
  TTemplateActionPage(result).url := url;
  TTemplateActionPage(result).headers := headers;
  SetLength(TTemplateActionPage(result).headers, length(headers));
  TTemplateActionPage(result).postparams := postparams;
  SetLength(TTemplateActionPage(result).postparams, length(postparams));
  TTemplateActionPage(result).condition:=condition;
  TTemplateActionPage(result).method:=method;
  TTemplateActionPage(result).inputFormat:=inputFormat;
  result := result;
end;



procedure TTemplateActionVariable.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  ignore(context);
  name := t['name'];
  hasValueStr :=  t.getAttributeTry('value', value);
  valuex := t.deepNodeText();
end;

procedure TTemplateActionVariable.perform(reader: TMultipageTemplateReader);
var
  v: IXQValue;
begin
  if hasValueStr then
    reader.setVariable(name, reader.parser.replaceEnclosedExpressions(value));
  if valuex <> '' then begin
    v := evaluateQuery(reader, valuex);
    if name <> '' then reader.setVariable(name, v);
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

procedure TTemplateActionMain.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  name := t['id'];
  addChildrenFromTree(context, t);
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


procedure TTemplateAction.initFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  ignore(context);
  ignore(t);
end;

procedure TTemplateAction.addChildFromTree(context: TTemplateLoadingContext; t: TTreeNode);
  procedure addChild(c: TTemplateActionClass);
  begin
    SetLength(children, length(children)+1);
    children[high(children)] := c.create();
    children[high(children)].initFromTree(context, t);
  end;
  procedure addChildrenFromInclude();
  var
    href, oldpath, dir: String;
    tree: TTreeParser;
  begin
    href := t['href'];
    oldpath := context.path;
    if href.contains('/') or href.contains('\') then begin
      href := href.Replace('\', '/', [rfReplaceAll]);
      dir := href.pcharViewLeftWith(href.pcharView.findLast('/')).ToString;
      if context.path = '' then context.path := dir
      else context.path := IncludeTrailingPathDelimiter(context.path) + dir;
    end;
    tree := context.createParser;
    try
      addChildrenFromTree(context, tree.parseTree(context.loadFileCallback(oldpath+href)));
    finally
      tree.free;
      context.path := oldpath
    end;
  end;

begin
  if t.typ <> tetOpen then exit;
  case LowerCase(t.value) of
    'variable': addChild(TTemplateActionVariable);
    'action': addChild(TTemplateActionMain);
    'actions': addChildrenFromTree(context, t);
    'page', 'json': addChild(TTemplateActionPage);
    'pattern': addChild(TTemplateActionPattern);
    'call': addChild(TTemplateActionCallAction);
    'choose': addChild(TTemplateActionChoose);
    'when': addChild(TTemplateActionChooseWhen);
    'otherwise': addChild(TTemplateActionChooseOtherwise);
    'loop': addChild(TTemplateActionLoop);
    'meta': addChild(TTemplateActionMeta);
    'if': addChild(TTemplateActionIf);
    'else':
      if (length(children) = 0 ) or not objInheritsFrom(children[high(children)], TTemplateActionIf) then raise ETemplateReader.create('<else> must follow <if>')
      else begin
        TTemplateActionIf(children[high(children)]).&else := TTemplateAction.Create;
        TTemplateActionIf(children[high(children)]).&else.addChildrenFromTree(context, t);
      end;
    's': addChild(TTemplateActionShort);
    'try': addChild(TTemplateActionTry);
    'catch': addChild(TTemplateActionCatch);
    'include': addChildrenFromInclude();
    else raise ETemplateReader.Create('Unknown template node: '+t.outerXML);
  end;
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

function TTemplateAction.parseQuery(reader: TMultipageTemplateReader; const query: string): IXQuery;
begin
  result := reader.parseQuery(query);
end;

function TTemplateAction.evaluateQuery(reader: TMultipageTemplateReader; const query: string): IXQValue;
begin
  reader.needLoadedData;
  //result := parseQuery(reader, query).evaluate(reader.parser.HTMLTree);
  result := reader.evaluateQuery(parseQuery(reader, query));
end;

procedure TTemplateAction.addChildrenFromTree(context: TTemplateLoadingContext; t: TTreeNode);
begin
  t := t.getFirstChild();
  while t <> nil do begin
    addChildFromTree(context, t);
    t := t.getNextSibling();
  end;
end;

procedure TTemplateAction.perform(reader: TMultipageTemplateReader);
begin
  ignore(reader);
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

procedure setPatternNames(a: TTemplateAction; baseName: string='');
var
  i: Integer;
begin
  if objInheritsFrom(a, TTemplateActionPage) then begin
    baseName+=' page:'+TTemplateActionPage(a).url;
  end else if objInheritsFrom(a, TTemplateActionPattern) then begin
    if TTemplateActionPattern(a).name = '' then TTemplateActionPattern(a).name:='(pattern of'+baseName+')';
  end else if objInheritsFrom(a, TTemplateActionMain) then
      baseName+=' action:'+TTemplateActionMain(a).name;
  for i := 0 to high(a.children) do
    setPatternNames(a.children[i], baseName);
end;

procedure TMultiPageTemplate.readTemplateFromString(template: string; loadFileCallback: TLoadTemplateFile; path: string);
var
  context: TTemplateLoadingContext;
  tree: TTreeParser;
begin
  context := TTemplateLoadingContext.Create;
  if path <> '' then path := IncludeTrailingPathDelimiter(path);
  context.path := path;
  context.loadFileCallback := loadFileCallback;
  tree := context.createParser;
  try
    readTree(context, tree.parseTree(template));
    setPatternNames(baseActions);
  finally
    tree.free;
    context.free;
  end;
end;

procedure TMultiPageTemplate.readTree(context: TTemplateLoadingContext; t: TTreeNode);
var  u: TTreeNode;
begin
  baseActions.clear;

  if not (t.typ in [tetOpen, tetDocument]) then raise ETemplateReader.Create('Empty template');
  u := t.findChild(tetOpen,'action',[tefoIgnoreText]);
  if u = nil then raise ETemplateReader.Create('Empty template');
  baseActions.addChildrenFromTree(context, u.getParent());
end;


constructor TMultiPageTemplate.create();
begin
  baseActions:=TTemplateAction.Create;
end;


procedure TMultiPageTemplate.loadTemplateFromDirectory(_dataPath: string; aname: string);
begin
  IncludeTrailingPathDelimiter(_dataPath);
  if not FileExists(_dataPath+'template') then
    raise ETemplateReader.Create('Template '+_dataPath+' nicht gefunden');
  loadTemplateWithCallback(@strLoadFromFileUTF8, _dataPath, aname);
end;


procedure TMultiPageTemplate.loadTemplateFromString(template: string; aname: string; path: string = '');
begin
  self.name:=aname;
  readTemplateFromString(template, @strLoadFromFileUTF8, path );
end;

procedure TMultiPageTemplate.loadTemplateWithCallback(loadSomething: TLoadTemplateFile; _dataPath: string; aname: string);
begin
  self.name:=aname;
  readTemplateFromString(loadSomething(_dataPath+'template'), loadSomething, _dataPath);
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
      if objInheritsFrom(a.children[i], TTemplateActionMain) then
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
    if objInheritsFrom(a.children[i], TTemplateActionVariable) then
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
  result.baseActions.free;
  result.name:=name;
  result.baseActions:=baseActions.clone;
end;

procedure TMultipageTemplateReader.needLoadedData;
var
  curUrl: String;
begin
  if dataLoaded then exit;
  curUrl := internet.lastUrl;
  setVariable('url', cururl);
  setVariable('raw', lastData);

  case lastDataFormat of
    itfJSON: setVariable('json', parser.QueryEngine.DefaultJSONParser.parse(lastData));
    else parser.parseHTMLSimple(lastData, curUrl, lastContentType);
  end;
  dataLoaded := true;
end;

procedure TMultipageTemplateReader.setTemplate(atemplate: TMultiPageTemplate);
var
  i: Integer;
begin
  template:=atemplate;
  for i:=0 to high(atemplate.baseActions.children) do
    if objInheritsFrom(atemplate.baseActions.children[i], TTemplateActionVariable) then
      atemplate.baseActions.children[i].perform(self);
end;

procedure TMultipageTemplateReader.applyPattern(pattern, name: string);
begin
  needLoadedData;
  actionTrace.add(name);
  parser.parseTemplate(pattern, name);
  parser.matchLastTrees;

  if Assigned(onPageProcessed) then
    onPageProcessed(self, parser);
  actionTrace.deleteLast();
end;

procedure TMultipageTemplateReader.setVariable(name: string; value: IXQValue; namespace: string);
begin
  parser.variableChangeLog.setOverride(name, value, namespace);
end;

procedure TMultipageTemplateReader.setVariable(name: string; value: string; namespace: string);
begin
  setVariable(name, xqvalue(value), namespace);
end;

type TXQueryBreaker = class(TXQuery);

function TMultipageTemplateReader.parseQuery(const query: string): IXQuery;
var
  found: Integer;
  q: IXQuery;
  qb: TXQueryBreaker;
begin
  //cache all parsed queries
  //this is not for performance reasons, but to ensure that the variables declared by "declare ..." are available in later queries.
  //Once a query is freed, the variables declared there disappear
  found := queryCache.IndexOf(query);
  if found >= 0 then exit(TXQuery(queryCache.Objects[found]));
  q := parser.parseQuery(query);
  qb := TXQueryBreaker(q as TXQuery);
  qb._AddRef;
  queryCache.AddObject(query, qb);
  result := q;
end;

function TMultipageTemplateReader.evaluateQuery(const query: IXQuery): IXQValue;
begin
  result := query.evaluate(parser.HTMLTree);
end;

constructor TMultipageTemplateReader.create(atemplate:TMultiPageTemplate; ainternet: TInternetAccess; patternMatcher: THtmlTemplateParser);
begin
  internet:=ainternet;
  parser:=patternMatcher;
  if parser = nil then parser := THtmlTemplateParser.create;
  if atemplate<>nil then setTemplate(atemplate);
  retryOnConnectionFailures := true;
  queryCache := TXQMapStringObject.Create;
  queryCache.OwnsObjects := false;
  actionTrace.init;
end;


destructor TMultipageTemplateReader.destroy();
var
  i: Integer;
begin
  for i := 0 to queryCache.Count - 1 do
    TXQueryBreaker(queryCache.Objects[i])._Release;
  queryCache.Free;
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
  if act=nil then begin
    if strEndsWith(action, '?') then begin
      delete(action,length(action),1);
      act := findAction(action);
      if act = nil then exit;
    end;
    if act = nil then
      raise ETemplateReader.Create(Format(rsActionNotFound, [action]));
  end;
  callAction(act);
end;

procedure TMultipageTemplateReader.callAction(const action:TTemplateAction);
var
  actionTraceCount: SizeInt;
begin
  if Assigned(onLog) then onLog(self, 'Enter performAction, finternet:', 5); //TODO: parser log

  //OutputDebugString(pchar(lib.defaultVariables.Text));
  Assert(internet<>nil,'Internet nicht initialisiert');

  if action is TTemplateActionMain then begin
    actionTraceCount := actionTrace.count;
    actionTrace.add(TTemplateActionMain(action).name);
  end;
  action.perform(self);
  if action is TTemplateActionMain then actionTrace.count := actionTraceCount;


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

h
