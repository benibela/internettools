(***
  @abstract(@bold(This unit contains a very simple procedural interface to make http requests and @noAutoLink(process) the returned data.))@br@br

  To get started, you can just use the function retrieve('...url...') to get the data you need as a string. @br

  Retrieve also supports file:// urls or other data source; if you want to restrict it to http GET/POST-requests only, you can use the httpRequest function.@br@br

  The data can then be processed with process which applies an XPath/XQuery expression or a html template to it.@br@br

  @bold(Example)

  Get all links on a page:

  @longCode(#
    for v in process('http://www.freepascal.org', '//a') do
      writeln(v.toString, ' => ', v.toNode['href']);
  #)



*)
unit simpleinternet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xquery, internetaccess, simplehtmltreeparser;

//Usually this unit uses synapse on linux and wininet on windows, but you can choose a certain wrapper below:

//{$DEFINE USE_SYNAPSE_WRAPPER}
//{$DEFINE USE_WININET_WRAPPER}
//{$DEFINE USE_NO_WRAPPER}

//** IXQValue from the xquery unit. Just a wrapper, so that no other unit needs to be included
type IXQValue = xquery.IXQValue;

(***
Retrieve data from any url.@br@br

It is really simple to use, you  pass the desired url as single parameter and get the data of the url.@br@br

It supports:

@unorderedList(
@item(http://...)
@item(https://...)
@item(file://...)
@item(/normal/unix/file/paths)
@item(C:\normal\windows\paths)
@item(<html>data</html>)
)

*)
function retrieve(data: string): string;



//**Make a http GET request to a certain url.
function httpRequest(url: string): string;
//**Make a http POST request to a certain url, sending the data in rawpostdata unmodified to the server.
function httpRequest(url: string; rawpostdata: string): string;
//**Make a http POST request to a certain url, sending the data in postdata to the server, after url encoding all name=value pairs of it.
function httpRequest(url: string; postdata: TStringList): string;
//**Make a http request to an address given in an IXQValue.  @br
//**node: if a link (a), download @@href. If a resource (img, frame), download @@src. Otherwise download the text@br.
//**object: Download obj.url, possibly sending obj.post as postdata.
//**else: Download the string value.
function httpRequest(const destination: xquery.IXQValue): string;

(***
Processes data with a certain query.@br@br

data can be an url, or a html/xml file in a string, like in retrieve.@br@br


query can be a @link(xquery.TXQueryEngine XPath/XQuery expression), like in @code(process('http://www.google.de', '//title');). @br@br

Or query can be a @link(extendedhtmlparser.THtmlTemplateParser html template) like @code(process('http://www.google.de', '<title><t:s>result:=text()</t:s></title>');)
The advantage of such templates is that they can return several variables at once and a canonical representation of the same data on different web sites.
To get a list of all variables of the last query you can use processedVariables.@br@br

This function returns an IXQValue value, which is a variant for XQuery expression.
If you want a string value, you can convert it like @code(process(...).toString). Or if you want to access a retrieved node directly, you can use @code(process(..).toNode).@br
It can also contain multiple values, which can be access like @code(for x in process(..)), where x is another IXQValue.


The global function processedTree returns a tree representation of the last processed data string.



*)
function process(data: string; query: string): xquery.IXQValue;
//**Returns a tree representation of the last processed html/xml data@br
//**Might return nil
function processedTree: TTreeNode;
//**Returns all variable assignments during the last query
function processedVariables: TXQVariableChangeLog;

//**If you use the functions in this unit from different threads, you have to call freeThreadVars
//**before the thread terminates to prevent memory leaks
procedure freeThreadVars;

threadvar defaultInternet: TInternetAccess;
function defaultQueryEngine: TXQueryEngine;

//**Initializes the defaultInternet variable
procedure needInternetAccess;

implementation

{$IFNDEF USE_SYNAPSE_WRAPPER}
{$IFNDEF USE_WININET_WRAPPER}
{$IFNDEF USE_ANDROID_WRAPPER}
{$IFNDEF USE_NO_WRAPPER}

//use default wrapper
{$IFDEF WINDOWS}
{$DEFINE USE_WININET_WRAPPER}
{$ELSE}
{$IFDEF ANDROID}
{$DEFINE USE_ANDROID_WRAPPER}
{$ELSE}
{$DEFINE USE_SYNAPSE_WRAPPER}
{$ENDIF}
{$ENDIF}

{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

uses bbutils, extendedhtmlparser
     ,xquery_json //enable json as default
{$IFDEF USE_SYNAPSE_WRAPPER}
, synapseinternetaccess
{$ENDIF}
{$IFDEF USE_WININET_WRAPPER}
, w32internetaccess
{$ENDIF}
{$IFDEF USE_ANDROID_WRAPPER}
, androidinternetaccess      //if the androidinternetaccess unit is not found, you need to add the directory containing this (simpleinternet) unit to the search paths
                             //(it is not included in the lpk, because it depends on the lcl, while all other units depend only on the fcl)
{$ENDIF}
;


threadvar
  tree: TTreeParser;
  templateParser: THtmlTemplateParser;
  pxpParser: TXQueryEngine;
  lastQueryWasPXP: boolean;
  lastRetrievedType: TRetrieveType;


function retrieve(data: string): string;
var trimmed: string;
begin
  trimmed:=TrimLeft(data);
  lastRetrievedType:=guessType(data);
  case lastRetrievedType of
    rtEmpty:;
    rtRemoteURL: exit(httpRequest(trimmed));
    rtFile: exit(strLoadFromFileUTF8(trimmed));
    rtXML: exit(data);
  end;
end;


function process(data: string; query: string): xquery.IXQValue;
var dataFileName: string;
  datain: String;
  tempVars: TXQVariableChangeLog;
begin
  result := xqvalue();
  if query = '' then exit();

  datain := data;

  data := retrieve(data);

  if lastRetrievedType <> rtXML then dataFileName:=datain;

  query := trim(query);
  if query[1] = '<' then begin
    lastQueryWasPXP := false;
    if templateParser = nil then begin templateParser := THtmlTemplateParser.create; templateParser.TemplateParser.parsingModel:= pmHTML;end;
    templateParser.parseTemplate(query);
    templateParser.parseHTML(data, dataFileName);
    if templateParser.variableChangeLog.count > 0 then begin
      tempVars := templateParser.VariableChangeLogCondensed.collected;
      if (tempVars.count = 1) and (tempVars.getName(0) = templateParser.UnnamedVariableName) then begin
        result := tempVars.get(0);
        tempVars.free;
      end else result := TXQValueObject.createTakingVariableLog(tempVars);
    end;
  end else begin
    lastQueryWasPXP := true;
    if tree = nil then begin
      tree := TTreeParser.Create;
      tree.parsingModel:=pmHTML;
    end;
    tree.parseTree(data, dataFileName);
    if pxpParser = nil then pxpParser := TXQueryEngine.create;
    pxpparser.StaticContext.baseURI:=dataFileName;
    if strBeginsWith(query, 'xquery') then pxpParser.parseXQuery1(query)
    else pxpParser.parseXPath2(query);
    pxpparser.ParentElement := tree.getLastTree;
    pxpparser.RootElement := tree.getLastTree;
    result := pxpParser.evaluate();
  end;
end;

function processedTree: TTreeNode;
begin
  if lastQueryWasPXP then begin
    if tree = nil then exit(nil);
    result := tree.getLastTree;
  end else if templateParser = nil then exit(nil)
  else result := templateParser.HTMLTree;
end;

function processedVariables: TXQVariableChangeLog;
begin
  if templateParser = nil then templateParser := THtmlTemplateParser.create;
  result := templateParser.variableChangeLog;
end;

procedure freeThreadVars;
begin
  pxpParser.Free; pxpParser := nil;
  defaultInternet.Free; defaultInternet := nil;
  templateParser.Free; templateParser := nil;
  tree.Free; tree := nil;
end;

function defaultQueryEngine: TXQueryEngine;
begin
  result := pxpParser;
end;

procedure needInternetAccess;
begin
  if defaultInternet <> nil then exit;
  if defaultInternetAccessClass = nil then begin
    {$IFDEF USE_SYNAPSE_WRAPPER}
    defaultInternetAccessClass := TSynapseInternetAccess;
    {$ENDIF}
    {$IFDEF USE_WININET_WRAPPER}
    defaultInternetAccessClass := TW32InternetAccess;
    {$ENDIF}
    {$IFDEF USE_ANDROID_WRAPPER}
    defaultInternetAccessClass := TAndroidInternetAccess;
    {$ENDIF}
    if defaultInternetAccessClass = nil then
      raise Exception.Create('You need to set defaultInternetAccessClass to choose between synapse, wininet or android');
  end;
  defaultInternet := defaultInternetAccessClass.create;
end;

function httpRequest(url: string): string;
begin
  needInternetAccess();
  //writeln(stderr, url);
  result:=defaultInternet.get(url);
end;

function httpRequest(url: string; rawpostdata: string): string;
begin
  needInternetAccess();
  result:=defaultInternet.post(url, rawpostdata);
end;

function httpRequest(url: string; postdata: TStringList): string;
begin
  result := httpRequest(url, TInternetAccess.urlEncodeData(postdata));
end;

function httpRequest(const destination: xquery.IXQValue): string;
var dest: IXQValue;
begin
  if destination.kind = pvkSequence then dest := destination.getChild(1)
  else dest := destination;
  dest := pxpParser.evaluateXPath3('pxp:resolve-html(.)', dest);
  if dest.kind = pvkSequence then dest := dest.getChild(1);

  case dest.kind of
    pvkUndefined: exit('');
    pvkNode: raise EXQEvaluationException.Create('pxp:ASSERT', 'Got '+dest.debugAsStringWithTypeAnnotation()+', but expected resolved url');
    pvkObject:
      if dest.getProperty('method').toString <> 'GET' then result := httpRequest(dest.getProperty('url').toString, dest.getProperty('post').toString)
      else result := httpRequest(dest.getProperty('url').toString);
    pvkSequence: raise EXQEvaluationException.Create('pxp:ASSERT', 'Impossible (nested) sequence');
    else result := httpRequest(dest.toString);
  end;
end;

finalization
freeThreadVars;

end.

