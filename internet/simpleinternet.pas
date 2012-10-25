(***
  @abstract(@bold(This unit contains a very simple procedural interface to make http requests and @noAutoLink(process) the returned data.))@br@br

  To get started, you can just use the function retrieve('...url...') to get the data you need as a string. @br

  Retrieve also supports file:// urls or other data source; if you want to only get http sites or make a http POST request, you have to use httpRequest.@br@br

  The data can then be processed with process, with applies an PXPath expression or a html template to it.

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



//**Make a http GET request to a certain url
function httpRequest(url: string): string;
//**Make a http POST request to a certain url, sending the data in rawpostdata unmodified to the server
function httpRequest(url: string; rawpostdata: string): string;
//**Make a http POST request to a certain url, sending the data in postdata to the server, after url encoding all name=value pairs of it
function httpRequest(url: string; postdata: TStringList): string;

(***
Processes data with a certain query.@br@br

data can be an url, or a html/xml file in a string.@br@br


query can be a @link(pseudoxpath.TPseudoXPathParser PXPath expression), like in @code(process('http://www.google.de', '/html/head/title');). @br@br

Or query can be a @link(extendedhtmlparser.THtmlTemplateParser html template) like @code(process('http://www.google.de', '<title><t:s>result:=text()</t:s></title>');)
The advantage of such templates is that they can return several variables at once and a canonical representation of the same data on different web sites.
To get a list of all variables of the last query you can use processedVariables.@br@br

The function processedTree will always return a tree representation of the last processed data string.
*)
function process(data: string; query: string): IXQValue;
//**Returns a tree representation of the last processed html/xml data@br
//**Might return nil
function processedTree: TTreeElement;
//**Returns all variable assignments during the last query
function processedVariables: TXQVariableChangeLog;

//**If you use the functions in this unit from different threads, you have to call freeThreadVars
//**before the thread terminates to prevent memory leaks
procedure freeThreadVars;

threadvar defaultInternet: TInternetAccess;

//**Initializes the defaultInternet variable
procedure needInternetAccess;

implementation

{$IFNDEF USE_SYNAPSE_WRAPPER}
{$IFNDEF USE_WININET_WRAPPER}
{$IFNDEF USE_NO_WRAPPER}

//use default wrapper
{$IFDEF WIN32}
{$DEFINE USE_WININET_WRAPPER}
{$ELSE}
{$DEFINE USE_SYNAPSE_WRAPPER}
{$ENDIF}

{$ENDIF}
{$ENDIF}
{$ENDIF}

uses bbutils, extendedhtmlparser
{$IFDEF USE_SYNAPSE_WRAPPER}
, synapseinternetaccess
{$ENDIF}
{$IFDEF USE_WININET_WRAPPER}
, w32internetaccess
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


function process(data: string; query: string): IXQValue;
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
    if strBeginsWith(query, 'xquery') then pxpParser.parseXQuery1(query)
    else pxpParser.parseXPath2(query);
    pxpparser.ParentElement := tree.getLastTree;
    pxpparser.RootElement := tree.getLastTree;
    pxpparser.StaticContext.baseURI:=dataFileName;
    result := pxpParser.evaluate();
  end;
end;

function processedTree: TTreeElement;
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
    if defaultInternetAccessClass = nil then
      raise Exception.Create('You need to set defaultInternetAccessClass to choose between synapse and wininet');
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

finalization
freeThreadVars;

end.

