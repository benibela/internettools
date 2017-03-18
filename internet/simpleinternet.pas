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

//** IXQValue from the xquery unit. Just a wrapper, so that no other unit needs to be included @noAutoLinkHere
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



function httpRequest(url: string): string; overload; deprecated 'The httpRequest functions have been moved to the internetaccess unit.';
function httpRequest(url: string; rawpostdata: string): string; overload; deprecated 'The httpRequest functions have been moved to the internetaccess unit.';
function httpRequest(url: string; postdata: TStringList): string; overload; deprecated 'The httpRequest functions have been moved to the internetaccess unit.';
function httpRequest(const method, url, rawdata: string): string; overload; deprecated 'The httpRequest functions have been moved to the internetaccess unit.';


//**Make a http request to an address given in an IXQValue.  @br
//**node: if a link (a), download @@href. If a resource (img, frame), download @@src. Otherwise download the text@br.
//**object: Download obj.url, possibly sending obj.post as postdata.
//**else: Download the string value.
function httpRequest(const destination: xquery.IXQValue): string;  overload; deprecated 'Use destination.retrieve() instead.';

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
//**before the thread terminates to prevent memory leaks @br
//**This also calls freeThreadVars of the xquery and internetaccess units
procedure freeThreadVars;

function defaultInternet: TInternetAccess; inline;
function defaultQueryEngine: TXQueryEngine; inline;

procedure needInternetAccess; deprecated 'This procedure no longer does anything';

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
  lastQueryWasPXP: boolean;
  lastRetrievedType: TRetrieveType;


function retrieve(data: string): string;
var trimmed: string;
begin
  trimmed:=TrimLeft(data);
  lastRetrievedType:=guessType(data);
  case lastRetrievedType of
    rtEmpty: exit('');
    rtRemoteURL: exit(internetaccess.httpRequest(trimmed));
    rtFile: exit(strLoadFromFileUTF8(trimmed));
    else exit(data);
  end;
end;

function lastContentType: string;
begin
  if lastRetrievedType = rtRemoteURL then result := defaultInternet.getLastContentType
  else result := '';
end;

type TXQueryEngineBreaker = class(TXQueryEngine)
end;

function process(data: string; query: string): xquery.IXQValue;
var dataFileName: string;
  datain: String;
  tempVars: TXQVariableChangeLog;
  context: TXQEvaluationContext;
  format: TInternetToolsFormat;
  querykind: TExtractionKind;
  pxpParser: TXQueryEngine;
begin
  result := xqvalue();
  if query = '' then exit();

  datain := data;

  data := retrieve(data);

  if lastRetrievedType in [rtFile, rtRemoteURL] then dataFileName:=fileNameExpandToURI(datain);
  format := guessFormat(data, dataFileName, lastContentType);

  query := trim(query);
  querykind := guessExtractionKind(query);

  case querykind of
    ekPatternHTML, ekPatternXML: begin
      lastQueryWasPXP := false;
      if templateParser = nil then begin
        templateParser := THtmlTemplateParser.create;
        if querykind = ekPatternXML then templateParser.TemplateParser.parsingModel:= pmStrict
        else templateParser.TemplateParser.parsingModel:= pmHTML;
        templateParser.TemplateParser.repairMissingStartTags := false;
        templateParser.HTMLParser.repairMissingStartTags := format = itfHTML;
      end;
      templateParser.parseTemplate(query);
      templateParser.parseHTML(data, dataFileName);
      if templateParser.variableChangeLog.count > 0 then begin
        tempVars := templateParser.VariableChangeLogCondensed.collected;
        if (tempVars.count = 1) and (tempVars.getName(0) = templateParser.UnnamedVariableName) then begin
          result := tempVars.get(0);
          tempVars.free;
        end else result := TXQValueObject.createTakingVariableLog(tempVars);
      end;
    end;
    else begin
      lastQueryWasPXP := true;
      pxpParser := defaultQueryEngine;
      pxpparser.StaticContext.baseURI:=dataFileName;
      TXQueryEngineBreaker(pxpParser).addAWeirdGlobalVariable('', 'json');
      case querykind of
        ekXQuery1, ekXQuery3: begin
          pxpParser.ParsingOptions.StringEntities := xqseResolveLikeXQuery;
          pxpParser.parseXQuery3(query, pxpParser.StaticContext);
        end;
        ekXPath2, ekXPath3: begin
          pxpParser.ParsingOptions.StringEntities := xqseIgnoreLikeXPath;
          pxpParser.parseXQuery3(query, pxpParser.StaticContext) //no point in using a less powerful language.
        end;
        ekCSS: pxpParser.parseCSS3(query);
        else raise Exception.Create('internal error 21412466');
      end;
      context := pxpParser.getEvaluationContext();

      if format <> itfJSON then begin
        if tree = nil then begin
          tree := TTreeParser.Create;
          tree.parsingModel:=pmHTML;
        end;
        tree.repairMissingStartTags := format = itfHTML;
        tree.parseTree(data, dataFileName);
        context.ParentElement := tree.getLastTree;
        context.RootElement := tree.getLastTree;
      end else
        pxpParser.VariableChangelog.add('json', parseJSON(data)); //TODO: this is bad, it leaks all JSON data, till the engine is reset

      result := pxpParser.evaluate(context);
    end;
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
  templateParser.Free; templateParser := nil;
  tree.Free; tree := nil;
  xquery.freeThreadVars;
end;

function defaultInternet: TInternetAccess;
begin
  result := internetaccess.defaultInternet;
end;

function defaultQueryEngine: TXQueryEngine;
begin
  result := xquery.defaultQueryEngine;
end;

procedure needInternetAccess;
begin
end;


function httpRequest(url: string): string;
begin
  result:=defaultInternet.get(url);
end;

function httpRequest(url: string; rawpostdata: string): string;
begin
  result:=defaultInternet.post(url, rawpostdata);
end;

function httpRequest(url: string; postdata: TStringList): string;
begin
  result := internetaccess.httpRequest(url, TInternetAccess.urlEncodeData(postdata));
end;

function httpRequest(const method, url, rawdata: string): string;
begin
  result := defaultInternet.request(method, url, rawdata);
end;

function httpRequest(const destination: xquery.IXQValue): string;
var dest: IXQValue;
  tempHeaders: String;
  method: string;
  url: string;
  post: string;
begin
  if destination.kind = pvkSequence then dest := destination.get(1)
  else dest := destination;
  dest := defaultQueryEngine.evaluateXPath3('pxp:resolve-html(.)', dest);
  if dest.kind = pvkSequence then dest := dest.get(1);

  case dest.kind of
    pvkUndefined: exit('');
    pvkNode: raise EXQEvaluationException.Create('pxp:ASSERT', 'Got '+dest.toXQuery()+', but expected resolved url');
    pvkObject: begin
      tempHeaders := defaultInternet.additionalHeaders.Text;
      TXQValueObject.prepareInternetRequest(dest, method, url, post, defaultInternet);
      result := internetaccess.httpRequest(method, url, post);
      defaultInternet.additionalHeaders.Text := tempHeaders;
    end;
    pvkSequence: raise EXQEvaluationException.Create('pxp:ASSERT', 'Impossible (nested) sequence');
    else result := internetaccess.httpRequest(dest.toString);
  end;
end;



finalization
  freeThreadVars;

end.

