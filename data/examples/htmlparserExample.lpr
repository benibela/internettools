program htmlparserExample;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  Interfaces,
  Classes,
  extendedhtmlparser,  pseudoxpath, FileUtil,sysutils, bbutils, simplehtmltreeparser, internetaccess, simpleinternet,
  rcmdline //<< if you don't have this command line parser unit, you can download it from www.benibela.de
  { you can add units after this };


{$R *.res}

function joined(s: array of string): string;
var
  i: Integer;
begin
  if length(s) = 0 then exit('');
  result := s[0];
  for i:=1 to high(s) do result := result + LineEnding + s[i];
end;



type

{ TProcessingRequest }

 TProcessingRequest = record
  urls: TStringArray;
  extract: string;
  extractExclude, extractInclude: TStringArray;

  follow: string;
  followExclude, followInclude: TStringArray;

  wait: Extended;
  userAgent: string;
  proxy: string;
  post: string;

  quiet: boolean;
  defaultName: string;
  printVariables: set of (pvLog, pvCondensedLog, pvFinal);
  printVariablesTime: set of (pvtImmediate, pvtFinal);
  printTypeAnnotations,  printVariableNames, printNodeXML: boolean;
  outputEncoding: TEncoding;

  procedure initFromCommandLine(cmdLine: TCommandLineReader);
  procedure mergeWithObject(obj: TPXPValueObject);

  procedure setVariables(v: string);
  procedure setVariablesTime(v: string);

  procedure deleteUrl0;
  procedure addBasicValueUrl(dest: tpxpvalue);

  procedure printStatus(s: string);
  procedure printExtractedValue(value: TPXPValue);
  procedure printExtractedVariables(vars: TPXPVariableChangeLog; state: string);
  procedure printExtractedVariables(parser: THtmlTemplateParser);
end;

type EInvalidArgument = Exception;
{ TProcessingRequest }

procedure TProcessingRequest.initFromCommandLine(cmdLine: TCommandLineReader);
var
  tempSplitted: TStringArray;
begin
  if cmdLine.readString('extract-file') <> '' then extract := strLoadFromFile(cmdLine.readString('extract-file'))
  else extract := cmdLine.readString('extract');
  extract := trim(extract);
  extractExclude := strSplit(cmdLine.readString('extract-exclude'), ',', false);
  extractInclude := strSplit(cmdLine.readString('extract-include'), ',', false);

  if cmdLine.readString('follow-file') <> '' then follow := strLoadFromFile(cmdLine.readString('follow-file'))
  else follow := cmdLine.readString('follow');
  follow := trim(follow);
  followExclude := strSplit(cmdLine.readString('follow-exclude'), ',', false);
  followInclude := strSplit(cmdLine.readString('follow-include'), ',', false);

  wait := cmdLine.readFloat('wait');
  userAgent := cmdLine.readString('user-agent');
  proxy := cmdLine.readString('proxy');
  post := cmdLine.readString('post');

  quiet := cmdLine.readFlag('quiet');
  defaultName := cmdLine.readString('default-variable-name');
  printTypeAnnotations:=cmdLine.readFlag('print-type-annotations');
  printVariableNames := cmdLine.readFlag('print-variable-names');

  setVariables(cmdLine.readString('print-variables'));
  setVariablesTime(cmdLine.readString('print-variables-time'));
  outputEncoding:=strEncodingFromName(cmdLine.readString('output-encoding'));


  if cmdLine.readString('printed-node-format') <> '' then begin
    if cmdLine.readString('printed-node-format') = 'xml' then printNodeXML:=true
    else if cmdLine.readString('printed-node-format') = 'text' then printNodeXML:=false
    else raise EInvalidArgument.create('Unknown option: '+cmdLine.readString('printed-node-format'));
  end;

  urls:=cmdLine.readNamelessFiles();
end;

procedure TProcessingRequest.mergeWithObject(obj: TPXPValueObject);
var
  temp: TPXPValue;
  tempSplitted: TStringArray;
begin
  if obj.hasProperty('extract-file', @temp) then extract := temp.asString
  else if obj.hasProperty('extract', @temp) then extract := temp.asString;
  if obj.hasProperty('extract-exclude', @temp) then extractExclude := strSplit(temp.asString, ',', false);
  if obj.hasProperty('extract-include', @temp) then extractInclude := strSplit(temp.asString, ',', false);

  if obj.hasProperty('follow-file', @temp) then follow := strLoadFromFile(temp.asString)
  else if obj.hasProperty('follow', @temp) then follow := temp.asString;
  if obj.hasProperty('follow-exclude', @temp) then followExclude := strSplit(temp.asString, ',', false);
  if obj.hasProperty('follow-include', @temp) then followInclude := strSplit(temp.asString, ',', false);

  if obj.hasProperty('wait', @temp) then wait := temp.asDecimal;
  if obj.hasProperty('user-agent', @temp) then userAgent := temp.asString;
  if obj.hasProperty('proxy', @temp) then proxy := temp.asString;
  if obj.hasProperty('post', @temp) then post := temp.asString;

  if obj.hasProperty('quiet', @temp) then quiet := temp.asBoolean;
  if obj.hasProperty('default-variable-name', @temp) then defaultName := temp.asString;
  if obj.hasProperty('print-type-annotations', @temp) then printTypeAnnotations:=temp.asBoolean;
  if obj.hasProperty('print-variable-names', @temp) then printVariableNames := temp.asBoolean;

  if obj.hasProperty('print-variables', @temp) then setVariables(temp.asString);
  if obj.hasProperty('print-variables-time', @temp) then setVariablesTime(temp.asString);
  if obj.hasProperty('output-encoding', @temp) then outputEncoding:=strEncodingFromName(temp.asString);

  setlength(urls, 0);
  if obj.hasProperty('follow', @temp) then
    addBasicValueUrl(temp);
end;

procedure TProcessingRequest.setVariables(v: string);
var
  tempSplitted: TStringArray;
begin
  printVariables:=[];
  tempSplitted := strSplit(v, ',');
  if arrayIndexOf(tempSplitted, 'log') >= 0 then include(printVariables, pvLog);
  if arrayIndexOf(tempSplitted, 'condensed-log') >= 0 then include(printVariables, pvCondensedLog);
  if arrayIndexOf(tempSplitted, 'final') >= 0 then include(printVariables, pvFinal);
end;

procedure TProcessingRequest.setVariablesTime(v: string);
var
  tempSplitted: TStringArray;
begin
  printVariablesTime:=[];
  tempSplitted := strSplit(v, ',');
  if arrayIndexOf(tempSplitted, 'immediate') >= 0 then include(printVariablesTime, pvtImmediate);
  if arrayIndexOf(tempSplitted, 'final') >= 0 then include(printVariablesTime, pvtFinal);
end;


procedure TProcessingRequest.deleteUrl0;
var
  i: Integer;
begin
  for i:=1 to high(urls) do
    urls[i-1] := urls[i];
  setlength(urls,length(urls)-1);
end;

procedure TProcessingRequest.addBasicValueUrl(dest: tpxpvalue);
var
  n: TTreeElement;
  i: Integer;
begin
  case dest.kind of
    pvkUndefined: exit;
    pvkNode: begin
      n := dest.asNode;
      if n = nil then exit;
      if n.typ <> tetOpen then arrayAdd(urls, dest.asString)
      else if SameText(n.value, 'a') then arrayAdd(urls, n.getAttribute('href', ''))
      else if SameText(n.value, 'frame') or SameText(n.value, 'iframe') then arrayAdd(urls, n.getAttribute('src', ''))
      else arrayAdd(urls, n.deepNodeText());
    end;
    pvkSequence:
      for i:=0 to TPXPValueSequence(dest).seq.Count-1 do
        addBasicValueUrl(TPXPValueSequence(dest).seq[i]);
    else arrayAdd(urls, dest.asString);
  end;
end;

procedure TProcessingRequest.printStatus(s: string);
begin
  if not quiet then writeln(stderr, s);
end;

procedure TProcessingRequest.printExtractedValue(value: TPXPValue);
var
  i: Integer;
  temp: TPXPValueObject;
begin
  if printTypeAnnotations then write(value.typeName+': ');
  if value is TPXPValueSequence then begin
    for i:=0 to TPXPValueSequence(value).seq.Count-1 do begin
      printExtractedValue(TPXPValueSequence(value).seq[i]);
      writeln;
    end;
  end else if printNodeXML and (value is TPXPValueNode) then
    write(value.asNode.outerXML())
  else if value is TPXPValueObject then begin
    temp := TPXPValueObject(value.clone);
    write('{');
    if temp.values.count > 0 then begin
      write(temp.values.getVariableName(0),': '); printExtractedValue(temp.values.getVariableValue(0));
      for i:=1 to temp.values.count-1 do begin
        write(', ', temp.values.getVariableName(i), ': ');
        printExtractedValue(temp.values.getVariableValue(i));
      end;
      temp.free;
    end;
    write('}');
  end
  else write(value.asString);
end;

procedure TProcessingRequest.printExtractedVariables(parser: THtmlTemplateParser);
begin
  if pvFinal in printVariables then
    printExtractedVariables(parser.variables, '** Current variable state: **');

  if pvLog in printVariables then
    printExtractedVariables(parser.variableChangeLog, '** Current variable state: **');

  if pvCondensedLog in printVariables then
    printExtractedVariables(parser.VariableChangeLogCondensed, '** Current variable state: **');
end;

procedure TProcessingRequest.printExtractedVariables(vars: TPXPVariableChangeLog; state: string);
var
  i: Integer;
begin
  printStatus(state);
  for i:=0 to vars.count-1 do
    if ((length(extractInclude) = 0) and (arrayIndexOf(extractExclude, vars.getVariableName(i)) = -1)) or
       ((length(extractInclude) > 0) and (arrayIndexOf(extractInclude, vars.getVariableName(i)) > -1)) then begin
      if printVariableNames then write(vars.getVariableName(i) + ': ');
      printExtractedValue(vars.getVariableValue(i));
      writeln;
    end;
end;

var
  requests: array of TProcessingRequest;

procedure followTo(dest: TPXPValue);
var
  n: TTreeElement;
begin
  case dest.kind of
    pvkObject: begin //this can't imho be in addBasicValueUrl, because you can't reliable call a method on a  record in an array that will be modified
      SetLength(requests, length(requests) + 1);
      requests[high(requests)] := requests[0];
      requests[high(requests)].mergeWithObject(dest as TPXPValueObject);
    end;
    else requests[0].addBasicValueUrl(dest);
  end;
end;


type

{ THtmlTemplateParserBreaker }

 THtmlTemplateParserBreaker = class(THtmlTemplateParser)
  function createPXP: TPseudoXPathParser;
  procedure parseHTMLSimple(html,uri: string);
end;

var mycmdLine: TCommandLineReader;
    htmlparser:THtmlTemplateParserBreaker;
    i: Integer;
    temp: TStringArray;
    j: Integer;
    data: string;

    alreadyProcessed: TStringList;
    xpathparser: TPseudoXPathParser;

{ THtmlTemplateParserBreaker }

function THtmlTemplateParserBreaker.createPXP: TPseudoXPathParser;
begin
  result := inherited createPseudoXPathParser;
end;

procedure THtmlTemplateParserBreaker.parseHTMLSimple(html, uri: string);
begin
  FHTML.trimText := FTrimTextNodes = ttnWhenLoading;
  FHtmlTree := FHTML.parseTree(html, uri);

  //encoding trouble
  FHtmlTree.setEncoding(outputEncoding,true,true);

  if FTrimTextNodes = ttnWhenLoadingEmptyOnly then
    FHTML.removeEmptyTextNodes(true);
end;

begin
  //normalized formats (for use in unittests)
  DecimalSeparator:='.';
  ThousandSeparator:=#0;
  ShortDateFormat:='YYYY-MM-DD';
  LongDateFormat:='YYYY-MM-DD';

  mycmdLine:=TCommandLineReader.create;

  mycmdLine.beginDeclarationCategory('Extraction options:');

  mycmdLine.declareString('extract', joined(['Expression to extract from the data.','If it starts with < it is interpreted as template, otherwise as XPath 2 expression']));
  mycmdLine.declareString('extract-exclude', 'Comma separated list of variables ignored in an extract template. (black list) (default _follow)', '_follow');
  mycmdLine.declareString('extract-include', 'If not empty, comma separated list of variables to use in an extract template (white list)');
  mycmdLine.declareFile('extract-file', 'File containing an extract expression (for longer expressions)');

  mycmdLine.beginDeclarationCategory('Follow options:');

  mycmdLine.declareString('follow', joined(['Expression extracting links from the page which will be followed.', 'If the expression extracts a sequence, all elements are followed.', 'If the value is an "a" node, its @href attribute is followed, if it is a "i/frame" node its @src attribute is followed, otherwise its text().', 'If it is an object, its url properties and its other properties can override command line arguments','Otherwise, the string value is treated as url.']));
//  mycmdLine.declareString('follow-level', '');
  mycmdLine.declareString('follow-exclude', 'Comma separated list of variables ignored in an follow template. (black list)');
  mycmdLine.declareString('follow-include', 'Comma separated list of variables used in an foloow template. (white list)');
  mycmdLine.declareFile('follow-file', 'File containing an follow expression (for longer expressions)');

  mycmdLine.beginDeclarationCategory('Extraction options:');


  mycmdLine.beginDeclarationCategory('HTTP connection options:');

  mycmdLine.declareFloat('wait', 'Wait a certain count of seconds between requests');
  mycmdLine.declareString('user-agent', 'Useragent used in http request', defaultInternetConfiguration.userAgent);
  mycmdLine.declareString('proxy', 'Proxy used for http/s requests');
  mycmdLine.declareString('post', 'Post request to send (url encoded)');

  mycmdLine.beginDeclarationCategory('Output options:');

  mycmdLine.declareFlag('quiet','Do not print status information to stderr', 'q');
  mycmdLine.declareString('default-variable-name', 'Variable name for values read in the template without explicitely given variable name', 'result');
  mycmdLine.declareString('print-variables', joined(['Which of the separate variable lists are printed', 'Comma separated list of:', '  log: Prints every variable value', '  final: Prints only the final value of a variable, if there are multiple assignments to it', '  condensed-log: Like log, but removes assignments to object properties(default)']), 'condensed-log');
  mycmdLine.declareString('print-variables-time', joined(['When the template variables are printed. ', 'Comma separated list of:', '  immediate: Prints the variable values after processing each file (default)', '  final: Print the variable values after processing all pages']), 'immediate');
  mycmdLine.declareFlag('print-type-annotations','Prints all variable values with type annotations (e.g. string: abc, instead of abc)');
  mycmdLine.declareFlag('print-variable-names','Prints the name of variables defined in an extract template');
  mycmdLine.declareString('printed-node-format', 'Format of an extracted node: text or xml');
  mycmdLine.declareString('output-encoding', 'Character encoding of the output. utf8 (default), latin1, or input (no encoding conversion)', 'utf8');

  mycmdLine.parse();


  SetLength(requests,1);
  requests[0].initFromCommandLine(mycmdLine);

  defaultInternetConfiguration.userAgent:=requests[0].userAgent;
  defaultInternetConfiguration.setProxy(requests[0].proxy);

  htmlparser:=THtmlTemplateParserBreaker.create;
  xpathparser := htmlparser.createPXP;
  alreadyProcessed := TStringList.Create;


  try
    while length(requests) > 0 do begin
      while (length(requests[0].urls) > 0) and (alreadyProcessed.indexOf(requests[0].urls[0]+#1+requests[0].post) >= 0) do
        requests[0].deleteUrl0;

      if length(requests[0].urls) = 0 then begin
        for i:=1 to high(requests) do
          requests[i-1] := requests[i];
        setlength(requests,length(requests)-1);
        continue;
      end;

      with requests[0] do begin
        urls[0] := trim(urls[0]);
        if urls[0] = '' then begin deleteUrl0; continue; end;

        if strBeginsWith(urls[0], 'http') and strContains(urls[0], '://')  then begin
          simpleinternet.needInternetAccess;
          if assigned(simpleinternet.defaultInternet.internetConfig) then begin
            simpleinternet.defaultInternet.internetConfig^.userAgent := userAgent;
            simpleinternet.defaultInternet.internetConfig^.setProxy(proxy);
          end;
        end;

        printStatus('**** Retrieving:'+urls[0]+' ****');
        if post = '' then data := retrieve(urls[0])
        else data := httpRequest(urls[0], post);

        alreadyProcessed.Add(urls[0]+#1+post);

        printStatus('**** Processing:'+urls[0]+' ****');
        if extract <> '' then begin
          htmlparser.OutputEncoding := outputEncoding;

          if extract[1] = '<' then begin //assume my template
            htmlparser.parseTemplate(extract); //todo reuse existing parser
            htmlparser.parseHTML(data, urls[0]);
            printExtractedVariables(htmlparser);

            for i := 0 to htmlparser.variableChangeLog.count-1 do
              if htmlparser.variableChangeLog.getVariableName(i) = '_follow' then
                followTo(htmlparser.variableChangeLog.getVariableValue(i));
          end else begin
            //assume xpath
            htmlparser.parseHTMLSimple(data, urls[0]);
            xpathparser.RootElement := htmlparser.HTMLTree;
            xpathparser.ParentElement := xpathparser.RootElement;
            xpathparser.StaticBaseUri := urls[0];
            xpathparser.parse(extract);
            printExtractedValue(xpathparser.evaluate());
            writeln;
          end;
        end;

        if follow <> '' then begin
          htmlparser.OutputEncoding := outputEncoding; //todo correct encoding?

          if follow[1] = '<' then begin //assume my template
            htmlparser.parseTemplate(follow); //todo reuse existing parser
            htmlparser.parseHTML(data, urls[0]);
            for i:=0 to htmlparser.variableChangeLog.count-1 do
              if ((length(followInclude) = 0) and (arrayIndexOf(followExclude, htmlparser.variableChangeLog.getVariableName(i)) = -1)) or
                 ((length(followInclude) > 0) and (arrayIndexOf(followInclude, htmlparser.variableChangeLog.getVariableName(i)) > -1)) then
                followTo(htmlparser.variableChangeLog.getVariableValue(i));
          end else begin
            //assume xpath
            htmlparser.parseHTMLSimple(data, urls[0]);
            xpathparser.RootElement := htmlparser.HTMLTree;
            xpathparser.ParentElement := xpathparser.RootElement;
            xpathparser.StaticBaseUri := urls[0];
            xpathparser.parse(follow);
            followTo(xpathparser.evaluate());
          end;
        end;

        deleteUrl0;
        if wait > 0.001 then Sleep(trunc(wait * 1000));
      end;
    end;
  except
    on e: EHTMLParseException do begin
      writeln(stderr, 'Parsing error:');
      writeln(stderr, e.Message);
      writeln(stderr, 'Partial matching:');
      temp := strSplit(htmlparser.debugMatchings(50), LineEnding); //print line by line, or the output "disappears"
      for j := 0 to high(temp) do  writeln(stderr, temp[j]);

      raise;
    end;
  end;
  xpathparser.free;
  htmlparser.free;
  mycmdLine.free;
  alreadyProcessed.Free;


end.

