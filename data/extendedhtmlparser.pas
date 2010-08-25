{**
  @abstract This units contains a template based html parser named THtmlTemplateParser

  @author Benito van der Zander (http://www.benibela.de)
*}

{
Copyright (C) 2008 Benito van der Zander (BeniBela)
                   benito@benibela.de
                   www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

unit extendedhtmlparser;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils,simplehtmlparser,simplehtmltreeparser,pseudoxpath,
    dRegExpr, //this should contain TRegExpr from  Andrey V. Sorokin (regexpstudio.com -- page dead, I create a mirror on benibela.de) (his file is named regexpr, but you should rename is to differentiate it from fpc regexpr)
    bbutils;


type
TTemplateElementType=(tetHTML, tetCommandMeta, tetCommandRead,tetCommandLoop,tetCommandIf);

TNotifyCallbackFunction = procedure () of object;
TVariableCallbackFunction = procedure (variable: string; value: string) of object;
TReadCallbackFunction = procedure (read: pchar; readLen:longint) of object;

TReplaceFunction = procedure (variable: string; var value:string) of object;

ETemplateParseException = Exception;

{ TTemplateElement }

TTemplateElement=class(TTreeElement)
  templateType: TTemplateElementType;
  function isOptional: boolean;
  procedure initialized;override;
end;

{ THtmlTemplateParser }

THtmlTemplateParser=class
private
  Fvariables: TStringList;
  protected
    templateEncoding,htmlEncoding, outputEncoding: TEncoding;

    FTemplate, FHTML: TTreeParser;
    FTemplateName: string;

    FPseudoXPath: TPseudoXPathParser;
  protected
    FCurrentTemplateName: string; //currently loaded template, only needed for debugging (a little memory waste)
    //FCurrentStack: TStringList;
    FOnVariableRead: TVariableCallbackFunction;

    //function readTemplateElement(status:TParsingStatus):boolean; //gibt false nach dem letzten zurück
    function executePseudoXPath(str: string; replaceVariables: boolean=true):string;
    //procedure executeTemplateCommand(status:TParsingStatus;cmd: TTemplateElement;afterReading:boolean);
    //function getTemplateElementDebugInfo(element: TTemplateElement): string;
  public
    constructor create;
    destructor destroy; override;


    procedure parseHTML(html: string); //**< parses the given data
    procedure parseHTMLFile(htmlfilename: string); //**< parses the given file
    procedure parseTemplate(template: string; templateName: string='<unknown>');//**< loads the given template, stores templateName for debugging issues
    procedure parseTemplateFile(templatefilename: string);
    //procedure addFunction(name:string;varCallFunc: TVariableCallbackFunction);overload;
    //procedure addFunction(name:string;notifyCallFunc: TNotifyCallbackFunction);overload;

    //**This replaces every $variable; in s with variables.values['variable'] or the value returned by customReplace
    function replaceVars(s:string;customReplace: TReplaceFunction=nil):string;

    property variables: TStringList read Fvariables;//**<List of all variables
  end;

implementation

const TEMPLATE_COMMANDS=[tetCommandMeta..tetCommandIf];
      COMMAND_CLOSED:array[tetCommandMeta..tetCommandIf] of boolean=(false,false,true,true);
      COMMAND_STR:array[tetCommandMeta..tetCommandIf] of string=('meta','read','loop','if');

{ TTemplateElement }

function strToCommand(s:string): TTemplateElementType;
var i:longint;
    tag:pchar; taglen: integer;
begin
  Result:=tetHTML;
  if strlibeginswith(s,'htmlparser:') then begin
    tag:=@s[length('htmlparser:')];
    taglen:=length(s)-length('htmlparser:');
    for Result:=low(COMMAND_STR) to high(COMMAND_STR) do
      if strliequal(tag,COMMAND_STR[Result],taglen) then
        exit();
    raise ETemplateParseException.Create('Unbekannter Templatebefehl: '+s)
  end;
end;

function TTemplateElement.isOptional: boolean;
begin
  if (typ=tetText) and (value='') then exit(true);
  if (attributes=nil) then exit(false);
  result:=(attributes.Values['htmlparser-optional']='true');
end;

procedure TTemplateElement.initialized;
begin
  //inherited initialized;
  if typ <> tetOpen then exit;
  templateType:=strToCommand(value);
end;

procedure changeEncoding(tree:TTreeElement; from,toe: TEncoding);
begin
  if (from = toe) or (from = eUnknown) or (toe = eUnknown) then exit;
  while tree <> nil do begin
    if tree.typ = tetText then tree.value:=strChangeEncoding(tree.value, from, toe)
    else if tree.attributes <> nil then tree.attributes.text:=strChangeEncoding(tree.attributes.text,from,toe);
    //TODO: convert tree tag names (but this is not necessary as long as only latin1 and utf8 is supported)
    tree := tree.next;
  end;
end;


function THtmlTemplateParser.executePseudoXPath(str: string; replaceVariables: boolean): string;
begin
  if replaceVariables then str := replaceVars(str);
  FPseudoXPath.parse(str);
  result:=FPseudoXPath.evaluate();
end;

constructor THtmlTemplateParser.create;
begin
  FTemplate := TTreeParser.Create;
  FTemplate.parsingModel:=pmStrict;
  FHTML := TTreeParser.Create;
  FHTML.parsingModel:=pmHTML;
  FPseudoXPath := TPseudoXPathParser.Create;
  outputEncoding:=eUTF8;
end;

destructor THtmlTemplateParser.destroy;
begin
  FTemplate.Free;
  FHTML.Free;
  FPseudoXPath.free;
  inherited destroy;
end;

procedure THtmlTemplateParser.parseHTML(html: string);
var
  head: TTreeElement;
  meta: TTreeElement;
  encoding: string;
begin
  FHTML.parseTree(html);

  //encoding trouble
  htmlEncoding:=eUnknown;
  encoding := lowercase(TPseudoXPathParser.Evaluate('html/head/meta[@http-equiv=''content-type'']/@content', FHTML.getTree));
  if encoding <> '' then begin
    if pos('charset=utf-8', encoding) > 0 then htmlEncoding:=eUTF8
    else if (pos('charset=windows-1252',encoding) > 0) or
            (pos('charset=iso-8859-1',encoding) > 0) then
      htmlEncoding:=eWindows1252;
  end;

  changeEncoding(FHTML.getTree, htmlEncoding, outputEncoding);
  changeEncoding(FHTML.getTree, templateEncoding, outputEncoding);
end;

procedure THtmlTemplateParser.parseHTMLFile(htmlfilename: string);
begin
  parseHTML(strLoadFromFile(htmlfilename));
end;

procedure THtmlTemplateParser.parseTemplate(template: string;
  templateName: string);
var el: TTemplateElement;
begin
  templateEncoding:=eUnknown;
  if strbeginswith(template,#$ef#$bb#$bf) then begin
    delete(template,1,3);
    templateEncoding:=eUTF8;
  end else if strbeginswith(template,#$fe#$ff) or strbeginswith(template,#$ff#$fe) or
    strbeginswith(template,#00#00#$fe#$ef) then
    raise Exception.Create('Ungültiger Codierung BOM im Template');

  //read template
  FTemplate.parseTree(template);
  FTemplateName := templateName;

  //evaluate meta
  el := TTemplateElement(FTemplate.getTree);
  while el <> nil do begin
    if el.templateType = tetCommandMeta then begin
      if el.attributes.Values['encoding'] <> '' then begin
        if striequal(el.attributes.Values['encoding'],'utf8') or
            striequal(el.attributes.Values['encoding'],'utf-8') then
            templateEncoding:=eUTF8
          else
            templateEncoding:=eWindows1252;
      end;
    end;
  end;
end;

procedure THtmlTemplateParser.parseTemplateFile(templatefilename: string);
begin
  parseTemplate(strLoadFromFile(templatefilename),templatefilename);
end;

function THtmlTemplateParser.replaceVars(s: string; customReplace: TReplaceFunction): string;
var f,i:longint;
    temp,value:string;
begin
  Result:='';
  i:=1;
  while i<=length(s) do begin
    if s[i]='$' then begin
      f:=i+1;
      while (i<=length(s)) and (s[i]<>';')  do inc(i);
      temp:=copy(s,f,i-f);
      value:=variables.Values[temp];
      if assigned(customReplace) then customReplace(temp,value);
    //  OutputDebugString(pchar(parser.variables.Text));
      result+=value;
    end else Result+=s[i];
    i+=1;
  end;
end;



end.




















interface

uses
  Classes, SysUtils,simplehtmlparser,
    dRegExpr, //this should contain TRegExpr from  Andrey V. Sorokin (regexpstudio.com) (his file is named regexpr, but you should rename is to differentiate it from fpc regexpr)
    bbutils;

{
TODO: - deepNodeText() auch erlauben, wenn andere Befehle vorkommen
      - mögliche Probleme mit leeren Anweisungen untersuchen (leere Datei, Befehle am Anfang/Ende...)
      - Befehle direkt nach/vor einem loop-Befehl ermöglichen
}


type

  EHTMLParseException=class(exception)
  end;
  ETemplateParseException=class(exception)
  end;

  { TTemplateElement }

  TTemplateElement= class
    typ: TTemplateElementType;
    closeTag: boolean;
    text: string; //Name bzw. Inhalt für tetText
    attributes: TStringList;  //nil für tetText
    //children: TList;
    reverse: TTemplateElement; //Schließen/Öffnen
    next,rnext: TTemplateElement;


    offset:longint; //für debugging, bytes between element in template and template start
    function toStr:string;
    procedure freeAll;
    destructor destroy;override;
  end;

type

  { TParsingStatus }

  TParsingStatus=class
    lastElement: TTemplateElement;
    nextElement: TTemplateElement;
    lastopenedElement, latestElement: TTemplateElement; //debugging
    elementStack: TStringList;
    destructor destroy;override;
  end;

  { THtmlTemplateParser }

  {**
    @abstract This is the html parser class
    You can use it simply by calling first parseTemplate to load a given template
    and then parseHTML to parse the html data. @br
    You can access the read variables with the property variables or the event onVariableRead. @br @br
    A template file is just like a html file with special commands. The parser tries now to match every
    text and tag of the template to text/tag in the html file, while ignoring every additional data. If no match is possible an exception is raised. 

    @bold(Examples)

    Example, how to read the first <b>-tag:@br
      Template: @code(<b><htmlparser:read var="test" source="text()"></b>)@br
      Html-File: @code(<b>Hello World!</b>))@br

    This will set the variable test to "Hello World!" @br

    Example, how to read all rows of a table:@br
      Template: @code(<table> <htmlparser:loop> <tr> <td> <htmlparser:read var="readAnotherRow()" source="text()"> </td> </tr> </htmlparser:loop> </table>)@br
      Html-File: @code(<table> <tr> <td> row-cell 1 </td> </tr> <tr> <td> row-cell 2 </td> </tr> ... <tr> <td> row-cell n </td> </tr> </table>)@br

    This will read row after row, and call the @code(onVariableRead) event with the text of the first column in every row. (and additionally the last row will be stored in a variable called @code(readAnotherRow())) @br

    See the unit tests at the end of the file extendedhtmlparser.pas for more examples

    @bold(Syntax of a template file)

    Basically the template file is a html file, and the parser tries to match the structure of the template html file to the html file to parse. @br
    A tag of the html file is considered as equal to an tag of the template file, if the tag names are equal, all attributes are the same (regardless of their order) and every child tag of the tag in the template exists also in the html file (in the same order and nesting).@br
    Text nodes are considered as equal, if the text in the html file starts with the whitespace trimmed text of the template file.



    There are 4 special commands allowed:
     @unorderedList(
        @item(@code(<htmlparser:meta encoding="??"/>) @br Specifies the encoding the template, only windows-1252 and utf-8 allowed)
        @item(@code(<htmlparser:if test="??"/>  .. </htmlparser:if>) @br Everything inside this tag is only used if the pseudo-XPath-expression in test equals to true)
        @item(@code(<htmlparser:loop>  .. </htmlparser:loop>) @br Everything inside this tag is executed as long as possible (including never))
        @item(@code(<htmlparser:read var="??" source="??" [regex="??" [submatch="??"]]/>) @br The pseudo-XPath-expression in source is evaluated and stored in variable of var. If a regex is given, only the matching part is saved. If submatch is given, only the submatch-th match of the regex is returned. (e.g. b will be the 2nd match of "(a)(b)(c)"))
      )
      @br
      There are two special attributes:
      @unorderedList(
        @item(@code(htmlparser-optional="true") @br if this is set the file is read sucessesfully even if the tag doesn't exist.)
        @item(@code(htmlparser-condition="pseudo xpath") @br if this is given, a tag is only accepted as matching, iff the given pxpath-expression returns 'true' (powerful, but slow))
      )

  }
  THtmlTemplateParser=class
  protected
    templateEncoding,htmlEncoding, outputEncoding: TEncoding;
    FTemplateCount:longint;
    FRootTemplate: TTemplateElement;
    FLastHTMLTemplateElement: TTemplateElement;
    FCurrentTemplateElement: TTemplateElement;
    //FLastTemplateElements: TList;
    //FNextTemplateElements: TList;
    FTemplateElementStack: TList;

    function newTemplateElement(text: pchar; textLen: longint):TTemplateElement;
    procedure rememberNewHTMLElement(el: TTemplateElement);
    function strToCommand(tagName: pchar; tagNameLen: longint):TTemplateElementType;
    function templateEnterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
    function templateLeaveTag(tagName: pchar; tagNameLen: longint):boolean;
    function templateTextEvent(text: pchar; textLen: longint):boolean;

  protected
    FParsingCompleted: boolean;
    FlastText,fdeepNodeText:string;
    FCurrentTemplateName, FCurrentTemplate: string; //currently loaded template, only needed for debugging (a little memory waste)
    //FCurrentStack: TStringList;
    FVariables,FNotifyFunctions,FVariableFunctions: TStringList;
    FOldProperties: THTMLProperties;
    FParsingAlternatives: TFPList;
    FAutoCloseTag: boolean;
    FCollectDeepNodeText: boolean; //concat every read text block
    LastEventIsText:boolean;

    FOnEnterTag, FOnLeaveTag, FOnTextRead: TReadCallbackFunction;
    FOnVariableRead: TVariableCallbackFunction;

    function elementIsOptional(element:TTemplateElement): boolean;
    procedure finishHtmlParsing(status:TParsingStatus);
    function readTemplateElement(status:TParsingStatus):boolean; //gibt false nach dem letzten zurück
    function executePseudoXPath(str: string):string;
    procedure executeTemplateCommand(status:TParsingStatus;cmd: TTemplateElement;afterReading:boolean);
    function enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
    function leaveTag(tagName: pchar; tagNameLen: longint):boolean;
    function textEvent(text: pchar; textLen: longint):boolean;

    function getTemplateElementDebugInfo(element: TTemplateElement): string;
  public
    constructor create;
    destructor destroy; override;


    procedure parseHTML(html: string); //**< parses the given data
    procedure parseHTMLFile(htmlfilename: string); //**< parses the given file
    procedure parseTemplate(template: string; templateName: string='<unknown>');//**< loads the given template, stores templateName for debugging issues
    procedure parseTemplateFile(templatefilename: string);
    //procedure addFunction(name:string;varCallFunc: TVariableCallbackFunction);overload;
    //procedure addFunction(name:string;notifyCallFunc: TNotifyCallbackFunction);overload;

    //**This replaces every $variable; in s with variables.values['variable'] or the value returned by customReplace
    function replaceVars(s:string;customReplace: TReplaceFunction=nil):string;

    property variables: TStringList read Fvariables;//**<List of all variables
    property onEnterTag: TReadCallbackFunction read FOnEnterTag write FOnEnterTag; //**< is called when a tag is entered
    property onLeaveTag: TReadCallbackFunction read FOnLeaveTag write FOnLeaveTag; //**< is called when a tag is leaved
    property onTextRead: TReadCallbackFunction read FOnTextRead write FOnTextRead; //**< is called when text is read
    property onVariableRead: TVariableCallbackFunction read FOnVariableRead write FOnVariableRead; //**< is called whenever a variable is read
  end;

type

{ TLogClass }

TTemplateHTMLParserLogClass=class
  text: string;
  parser: THtmlTemplateParser;
  function printTemplate:string;
  function currentElements:string;
  function conv(t:pchar;tl:longint):string;
  procedure et(read: pchar; readLen:longint);
  procedure lt(read: pchar; readLen:longint);
  procedure tr(read: pchar; readLen:longint);
  procedure vr(variable: string; value: string);
end;

procedure checkHTMLTemplate(templateFileName, inputFile: string);

implementation






{ THtmlTemplateParser }

function THtmlTemplateParser.readTemplateElement(status:TParsingStatus): boolean;
begin
  with status do begin
    if lastElement=nil then lastElement:=nextElement;
    while lastElement<>nextElement do begin
      lastElement:=lastElement.next;
      //if lastElement=nil then exit;
      if (lastElement.typ in TEMPLATE_COMMANDS)  then
        executeTemplateCommand(status,lastElement,true);
    end;
                           ;
  //  FlastText:='';//Letzten Text löschen

    result:=true;
    lastElement:=nextElement;
    nextElement:=nextElement.next;
    if nextElement=nil then begin
      finishHtmlParsing(status);
      exit(false);
    end;
    while nextElement.typ in TEMPLATE_COMMANDS do begin
      executeTemplateCommand(status,nextElement,false);
      nextElement:=nextElement.next;
      if nextElement=nil then begin
        finishHtmlParsing(status);
        exit(false);
      end;
    end;
    //debug history:
    if (latestElement=nil) or ((nextElement<>nil) and (nextElement.offset>latestElement.offset)) then
      latestElement:=nextElement;
    if (nextElement<>nil) and (nextElement.typ = tetHTML) and not nextElement.closeTag then
      lastopenedElement:=nextElement;
  end;
end;


procedure THtmlTemplateParser.executeTemplateCommand(status:TParsingStatus;cmd: TTemplateElement;afterReading:boolean);

  procedure executeReadCommand;
  var text,vari:string;
    regexp: TRegExpr;
  begin
    FCollectDeepNodeText:=false;
    text:=executePseudoXPath(replaceVars(cmd.attributes.Values['source']));

    if cmd.attributes.Values['regex']<>'' then begin
      regexp:=TRegExpr.Create;
      regexp.Expression:=cmd.attributes.Values['regex'];
      regexp.Exec(text);
      text:=regexp.Match[StrToIntDef(cmd.attributes.Values['submatch'],0)];
      regexp.free;
    end;

    //Zeichensatz konvertierung
    //(ohne Annahme template-ZS=html-ZS, es müsste bereits früher konvertiert werden)
    if htmlEncoding<>outputEncoding then
      text:=strChangeEncoding(text, htmlEncoding, outputEncoding);

    vari:=replaceVars(cmd.attributes.Values['var']);
    variables.Values[vari]:=text;
    if Assigned(FOnVariableRead) then FOnVariableRead(vari,text);
  end;

var condition:string;
    comparisonPos:longint;
    equal:boolean;
    ls,rs:string;
begin
  //afterReading: the nextElement has been read
  if afterReading then
    case cmd.typ of
      tetCommandRead:
        if FCollectDeepNodeText then
          executeReadCommand;
  end else begin
    //nextElement has not been read, but due to virtual empty texts the template
    //element named in the file can be be read
    case cmd.typ of
      tetCommandRead:
        if pos('deepNodeText',cmd.attributes.Values['source'])>0 then begin
          FCollectDeepNodeText:=true; //execute later, when more has been read
          fdeepNodeText:=FlastText;
         end else
          executeReadCommand;
      tetCommandIf: if not cmd.closeTag then begin
        condition:=cmd.attributes.Values['test']; //TODO:'==' von == unterscheiden
{        comparisonPos:=pos('==',condition);
        equal:=true;;
        if comparisonPos=0 then begin
          equal:=false;
          comparisonPos:=pos('!=',condition);
          if comparisonPos=0 then
            raise Exception.Create('Vergleichsoperation '+condition+' kann nicht ausgewertet werden');
        end;
        
        ls:=parsePseudoXPath(replaceVars(copy(condition,1,comparisonPos-1)));
        rs:=parsePseudoXPath(replaceVars(copy(condition,comparisonPos+2,length(condition))));
        if ls<>'' then rs:=trim(rs);
        if rs<>'' then ls:=trim(ls);

        equal:=(CompareText(ls,rs)=0) = equal;}
        
        equal:=executePseudoXPath(replaceVars(condition))='true';
        
        if not equal then begin
          status.nextElement:=cmd.reverse;
          status.lastElement:=cmd.reverse;
          //debug history:
          if (status.latestElement=nil) or ((status.nextElement<>nil) and
            (status.nextElement.offset>status.latestElement.offset)) then
            status.latestElement:=status.nextElement;
        end;
      end;
      tetCommandLoop: begin
        if cmd.closeTag then begin
          FParsingAlternatives.add(TParsingStatus.Create);
          with TParsingStatus(FParsingAlternatives[FParsingAlternatives.count-1]) do begin
            lastElement:=status.nextElement;
            nextElement:=status.nextElement.rnext;
            elementStack:=TStringList.Create;
            elementStack.Assign(status.elementStack);
            //debug history:
            latestElement:=status.latestElement;
            lastopenedElement:=status.lastopenedElement;
            if (latestElement=nil) or ((nextElement<>nil) and
              (nextElement.offset>latestElement.offset)) then
              latestElement:=nextElement;
          end;
          //TODO: Dies funktioniert nicht, wenn cmd von einem Befehl gefolgt wird
          status.lastElement:=cmd.reverse;
          status.nextElement:=cmd.reverse;
        end else begin
          //In diesem Fall kann die Schleife auch kein mal ausgeführt werden
          FParsingAlternatives.add(TParsingStatus.Create);
          with TParsingStatus(FParsingAlternatives[FParsingAlternatives.count-1]) do begin
            lastElement:=cmd.reverse;
            nextElement:=cmd.reverse.rnext;
            elementStack:=TStringList.Create;
            elementStack.Assign(status.elementStack);
            //debug history:
            latestElement:=status.latestElement;
            lastopenedElement:=status.lastopenedElement;
            if (latestElement=nil) or ((nextElement<>nil) and (nextElement.offset>latestElement.offset)) then
              latestElement:=nextElement;
          end;
        end;
      end;
    end;
  end;

end;

function THtmlTemplateParser.enterTag(tagName: pchar; tagNameLen: longint;
  properties: THTMLProperties): boolean;

  //check if the current tag is matched by element
  function perfectFit(element:TTemplateElement):boolean;
  var i,j,found,ok:longint;
      Name:string;
      tempProperties:THTMLProperties;
  begin
    if element=nil then result:=false;
    if not strliequal(tagName,element.text,tagNameLen) then
      exit(false);
    if element.attributes=nil then
      exit(true);
    for i := 0 to element.attributes.Count-1 do begin
      Name:=element.attributes.Names[i];
      if strlibeginswith(name,'htmlparser') then continue;
      found:=-1;
      for j:=0 to high(properties) do
        if strliequal(properties[j].name,name,properties[j].nameLen) then begin
          if strliequal(properties[j].value,element.attributes.ValueFromIndex[i],properties[j].valueLen) then
            found:=i
           else found:=-2;
          break;
        end;
      if (element.attributes.ValueFromIndex[i]='') and (found=-1) then
        continue; //a not existing property is interpreted as property="" TODO: test case
      if found<0 then exit(false);
    end;
    //check for additional xpath conditions
    name:=element.attributes.Values['htmlparser-condition'];
    if name<>'' then begin
      tempProperties:=FOldProperties;
      FOldProperties:=properties;
      result:=executePseudoXPath(??name)='true';
      FOldProperties:=tempProperties;
      exit;
    end;
    exit(true);
  end;
  

var i,j:longint;
    element: TTemplateElement;
    jumpAbout: boolean;
    currentParsingStatus: TParsingStatus;
begin
  if FParsingAlternatives.Count=0 then exit(false);

  if not LastEventIsText then textEvent('',0);

  with TParsingStatus(FParsingAlternatives[0]) do
    if FAutoCloseTag and (elementStack.Count>0) then
      leaveTag(@elementStack[elementStack.count-1][1],length(elementStack[elementStack.count-1]));

  if not LastEventIsText then textEvent('',0);

  if assigned(onEnterTag) then onEnterTag(tagName,tagNameLen);

  LastEventIsText:=false;
  if strliequal(tagName,'meta',tagNameLen) then
    if CompareText(getProperty('http-equiv',properties),'content-type')=0 then begin
      if pos('charset=utf-8',LowerCase(getProperty('content',properties)))>0 then
        htmlEncoding:=eUTF8
      else if (pos('charset=windows-1252',LowerCase(getProperty('content',properties)))>0) or
              (pos('charset=iso-8859-1',LowerCase(getProperty('content',properties)))>0) then
        htmlEncoding:=eWindows1252; //ist für iso-8859-1 zwar falsch, aber nur für Kontrollzeichen

      //Html und Template-Datei müssen den gleichen Zeichensatz verwenden
      if htmlEncoding<>templateEncoding then begin
        //ändere Templatecodierung
        element:=FRootTemplate;
        while element<>nil do begin
          if element.typ = tetText then begin
            //gibt nur UTF8 und W1252
            element.text:=strChangeEncoding(element.text,templateEncoding,htmlEncoding);
          end else begin //HTML-Namen sind ASCII und deshalb in UTF-8 und W1252 gleich
            if element.attributes<>nil then
              element.attributes.text:=strChangeEncoding(element.attributes.text,templateEncoding, htmlEncoding);
          end;
          element:=element.next;
        end;
        templateEncoding:=htmlEncoding;
      end;
    end;

  result:=true;
  for i:=0 to FParsingAlternatives.Count-1 do begin
    currentParsingStatus:=TParsingStatus(FParsingAlternatives[i]);
    with currentParsingStatus do begin
      if elementIsOptional(nextElement) and not perfectFit(nextElement) then begin
        //search a fitting element while jumping about every optional one
        element:=nextElement;
        while (element<>nil) and  elementIsOptional(element)  do begin
          if element.reverse<>nil then begin
            if perfectFit(element.reverse.rnext) then begin
              lastElement:=nextElement.reverse;
              nextElement:=nextElement.reverse.rnext;
            end;
            element:=element.reverse.rnext;
          end else begin
            if perfectFit(element.rnext) then begin
              lastElement:=element;
              nextElement:=element.rnext;
              break;
            end;
            element:=element.rnext;
          end;
        end;
      end;
      if perfectFit(nextElement) then begin
        elementStack.AddObject(strFromPchar(tagName,tagNameLen),nextElement);
        result:=readTemplateElement(TParsingStatus(FParsingAlternatives[i]));
        //if (lastElement<>nil) and  ((lastElement.next<>nextElement) or nextElementthen
        FOldProperties:=properties;

        //Ein passender Tag wird so interpretiert, das alles was nun folgt
        //korrekt ist, und somit alle späteren Alternativen nicht mehr
        //berücksichtigt werden müssen.
        //Die kann fehlerhafte Ergebnisse geben, wenn diese Annahme falsch ist.
        //Eigentlich müsste Backtracking benutzt werden, die HTML-Datei soll
        //jedoch nur einmal gelesen werden.
        for j:=i+1 to FParsingAlternatives.Count-1 do
          TParsingStatus(FParsingAlternatives[j]).free;
        FParsingAlternatives.Count:=i+1;
        break;
      end else
        elementStack.AddObject(strFromPchar(tagName,tagNameLen),nil);
    end;
  end;
end;

function THtmlTemplateParser.leaveTag(tagName: pchar; tagNameLen: longint
  ): boolean;

var alt:longint;
    i,j:longint;
    closed: longint; //Elementid im Stack, das geschlossen wird
begin
  if FParsingAlternatives.Count=0 then exit;

  if not LastEventIsText then textEvent('',0);

  with TParsingStatus(FParsingAlternatives[0]) do
    if FAutoCloseTag and (elementStack.Count>0) and not strliequal(tagName,elementStack[elementStack.count-1],tagNameLen) then
      leaveTag(@elementStack[elementStack.count-1][1],length(elementStack[elementStack.count-1]));

  if not LastEventIsText then textEvent('',0);

  if assigned(onLeaveTag) then onLeaveTag(tagName,tagNameLen);

  FAutoCloseTag:=false;
  LastEventIsText:=false;
  Result:=true;
  for alt:=0 to FParsingAlternatives.Count-1 do begin
    with TParsingStatus(FParsingAlternatives[alt]) do begin
      //geschlossenes Element im Stack suchen
      closed:=-1;
     for i:=elementStack.Count-1 downto 0 do
        if strliequal(tagName,elementStack[i],tagNameLen) then begin
          closed:=i;
          break;
        end;
      //ist closed <> letztes Element wird ein Knoten verlassen, der nicht der aktive ist
      //das Dokument ist also ungültig.
      //Allerdings soll dieser Parser fehler korrigierend sein und sucht deshalb
      //einen älteren Knoten
      //ist closed=-1 dann ignorieren
      //TODO: Ähnlichkeit zu aktuellem Knoten suchen, um auf Tippfehler reagieren
      if closed=-1 then
        continue; //HTML-Datei ist ungültig

      //überprüfen, ob das Element im Template
      if elementStack.Objects[closed]<>nil then begin
        while (nextElement<>nil) and elementIsOptional(nextElement) and not nextElement.closeTag do begin
          if (not nextElement.closeTag)and(nextElement.reverse<>nil) then begin
            lastElement:=nextElement.reverse;
            nextElement:=nextElement.reverse;
          end;
          readTemplateElement(TParsingStatus(FParsingAlternatives[alt]));
        end;
        if (nextElement.reverse<>elementStack.Objects[closed]) or (not nextElement.closeTag) then begin
          //ungültiges Element gwschlossen
          nextElement:=TTemplateElement(elementStack.Objects[closed]);
          lastElement:=nextElement; //Verhindert erneutes ausführen der Befehle vor nextElement
                                    //TODO: Überlegen, ob sie nicht doch ausgeführt werden sollen
        end else begin
          //erwartetes Element geschlossen => Alle anderen Alternativen löschen
          for i:=FParsingAlternatives.Count-1 downto 0 do
            if i<>alt then begin
              TParsingStatus(FParsingAlternatives[i]).Free;
              FParsingAlternatives.Delete(i);
            end;
          //geschlossenes Element löschen
          elementStack.Delete(closed);
          if not readTemplateElement(TParsingStatus(FParsingAlternatives[0])) then begin
            TParsingStatus(FParsingAlternatives[0]).free;
            FParsingAlternatives.delete(0);
            //continue;
          end;
          break; //keine anderen Alternativen betrachten
        end;
      end;

      //if strliequal(tagName,tagNameLen,FNextTemplateElement.text) then
      elementStack.Delete(closed);
    end;
  end;
  if FParsingAlternatives.count=0 then exit(false);
end;


function THtmlTemplateParser.textEvent(text: pchar; textLen: longint): boolean;
var alt,i: longint;
  temp:string;
begin
  if assigned(FOnTextRead) then FOnTextRead(text,textLen);
  result:=true;
  LastEventIsText:=true;
{  for i:=0 to textLen-1 do
    if not ((text+i)^ in WHITE_SPACE) then begin
      FlastText:=trim(pcharToString(text+i,textlen-i));
      break;
    end;                                    }
  FlastText:=strDecodeHTMLEntities(text,textlen,htmlEncoding);

  if FCollectDeepNodeText then
    fdeepNodeText+=FlastText
   else begin
     temp:=Trim(FLasttext);
     if temp<>'' then
       FlastText:=temp;

      for alt:=0 to FParsingAlternatives.count-1 do
        with TParsingStatus(FParsingAlternatives[alt]) do begin
          //if (lastElement<>nil) (*and (nextElement.{<>lastElement.next})*) then
          //Text speichern

          while nextElement.typ=tetText do begin
            if strlibeginswith(FlastText,nextElement.text) then
              result:=readTemplateElement(TParsingStatus(FParsingAlternatives[alt]))
             else break;
          end;
       end;
  end;
end;

function THtmlTemplateParser.getTemplateElementDebugInfo(
  element: TTemplateElement): string;
begin
  result:=element.toStr;
  if element.offset =-1 then exit(result+' in unknown line');
  if element.offset>length(FCurrentTemplate) then
    exit(result+' in unknown line at offset '+IntToStr(element.offset));
  result+=' in line '+IntToStr(1+strlcount(#13, @FCurrentTemplate[1], element.offset));
  result+=' at offset '+IntToStr(element.offset);
end;

function THtmlTemplateParser.newTemplateElement(text: pchar; textLen: longint):TTemplateElement;
begin
  if FRootTemplate=nil then begin
    FRootTemplate:=TTemplateElement.create;
    FCurrentTemplateElement:=FRootTemplate;
  end else begin
    FCurrentTemplateElement.next:=TTemplateElement.Create;
    FCurrentTemplateElement:=FCurrentTemplateElement.next;
  end;
  FCurrentTemplateElement.text:=strFromPchar(text,textLen);
  FTemplateCount+=1;
  if textlen=0 then FCurrentTemplateElement.offset:=-1
  else FCurrentTemplateElement.offset:=longint(text)-longint(@FCurrentTemplate[1]);
  //FCurrentTemplateElement.id:=FTemplateCount;
  Result:=FCurrentTemplateElement;
  
end;

procedure THtmlTemplateParser.rememberNewHTMLElement(el: TTemplateElement);
begin
  if FLastHTMLTemplateElement<>nil then
    while FLastHTMLTemplateElement<>FCurrentTemplateElement do begin
      FLastHTMLTemplateElement.rnext:=FCurrentTemplateElement;
      FLastHTMLTemplateElement:=FLastHTMLTemplateElement.next;
    end;
  FLastHTMLTemplateElement:=FCurrentTemplateElement;
end;


function THtmlTemplateParser.templateEnterTag(tagName: pchar;
  tagNameLen: longint; properties: THTMLProperties): boolean;
var nte: TTemplateElement; //New Template ELement
    i:longint;
begin
  if strliequal(tagName,'htmlparser:meta',tagNameLen) then begin
    if (lowercase(getProperty('encoding',properties))='utf-8') or
       (lowercase(getProperty('encoding',properties))='utf8') then templateEncoding:=eUTF8
    else templateEncoding:=eWindows1252;
    exit(true);
  end;

  Result:=true;
  nte:=newTemplateElement(tagName,tagNameLen);
  nte.typ:=strToCommand(tagName,tagNameLen);
  if nte.typ in TEMPLATE_COMMANDS then begin
    if COMMAND_CLOSED[nte.typ] then
      FTemplateElementStack.add(nte);
  end else begin
    FCurrentTemplateElement.typ:=tetHTML;
    rememberNewHTMLElement(FCurrentTemplateElement);
    FTemplateElementStack.add(FCurrentTemplateElement);
  end;
end;

function THtmlTemplateParser.templateLeaveTag(tagName: pchar; tagNameLen: longint):boolean;
var nte: TTemplateElement; //New Template ELement
    command: TTemplateElementType;
begin
  if strliequal(tagName,'htmlparser:meta',tagNameLen) then exit(true);
  Result:=true;
  command:=strToCommand(tagName,tagNameLen);
  if command in TEMPLATE_COMMANDS then
    if not COMMAND_CLOSED[command] then
      exit;

  if (FTemplateElementStack.Count = 0) then
    raise Exception.Create('Nicht geöffneter Tag '+strFromPchar(tagName,tagNameLen)+' wurde im Template geschlossen.');

  if not strliequal(tagname,TTemplateElement(FTemplateElementStack[FTemplateElementStack.Count-1]).text,tagNameLen) then
    raise Exception.Create('Der Tag '+strFromPchar(tagName,tagNameLen)+' wurde im Template geschlossen, obwohl "'+TTemplateElement(FTemplateElementStack[FTemplateElementStack.Count-1]).text+'" dran wäre.');

  nte:=newTemplateElement(tagName,tagNameLen);
  nte.reverse:=TTemplateElement(FTemplateElementStack[FTemplateElementStack.Count-1]);
  nte.reverse.reverse:=nte;
  nte.typ:=command;
  if not (command in TEMPLATE_COMMANDS) then
    rememberNewHTMLElement(nte);
  nte.closeTag:=true;
  FTemplateElementStack.Count:=FTemplateElementStack.Count-1;
end;

function THtmlTemplateParser.templateTextEvent(text: pchar; textLen: longint
  ): boolean;
var nte: TTemplateElement; //New Template ELement
    i:longint;
    ok:boolean;
    textend:pchar;
begin
  Result:=true;
  while (textLen>0) and (text^ in [#9,#10,#13,' ']) do begin
    text+=1;
    textLen-=1;
  end;
  textend:=text+textLen-1;
  while (textLen>0) and (textend^ in [#9,#10,#13,' ']) do begin
    textend-=1;
    textLen-=1;
  end;
  if textLen=0 then exit;
  nte:=newTemplateElement(text,textLen);
  nte.typ:=tetText;
  rememberNewHTMLElement(nte);
end;

procedure THtmlTemplateParser.finishHtmlParsing(status:TParsingStatus);
begin
  FParsingCompleted:=true;
end;

constructor THtmlTemplateParser.create;
begin
  FParsingAlternatives:=TFPList.Create;
  fvariables:=TStringList.Create;
  FNotifyFunctions:=TStringList.Create;
  FVariableFunctions:=TStringList.Create;
  FTemplateElementStack:=TList.Create;
  outputEncoding:=eUTF8;
end;

destructor THtmlTemplateParser.destroy;
begin
  FParsingAlternatives.free;
  FTemplateElementStack.free;
  FVariables.free;
  FNotifyFunctions.free;
  FVariableFunctions.free;
  if FRootTemplate<>nil then FRootTemplate.freeAll;
  inherited destroy;
end;

procedure THtmlTemplateParser.parseHTML(html: string);
var i:longint;
begin
  assert(FRootTemplate<>nil,'Kein Template geladen');
  for i:=0 to FParsingAlternatives.Count-1 do
    TParsingStatus(FParsingAlternatives[i]).free();
  FParsingAlternatives.Clear;
  FParsingAlternatives.add(TParsingStatus.Create);
  with TParsingStatus(FParsingAlternatives[0]) do begin
    lastElement:=nil;
    nextElement:=FRootTemplate;
    latestElement:=FRootTemplate;
    lastopenedElement:=FRootTemplate;
    elementStack:=TStringList.Create;
  end;
  FParsingCompleted:=false;
  htmlEncoding:=outputEncoding;
  FAutoCloseTag:=false;
  try
    simplehtmlparser.parseHTML(html,@enterTag,@leaveTag,@textEvent);
    if not FParsingCompleted then
       raise EHTMLParseException.create('Die HTML Datei ist kürzer als das Template "'+FCurrentTemplateName+'"'#13#10+
                                        'last: '+getTemplateElementDebugInfo(TParsingStatus(FParsingAlternatives[0]).lastElement)+#13#10+
                                        'next: '+getTemplateElementDebugInfo(TParsingStatus(FParsingAlternatives[0]).nextElement)+#13#10+
                                        'latest: '+getTemplateElementDebugInfo(TParsingStatus(FParsingAlternatives[0]).latestElement)+#13#10+
                                        'lastopened: '+getTemplateElementDebugInfo(TParsingStatus(FParsingAlternatives[0]).lastopenedElement)+#13#10+
                                        'Zahl der Alternativen: '+IntToStr(FParsingAlternatives.Count));
  finally
    for i:=0 to FParsingAlternatives.count-1 do
      TParsingStatus(FParsingAlternatives[i]).free;
    FParsingAlternatives.Clear;
  end;
end;

procedure THtmlTemplateParser.parseTemplate(template: string; templateName: string='<unknown>');
begin
  //FVariables.clear;
  if template='' then
    raise ETemplateParseException.Create('Kein Template gefunden');
  FTemplateCount:=0;
  if FRootTemplate<>nil then FRootTemplate.freeAll;
  FRootTemplate:=nil;
  FTemplateElementStack.Clear;
  FLastHTMLTemplateElement:=nil;

  FCurrentTemplate:=template;
  FCurrentTemplateName:=templateName;
  simplehtmlparser.parseHTML(template,@templateEnterTag,@templateLeaveTag,@templateTextEvent);
  if FRootTemplate = nil then
    raise ETemplateParseException.Create('Ungültiges/Leeres Template');
end;

{procedure THtmlTemplateParser.addFunction(name: string;varCallFunc: TVariableCallbackFunction);
begin
  FVariableFunctions.AddObject(name,tobject(@varCallFunc));
end;

procedure THtmlTemplateParser.addFunction(name: string;notifyCallFunc: TNotifyCallbackFunction);
begin
  FNotifyFunctions.AddObject(name,tobject(@notifyCallFunc));
end;}



{ TTemplateElement }

function TTemplateElement.toStr: string;
begin
  if self=nil then exit('nil');
  if closeTag then  result:='</'+text+'> '
  else if typ=tetText then result:=text
  else result:='<'+text+'>';
  if attributes=nil then result+=':nil'
  else result+=attributes.Text;
end;

procedure TTemplateElement.freeAll;
begin
  if next<>nil then next.freeAll;
  next:=nil;
  free;
end;

destructor TTemplateElement.destroy;
begin
  if attributes<>nil then attributes.free;
  inherited;
end;





{ TTemplateHTMLParserLogClass }

function TTemplateHTMLParserLogClass.printTemplate:string;
var el:TTemplateElement;
deep:longint;
begin
  assert(parser.FRootTemplate<>nil,'Kein Template geladen');
  result:='';
  el:=parser.FRootTemplate;
  deep:=0;
  while el<>nil do begin
    if ((el.typ=tetHTML) or ((el.typ in TEMPLATE_COMMANDS) and (el.text='htmlparser:loop'))) and  el.closeTag then
      dec(deep,2);
    result:=result+StringOfChar(' ',deep);
    if el.typ<>tetText then result+='<';
    if el.closeTag then result:=result+'/';
    result:=result+el.text;
    if (el.text='htmlparser:read') or
       (el.text='htmlparser:notify') then result+='/';
    if el.typ<>tetText then result+='>';
    result:=result+'<!--'+inttostr(el.offset)+'-->'#13#10;
    if ((el.typ=tetHTML) or ((el.typ in TEMPLATE_COMMANDS) and (el.text='htmlparser:loop'))) and not el.closeTag then inc(deep,2);


    el:=el.next;;
  end;
end;

function TTemplateHTMLParserLogClass.currentElements: string;
var i,j:longint;
begin
  Result:='';
  for i:=0 to parser.FParsingAlternatives.Count-1 do
    if TParsingStatus(parser.FParsingAlternatives[i]).nextElement<>nil then begin
      if TParsingStatus(parser.FParsingAlternatives[i]).nextElement.closeTag then
        result+='/';
      result:=result+TParsingStatus(parser.FParsingAlternatives[i]).nextElement.text+','+
              IntTostr(TParsingStatus(parser.FParsingAlternatives[i]).nextElement.offset)+'; '
    end else
      Result:=Result+'nil;';
//  result+='   |'+parser.FlastText;
end;

function TTemplateHTMLParserLogClass.conv(t: pchar; tl: longint): string;
begin
  result:=StringReplace(StringReplace(strDecodeHTMLEntities(t,tl,eWindows1252),#13,' ',[rfReplaceAll]),#10,' ',[rfReplaceAll]);
end;

procedure TTemplateHTMLParserLogClass.et(read: pchar; readLen: longint);
begin
  text+='enter tag '+conv(read,readlen)+': '+currentElements+#13#10;
end;

procedure TTemplateHTMLParserLogClass.lt(read: pchar; readLen: longint);
begin
  text+='leave tag /'+conv(read,readlen)+': '+currentElements+#13#10;
end;

procedure TTemplateHTMLParserLogClass.tr(read: pchar; readLen: longint);
begin
  text+='read text '+conv(read,readlen)+': '+currentElements+#13#10;
end;

procedure TTemplateHTMLParserLogClass.vr(variable: string; value: string);
begin
  text+='!! read '+variable+ ' "'+value+'" '#13#10;
end;

{ TParsingStatus }

destructor TParsingStatus.destroy;
begin
  FreeAndNil(elementStack);
  inherited;
end;


procedure checkHTMLTemplate(templateFileName, inputFile: string);
var parser:THtmlTemplateParser;
    log: TTemplateHTMLParserLogClass;
begin
  parser:=THtmlTemplateParser.create;
  log:=TTemplateHTMLParserLogClass.Create;
  log.parser:=parser;
  parser.onEnterTag:=@log.et;
  parser.onLeaveTag:=@log.lt;
  parser.onTextRead:=@log.tr;
  parser.onVariableRead:=@log.vr;
  parser.parseTemplateFile(templateFileName);
  try
    parser.parseHTMLFile(inputFile);
  except
    on e:exception do
      log.text:='fehler: '+e.MESSAGe+#13#10#13#10+log.text;
  end;
  strSaveToFile(inputFile+'.out',log.text );
  log.free;
  parser.free;
end;

{$IFDEF UNITTESTS}
{$IFNDEF DEBUG}{$WARNING unittests without debug}{$ENDIF}

procedure unitTest(extParser: THtmlTemplateParser;testID:longint;logClass: TTemplateHTMLParserLogClass);
var sl: TStringList;
i:longint;
temp:string;

begin
  case testID of
    1: begin //Verschiedene Lesetests
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/></b></a>');
      extParser.parseHTML('<a><b>Dies wird Variable test</b></a>');
      if extParser.variables.Values['test']<>'Dies wird Variable test' then
        raise Exception.create('ungültiges Ergebnis: '+extParser.variables.Values['test']);
    end;
    2: begin
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/></b></a>');
      extParser.parseHTML('<a><b>Dies wird erneut Variable test</b><b>Nicht Test</b><b>Test</b></a>');
      if extParser.variables.Values['test']<>'Dies wird erneut Variable test' then
        raise Exception.create('ungültiges Ergebnis');
    end;
    3: begin
      extParser.parseTemplate('<a><b>Test:</b><b><htmlparser:read source="text()" var="test"/></b></a>');
      extParser.parseHTML('<a><b>Nicht Test</b><b>Test:</b><b>Dies wird erneut Variable test2</b></a>');
      if extParser.variables.Values['test']<>'Dies wird erneut Variable test2' then
        raise Exception.create('ungültiges Ergebnis');
    end;
    4: begin
      extParser.parseTemplate('<a><b>Test:</b><b><htmlparser:read source="text()" var="test"/></b></a>');
      extParser.parseHTML('<a><b>1</b><b>Test:</b><b>2</b><b>3</b></a>');
      if extParser.variables.Values['test']<>'2' then
        raise Exception.create('ungültiges Ergebnis');
    end;
    5: begin
      extParser.parseTemplate('<a><b><htmlparser:read source="@att" var="att-test"/></b></a>');
      extParser.parseHTML('<a><b att="HAllo Welt!"></b></a>');
      if extParser.variables.Values['att-test']<>'HAllo Welt!' then
        raise Exception.create('ungültiges Ergebnis');
    end;
    6: begin
      extParser.parseTemplate('<a><b><htmlparser:read source="@att" var="regex" regex="<\d*>"/></b></a>');
      extParser.parseHTML('<a><b att="Zahlencode: <675> abc"></b></a>');
      if extParser.variables.Values['regex']<>'<675>' then
        raise Exception.create('ungültiges Ergebnis');
    end;
    7: begin
      extParser.parseTemplate('<a><b><htmlparser:read source="@att" var="regex" regex="<(\d* \d*)>" submatch="1"/></b></a>');
      extParser.parseHTML('<a><b att="Zahlencode: <123 543> abc"></b></a>');
      if extParser.variables.Values['regex']<>'123 543' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser 5');
    end;
    8: begin
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/></b></a>');
      extParser.parseHTML('<a><b>1</b><b>2</b><b>3</b><b>4</b><b>5</b></a>');
      if extParser.variables.Values['test']<>'1' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser 6');
    end;
    9: begin //Lesen mit Teiltext
      extParser.parseTemplate('<a><b>Nur diese: <htmlparser:read source="text()" var="test" regex="\d+"/></b></a>');
      extParser.parseHTML('<a><b>1</b><b>2</b><b>Nur diese: 3</b><b>4</b><b>5</b></a>');
      if extParser.variables.Values['test']<>'3' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    10: begin
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test" regex="\d+"/>Nur diese: </b></a>');
      extParser.parseHTML('<a><b>1</b><b>Nur diese: 2</b><b>3</b><b>4</b><b>5</b></a>');
      if extParser.variables.Values['test']<>'2' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    11: begin
      extParser.parseTemplate('<b>Hier<htmlparser:read source="@v" var="test"/></b>');
      extParser.parseHTML('<a><b v="abc">1</b><b v="def"></b>      <b>2</b><b>3</b><b v="ok">Hier</b><b v="!">5</b></a>');
      if extParser.variables.Values['test']<>'ok' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    12: begin
      extParser.parseTemplate('<b><htmlparser:read source="@v" var="test"/>Hier</b>');
      extParser.parseHTML('<a><b v="abc">1</b><b v="def"></b><b>2</b><b>3</b><b v="ok">Hier</b><b v="!">5</b></a>');
      if extParser.variables.Values['test']<>'ok' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    13: begin //Kein Lesen
      extParser.parseTemplate('<a><b><htmlparser:read var="test" source=" ''Saga der sieben Sonnen''"/></b></a>');
      extParser.parseHTML('<a><b>456</b></a>');
      if extParser.variables.Values['test']<>'Saga der sieben Sonnen' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    14: begin //Lesen mit concat 2 Parameter
      extParser.parseTemplate('<a><b><htmlparser:read var="test" source=" concat( ''123'', text() )"/></b></a>');
      extParser.parseHTML('<a><b>456</b></a>');
      if extParser.variables.Values['test']<>'123456' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    15: begin //Lesen mit concat 3 Parameter
      extParser.parseTemplate('<a><b><htmlparser:read var="test" source=" concat( ''abc'', text() , ''ghi'' )"/></b></a>');
      extParser.parseHTML('<a><b>def</b></a>');
      if extParser.variables.Values['test']<>'abcdefghi' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
   { 16: begin //Nicht geschlossene HTML-Tags
      extParser.parseTemplate('<a><p><htmlparser:read var="test" source="text()"/></p></a>');
      extParser.parseHTML('<a><p>Offener Paragraph</a>');
      if extParser.variables.Values['test']<>'Offener Paragraph' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;}
    16: begin
      extParser.parseTemplate('<a><img> <htmlparser:read var="test" source="@src"/> </img></a>');
      extParser.parseHTML('<a><img src="abc.jpg"></a>');
      if extParser.variables.Values['test']<>'abc.jpg' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    17: begin //mehrere davon
      extParser.parseTemplate('<a><img width="100"> <htmlparser:read var="test" source="@src"/> </img></a>');
      extParser.parseHTML('<a><img width=120 src="abc.jpg"><img width=320 src="def.jpg"><img width=100 src="123.jpg"><img width=500 src="baum.jpg"></a>');
      if extParser.variables.Values['test']<>'123.jpg' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    18: begin //IF-Test (Bed. == erfüllt)
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test="''$test;''==''abc''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>');
      extParser.parseHTML('<a><b>abc</b><c>dies kommt raus</c></a>');
      if extParser.variables.Values['test']<>'dies kommt raus' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    19: begin //IF-Test (Bed. == nicht erfüllt)
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test="''$test;''==''abc''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>');
      extParser.parseHTML('<a><b>abcd</b><c>dies kommt nicht raus</c></a>');
      if extParser.variables.Values['test']<>'abcd' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    20: begin //IF-Test (Bed. != erfüllt)
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test="''$test;''!=''abc''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>');
      extParser.parseHTML('<a><b>abcd</b><c>dies kommt raus</c></a>');
      if extParser.variables.Values['test']<>'dies kommt raus' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    21: begin //IF-Test (Bed. != nicht erfüllt)
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test="''abc''!=''$test;''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>');
      extParser.parseHTML('<a><b>abc</b><c>dies kommt nicht raus</c></a>');
      if extParser.variables.Values['test']<>'abc' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    22: begin //Text + If
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></b></a>');
      extParser.parseHTML('<a><b>nicht ok<c>dies kommt nicht raus</c></b></a>');
      if extParser.variables.Values['test']<>'nicht ok' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    23: begin //Text + If
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></b></a>');
      extParser.parseHTML('<a><b>ok<c>dies kommt raus!</c></b></a>');
      if extParser.variables.Values['test']<>'dies kommt raus!' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    24: begin //Text + If + ungeschlossen
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''"><img><htmlparser:read source="@src" var="test"/></img></htmlparser:if></b></a>');
      extParser.parseHTML('<a><b>ok<img src="abc.png"></b></a>');
      if extParser.variables.Values['test']<>'abc.png' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    25: begin //Text + If + ungeschlossen + Text
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''"><img><htmlparser:read source="@src" var="test"/></img><htmlparser:read source="text()" var="ende"/></htmlparser:if></b></a>');
      extParser.parseHTML('<a><b>ok<img src="abcd.png"></b></a>');
      if extParser.variables.Values['test']<>'abcd.png' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser');
    end;
    26: begin //Schleifen Vollständigkeits test
      extParser.parseTemplate('<a><htmlparser:loop><b><htmlparser:read source="text()" var="test"/></b></htmlparser:loop></a>');
      extParser.parseHTML('<a><b>1</b><b>2</b><b>3</b><b>4</b><b>5</b></a>');
      if extParser.variables.Values['test']<>'5' then
        raise Exception.create(extParser.variables.Values['test']+'<>5');
    end;
    27: begin //Leerschleifentest
      extParser.parseTemplate('<a><x><htmlparser:read source="text()" var="test"/></x><htmlparser:loop><b><htmlparser:read source="text()" var="test"/></b></htmlparser:loop></a>');
      extParser.parseHTML('<a><x>abc</x></a>');
      if extParser.variables.Values['test']<>'abc' then
        raise Exception.create('Fehler bei Unit Test extendedhtmlparser 7');
    end;
    28: begin
      extParser.parseTemplate('<a><ax><b>1</b></ax><ax><b><htmlparser:read source="text()" var="test"/></b></ax></a>');
      extParser.parseHTML('<a><ax>123124</ax><ax><b>525324</b></ax><ax><b>1</b></ax><ax><b>3</b></ax></a>');
      if extParser.variables.Values['test']<>'3' then
        raise Exception.create('ergebnis ungültig');
    end;
    29: begin //optionale elemente
      extParser.parseTemplate('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c></a>');
      extParser.parseHTML('<a><xx></xx><c>!!!</c></a>');
      if extParser.variables.Values['test']<>'!!!' then
        raise Exception.create('ergebnis ungültig');
    end;
    30: begin
      extParser.parseTemplate('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c></a>');
      extParser.parseHTML('<a><c>???</c></a>');
      if extParser.variables.Values['test']<>'???' then
        raise Exception.create('ergebnis ungültig');
    end;
    31: begin
      extParser.parseTemplate('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c></a>');
      extParser.parseHTML('<a><b>1</b><c>2</c></a>');
      if extParser.variables.Values['test']<>'2' then
        raise Exception.create('ergebnis ungültig');
    end;
    32: begin
      extParser.parseTemplate('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b></a>');
      extParser.parseHTML('<a><b>1</b><c>2</c><b>3</b></a>');
      if extParser.variables.Values['test']<>'3' then
        raise Exception.create('ergebnis ungültig');
    end;
    33: begin
      extParser.parseTemplate('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c><b htmlparser-optional="true">'+'<htmlparser:read source="text()" var="test"/></b><c htmlparser-optional="true"/><d htmlparser-optional="true"/><e htmlparser-optional="true"/></a>');
      extParser.parseHTML('<a><b>1</b><c>2</c><b>test*test</b></a>');
      if extParser.variables.Values['test']<>'test*test' then
        raise Exception.create('ergebnis ungültig');
    end;
    34: begin
      extParser.parseTemplate('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c><b htmlparser-optional="true">'+'<htmlparser:read source="text()" var="test"/></b><c htmlparser-optional="true"/><d htmlparser-optional="true"/><htmlparser:read source="text()" var="bla"/><e htmlparser-optional="true"/></a>');
      extParser.parseHTML('<a><b>1</b><c>2</c><b>hallo</b>welt</a>');
      if (extParser.variables.Values['test']<>'hallo') then
        raise Exception.create('ergebnis ungültig');
    end;
    35: begin //verzögertes optionale element
      extParser.parseTemplate('<a><x><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b></x></a>');
      extParser.parseHTML('<a><x>Hallo!<a></a><c></c><b>piquadrat</b>welt</x></a>');
      if (extParser.variables.Values['test']<>'piquadrat') then
        raise Exception.create('ergebnis ungültig');
    end;
    40: begin //mehrfach loops+concat
      extParser.parseTemplate('<a><s><htmlparser:read source="text()" var="test"/></s><htmlparser:loop><b><htmlparser:read source="concat(''$test;'',text())" var="test"/></b></htmlparser:loop></a>');
      extParser.parseHTML('<a><s>los:</s><b>1</b><b>2</b><b>3</b></a>');
      if extParser.variables.Values['test']<>'los:123' then
        raise Exception.create('ergebnis ungültig');
    end;
    41: begin
      extParser.parseTemplate('<a><s><htmlparser:read source="text()" var="test"/></s><htmlparser:loop><c><htmlparser:loop><b><htmlparser:read source="concat(''$test;'',text())" var="test"/></b></htmlparser:loop></c></htmlparser:loop></a>');
      extParser.parseHTML('<a><s>los:</s><c><b>a</b><b>b</b><b>c</b></c><c><b>1</b><b>2</b><b>3</b></c><c><b>A</b><b>B</b><b>C</b></c></a>');
      if extParser.variables.Values['test']<>'los:abc123ABC' then
        raise Exception.create('ergebnis ungültig');
    end;
    42: begin //deepNodeText()
      extParser.parseTemplate('<a><x><htmlparser:read source="deepNodeText()" var="test"/></x></a>');
      extParser.parseHTML('<a><x>Test:<b>in b</b><c>in c</c>!</x></a>');
      if extParser.variables.Values['test']<>'Test:in bin c!' then
        raise Exception.create('ergebnis ungültig');
    end;
    43: begin //deepNodeText() mit optionalen
      extParser.parseTemplate('<a><x><htmlparser:read source="text()" var="test1"/><br htmlparser-optional="true"/><htmlparser:read source="deepNodeText()" var="test2"/></x></a>');
      extParser.parseHTML('<a><x>Test:<br><b>in b</b><c>in c</c>!</x></a>');
      if (extParser.variables.Values['test1']<>'Test:') or
         (extParser.variables.Values['test2']<>'in bin c!') then
        raise Exception.create('ergebnis ungültig');
    end;                                                        {
    44: begin
      extParser.variables.Values['test2']:='not called at all';
      extParser.parseTemplate('<a><x><htmlparser:read source="text()" var="test1"/><br htmlparser-optional="true"/><htmlparser:read source="deepNodeText()" var="test2"/></x></a>');
      extParser.parseHTML('<a><x>Test:<b>in b</b><c>in c</c>!</x></a>');
      if (extParser.variables.Values['test1']<>'Test:') or
         (extParser.variables.Values['test2']<>'not called at all')   then
        raise Exception.create('ergebnis ungültig:'+extParser.variables.Values['test1']+'|'+extParser.variables.Values['test2']);
    end;                                                       }
    45: begin //html script tags containing <
      extParser.parseTemplate('<a><script></script><b><htmlparser:read source="text()" var="test"/></b></a>');
      extParser.parseHTML('<a><script>abc<def</script><b>test<b></a>');
      if extParser.variables.Values['test']<>'test' then
        raise Exception.create('ergebnis ungültig');
    end;
    46: begin //direct closed tags
      extParser.parseTemplate('<a><br/><br/><htmlparser:read source="text()" var="test"/><br/></a>');
      extParser.parseHTML('<a><br/><br   />abc<br /></a>');
      if extParser.variables.Values['test']<>'abc' then
        raise Exception.create('ergebnis ungültig');
    end;
    47: begin //xpath conditions
      extParser.parseTemplate('<html><a htmlparser-condition="filter(@cond, ''a+'') == ''aaa'' "><htmlparser:read source="text()" var="test"/></a></html>');
      extParser.parseHTML('<html><a>a1</a><a cond="xyz">a2</a><a cond="a">a3</a><a cond="xaay">a4</a><a cond="aaaa">a5</a><a cond="xaaay">a6</a><a cond="xaaaay">a7</a><a cond="xaay">a8</a></html>');
      if extParser.variables.Values['test']<>'a6' then
        raise Exception.create('ergebnis ungültig');
    end;

    80: begin //encoding detection
      extParser.parseTemplate('<a><htmlparser:read source="text()" var="test"/></a>');
      //no coding change utf-8 -> utf-8
      extParser.outputEncoding:=eUTF8;
      extParser.parseHTML('<meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><a>uu(bin:'#$C3#$84',ent:&Ouml;)uu</a></html>');
      if extParser.variables.Values['test']<>'uu(bin:'#$C3#$84',ent:'#$C3#$96')uu' then //ÄÖ
        raise Exception.create('ergebnis ungültig utf8->utf8');
      //no coding change latin1 -> latin1
      extParser.outputEncoding:=eWindows1252;
      extParser.parseHTML('<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" /><a>ll(bin:'#$C4',ent:&Ouml;)ll</a></html>');
      if extParser.variables.Values['test']<>'ll(bin:'#$C4',ent:'#$D6')ll' then
        raise Exception.create('ergebnis ungültig latin1->latin1');
      //coding change latin1 -> utf-8
      extParser.outputEncoding:=eUTF8;
      extParser.parseHTML('<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" /><a>lu(bin:'#$C4',ent:&Ouml;)lu</a></html>');
      temp:=extParser.variables.Values['test'];
      if extParser.variables.Values['test']<>'lu(bin:'#$C3#$84',ent:'#$C3#$96')lu' then
        raise Exception.create('ergebnis ungültig latin1->utf8');
      //coding change utf8 -> latin1
      extParser.outputEncoding:=eWindows1252;
      extParser.parseHTML('<meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><a>ul(bin:'#$C3#$84',ent:&Ouml;)ul</a></html>');
      if extParser.variables.Values['test']<>'ul(bin:'#$C4',ent:'#$D6')ul' then
        raise Exception.create('ergebnis ungültig utf8->latin1');

      extParser.parseHTML('<meta http-equiv="Content-Type" content="text/html; charset=" /><a>bin:'#$C4#$D6',ent:&Ouml;</a></html>');
      extParser.outputEncoding:=eUTF8;
    end;
    99: if FileExists('U:\components\pascal\html\tests\test8.template') then begin
      //Sollte was Bücherintrotest sein
      sl:=TStringList.Create;
      sl.LoadFromFile('U:\components\pascal\html\tests\test8.template');
      extParser.parseTemplate(sl.text);
      sl.LoadFromFile('U:\components\pascal\html\tests\test8.html');
      extParser.parseHTML(sl.text);
      sl.LoadFromFile('U:\components\pascal\html\tests\test8.log');
{      for i:=1 to length(sl.text) do
        if sl.text[i]<>logclass.text[i] then
          raise Exception.Create(IntToStr(i)+': '+sl.text[i]+'<>'+logclass.text[i]); }
      if sl.text<>logClass.text then
        raise Exception.Create('logs sind unterschiedlich');
      sl.free;
    end ;//else raise Exception.Create('Input für Test nicht vorhanden');

    100  : if FileExists('T:\test.template') then begin //Freier Test
      sl:=TStringList.Create;
      sl.LoadFromFile('T:\test.template');
      extParser.parseTemplate(sl.text);
      //logClass.printTemplate;
      sl.LoadFromFile('T:\test.html');
      extParser.parseHTML(sl.text);
      if FileExists('T:\test.log') then begin
        sl.LoadFromFile('T:\test.log');
        if sl.text<>logClass.text then
          raise Exception.Create('logs sind unterschiedlich');
      end;
      sl.free;
    end;
  end;
end;

procedure unitTests();

var i:longint;
    extParser:THtmlTemplateParser;
    log:TTemplateHTMLParserLogClass;
    sl:TStringList;
begin
  extParser:=THtmlTemplateParser.create;

  //simulate a html tag
  extParser.FlastText:='test:last text';
  extParser.fdeepNodeText:='test:deep node text';
  setlength(extParser.FOldProperties,3);
  extParser.FOldProperties[0].name:='attrib1';extParser.FOldProperties[0].nameLen:=7;
  extParser.FOldProperties[0].value:='FIRST ATTRIBUTE';extParser.FOldProperties[0].valueLen:=5+10;
  extParser.FOldProperties[1].name:='attrib2';extParser.FOldProperties[1].nameLen:=7;
  extParser.FOldProperties[1].value:='SECOND ATTRIBUTE';extParser.FOldProperties[1].valueLen:=6+10;
  extParser.FOldProperties[2].name:='attrib3';extParser.FOldProperties[2].nameLen:=7;
  extParser.FOldProperties[2].value:='THIRD ATTRIBUTE';extParser.FOldProperties[2].valueLen:=5+10;
  extParser.variables.add('testvar_t=true');
  extParser.variables.add('testvar_f=false');


  sl:=TStringList.Create;
  log:=TTemplateHTMLParserLogClass.Create;
  log.parser:=extParser;
  extParser.onEnterTag:=@log.et;
  extParser.onLeaveTag:=@log.lt;
  extParser.onTextRead:=@log.tr;
  extParser.onVariableRead:=@log.vr;
  try
    for i:=1 to 100 do begin
      try
        log.text:='';
        unitTest(extParser,i      ,log);
        if log.text<>'' then sl.text:=log.text;
      except on e:exception do begin
        sl.Text:='Unit-Test '+inttostr(i)+' fehlgeschlagen: '#13#10+e.message+#13#10+log.text;
        sl.SaveToFile('t:\extendedhtmlparser.log');
        sl.Text:=log.printTemplate;
        sl.SaveToFile('t:\extendedhtmlparser.log.template');
        raise Exception.Create('Unit-Test '+inttostr(i)+' fehlgeschlagen: '#13#10+e.message+#13#10+log.text);
        end;
      end;
    end;
    sl.SaveToFile('t:\extendedhtmlparser.last.log');
  except
  end;
  log.free;  extParser.free; sl.free;
end;

initialization
unitTests();

{$ENDIF}


end.

