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

//{$IFDEF DEBUG}
//{$DEFINE UNITTESTS}
//{$ENDIF}

interface
uses
  Classes, SysUtils,simplehtmltreeparser,pseudoxpath,
    dRegExpr, //this should contain TRegExpr from  Andrey V. Sorokin (regexpstudio.com -- page dead, I create a mirror on benibela.de) (his file is named regexpr, but you should rename is to differentiate it from fpc regexpr)
    bbutils;


type
//duplicate open/close because this simplifies the switch statements
TTemplateElementType=(tetIgnore,
                      tetHTMLOpen, tetHTMLClose, tetHTMLText,
                      tetCommandMeta, tetCommandRead,
                      tetCommandLoopOpen,tetCommandLoopClose,
                      tetCommandIfOpen, tetCommandIfClose);

TNotifyCallbackFunction = procedure () of object;
TVariableCallbackFunction = procedure (variable: string; value: string) of object;
TReadCallbackFunction = procedure (read: pchar; readLen:longint) of object;

TReplaceFunction = procedure (variable: string; var value:string) of object;

ETemplateParseException = Exception;

{ TTemplateElement }

TTemplateElement=class(TTreeElement)
  templateType: TTemplateElementType;
  function isOptional: boolean;
  procedure setOptional(opt: boolean);
  procedure initialized;override;
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
    outputEncoding: TEncoding;

    FTemplate, FHTML: TTreeParser;
    FTemplateName: string;

    FPseudoXPath: TPseudoXPathParser;

    Fvariables, FVariableLog: TStringList;
  protected
    FCurrentTemplateName: string; //currently loaded template, only needed for debugging (a little memory waste)
    //FCurrentStack: TStringList;
    //FOnVariableRead: TVariableCallbackFunction;

    //function readTemplateElement(status:TParsingStatus):boolean; //gibt false nach dem letzten zurück
    function executePseudoXPath(str: string; replaceVariables: boolean=true):string;
    //procedure executeTemplateCommand(status:TParsingStatus;cmd: TTemplateElement;afterReading:boolean);
    //function getTemplateElementDebugInfo(element: TTemplateElement): string;

    function templateElementFitHTMLOpen(html:TTreeElement; template: TTemplateElement): Boolean;
    function matchTemplateTree(htmlParent, htmlStart, htmlEnd:TTreeElement; templateStart, templateEnd: TTemplateElement): boolean;
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
    property variableChangeLog: TStringList read FVariableLog;
  end;

implementation

const //TEMPLATE_COMMANDS=[tetCommandMeta..tetCommandIfClose];
      COMMAND_CLOSED:array[tetCommandMeta..tetCommandIfClose] of longint=(0,0,1,2,1,2);
      COMMAND_STR:array[tetCommandMeta..tetCommandIfClose] of string=('meta','read','loop','loop','if','if');

{ TTemplateElement }

function strToCommand(s:string; treeTyp: TTreeElementType): TTemplateElementType;
var tag:pchar; taglen: integer;
  t: TTemplateElementType;
begin
  if ((treeTyp = tetOpen) or (treeTyp = tetClose)) and (strlibeginswith(s,'htmlparser:')) then begin
    tag:=@s[length('htmlparser:')+1];
    taglen:=length(s)-length('htmlparser:');
    for t:=low(COMMAND_STR) to high(COMMAND_STR) do
      if strliequal(tag,COMMAND_STR[t],taglen) then begin
        if treeTyp = tetOpen then exit(t)
        else if COMMAND_CLOSED[t] = 0 then exit(tetIgnore)
        else if COMMAND_CLOSED[t] = 2 then exit(t);
      end;
    raise ETemplateParseException.Create('Unbekannter Templatebefehl: '+s)
  end;
  case treeTyp of
    tetOpen: exit(tetHTMLOpen);
    tetClose: exit(tetHTMLClose);
    tetText: exit(tetHTMLText);
  end;
end;

function TTemplateElement.isOptional: boolean;
begin
  if (typ=tetText) and (value='') then exit(true);
  if (attributes=nil) then exit(false);
  result:=(attributes.Values['htmlparser-optional']='true');
end;

procedure TTemplateElement.setOptional(opt: boolean);
begin
  assert(attributes<>nil);
  if opt then attributes.Values['htmlparser-optional'] := 'true'
  else attributes.Values['htmlparser-optional'] := 'false';
end;

procedure TTemplateElement.initialized;
begin
  //inherited initialized;
  templateType:=strToCommand(value, typ);
end;

function THtmlTemplateParser.executePseudoXPath(str: string; replaceVariables: boolean): string;
begin
  if replaceVariables then str := replaceVars(str);
  FPseudoXPath.parse(str);
  result:=FPseudoXPath.evaluate();
end;

function THtmlTemplateParser.templateElementFitHTMLOpen(html: TTreeElement;
  template: TTemplateElement): Boolean;
var
  name: string;
  condition: string;
  i: Integer;
begin
  if (html.typ <> tetOpen) or (template.templateType <> tetHTMLOpen) or
     (html.value <> template.value) then
       exit(false);
  if template.attributes = nil then
    exit(true);
  for i:=0 to template.attributes.Count-1 do begin
    name := template.attributes.Names[i];
    if strlibeginswith(name, 'htmlparser') then continue;
    if html.attributes = nil then exit(false);
    if html.attributes.Values[name] <> template.attributes.ValueFromIndex[i] then
      exit(false);
  end;
  condition := template.attributes.Values['htmlparser-condition'];
  if condition = '' then
    exit(true);
  FPseudoXPath.ParentElement := html;
  FPseudoXPath.TextElement := nil;
  exit(executePseudoXPath(condition)='true');
end;

function THtmlTemplateParser.matchTemplateTree(htmlParent, htmlStart, htmlEnd: TTreeElement;
  templateStart, templateEnd: TTemplateElement): Boolean;

var xpathText: TTreeElement;

  procedure HandleHTMLText;
  begin
    //if we find a text match we can assume it is a true match
    if strlibeginswith(htmlStart.value, templateStart.value) then templateStart := TTemplateElement(templateStart.next);
    htmlStart := htmlStart.next;
  end;


  procedure HandleHTMLOpen;
  var ok: boolean;
  begin
    //If an element is option it can either be there (preferred) or not. Therefore we simple try both cases
    //Notice that this modifies the template, and it is NOT THREAD SAFE (so don't share
    //one instance, you can of course still use instances in different threads)
    if templateStart.isOptional then begin
      templateStart.setOptional(false);
      ok := matchTemplateTree(htmlParent, htmlStart, htmlEnd, templateStart, templateEnd);
      templateStart.setOptional(true);
      if ok then templateStart := templateEnd
      else templateStart := TTemplateElement(templateStart.reverse.next);
      exit;
    end;

    //To check if a node matches a template node we have to check all children, if they don't match
    //we have to test it with another node
    //But once a element E match we can assume that there is no better match on the same level (e.g. a
    //match F with E.parent = F.parent), because this is simple list matching
    if (not templateElementFitHTMLOpen(htmlStart, templateStart)) or
       (not matchTemplateTree(htmlStart, htmlStart.next, htmlStart.reverse, TTemplateElement(templateStart.next), TTemplateElement(templateStart.reverse))) then htmlStart:=htmlStart.next
    else begin
      htmlStart := htmlStart.reverse.next;
      templateStart := TTemplateElement(templateStart.reverse.next);
    end;
  end;

  procedure HandleCommandRead;
  var text,vari:string;
    regexp: TRegExpr;
  begin
    FPseudoXPath.ParentElement := htmlParent;
    FPseudoXPath.TextElement := xpathText;
    text:=executePseudoXPath(replaceVars(templateStart.attributes.Values['source']));

    if templateStart.attributes.Values['regex']<>'' then begin
      regexp:=TRegExpr.Create;
      regexp.Expression:=templateStart.attributes.Values['regex'];
      regexp.Exec(text);
      text:=regexp.Match[StrToIntDef(templateStart.attributes.Values['submatch'],0)];
      regexp.free;
    end;

    vari:=replaceVars(templateStart.attributes.Values['var']);

    Fvariables.Values[vari] := text;
    FVariableLog.Add(vari+'='+text);

    templateStart := TTemplateElement(templateStart.next);
  end;

  procedure HandleCommandIf;
  var
    condition: string;
    equal: Boolean;
  begin
    condition:=templateStart.attributes.Values['test'];

    FPseudoXPath.ParentElement := htmlParent;
    FPseudoXPath.TextElement := xpathText;
    equal:=executePseudoXPath(replaceVars(condition))='true';

    if not equal then
      templateStart := TTemplateElement(templateStart.reverse) //skip if block
     else
      templateStart := TTemplateElement(templateStart.next); //continue
  end;

  procedure HandleCommandLoopOpen;
  begin
    //Two possible cases:
    //1. Continue in loop (preferred of course)
    //2. Jump over loop
    //if matchTemplateTree(htmlParent, htmlStart, htmlEnd, TTemplateElement(templateStart.next), templateEnd) then templateStart := templateEnd
    if matchTemplateTree(htmlParent, htmlStart, htmlEnd, TTemplateElement(templateStart.next), templateEnd) then templateStart := templateEnd
    else templateStart := TTemplateElement(templateStart.reverse.next);
  end;

var realHtmlStart: TTreeElement;
  procedure HandleCommandLoopClose;
  begin
    //Jump to loop start if a html element was read in the loop
    //The condition is necessary, because if the loop can be executed without
    //reading a html element, it can be executed again, and again, and ... =>
    //endless loop
    if realHtmlStart <> htmlStart then
      templateStart := TTemplateElement(templateStart.reverse) //jump to loop start
     else
      templateStart := TTemplateElement(templateStart.next);
  end;

var logLength: longint;
  vari: string;
begin
  if htmlStart = nil then exit(false);
  if templateStart = nil then exit(false);
  realHtmlStart := htmlStart;
 // assert(templateStart <> templateEnd);
  logLength:=FVariableLog.Count;
  xpathText := nil;
  while (htmlStart <> nil) and
        (templateStart <> nil) and (templateStart <> templateEnd) and
        ((htmlStart <> htmlEnd.next)) do begin
            if htmlStart.typ = tetText then xpathText := htmlStart;
            case templateStart.templateType of
              tetHTMLText: HandleHTMLText;
              tetHTMLOpen: HandleHTMLOpen;
              tetHTMLClose:  raise ETemplateParseException.Create('Assertion fail: Closing template tag </'+templateStart.value+'> not matched');

              tetCommandRead: HandleCommandRead;

              tetCommandIfOpen: HandleCommandIf;

              tetCommandLoopOpen: HandleCommandLoopOpen;
              tetCommandLoopClose: HandleCommandLoopClose;

              tetIgnore, tetCommandMeta, tetCommandIfClose: templateStart := TTemplateElement(templateStart.next);

              else raise ETemplateParseException.Create('Unknown template element type - internal error');
            end
        end;

  result := templateStart = templateEnd;
  if not result then
    while (FVariableLog.Count>logLength) do begin
      vari := FVariableLog.Names[FVariableLog.Count-1];
      FVariableLog.Delete(FVariableLog.Count-1);
      Fvariables.Values[vari] := FVariableLog.Values[vari];
    end;
end;

constructor THtmlTemplateParser.create;
begin
  Fvariables := TStringList.Create;
  FVariableLog := TStringList.Create;
  FTemplate := TTreeParser.Create;
  FTemplate.parsingModel:=pmStrict;
  FTemplate.treeElementClass:=TTemplateElement;
  FHTML := TTreeParser.Create;
  FHTML.parsingModel:=pmHTML;
  FPseudoXPath := TPseudoXPathParser.Create;
  outputEncoding:=eUTF8;
end;

destructor THtmlTemplateParser.destroy;
begin
  Fvariables.Free;
  FVariableLog.Free;
  FTemplate.Free;
  FHTML.Free;
  FPseudoXPath.free;
  inherited destroy;
end;

procedure THtmlTemplateParser.parseHTML(html: string);
begin
  FHTML.parseTree(html);

  //encoding trouble
  FHTML.setEncoding(outputEncoding);
  FTemplate.setEncoding(outputEncoding);

  FVariableLog.Clear;
  Fvariables.Clear;
  matchTemplateTree(FHTML.getTree, FHTML.getTree.next, FHTML.getTree.reverse, TTemplateElement(FTemplate.getTree.next), TTemplateElement(FTemplate.getTree.reverse));
end;

procedure THtmlTemplateParser.parseHTMLFile(htmlfilename: string);
begin
  parseHTML(strLoadFromFile(htmlfilename));
end;

procedure THtmlTemplateParser.parseTemplate(template: string;
  templateName: string);
var el: TTemplateElement;
begin
  FTemplate.setEncoding(eUnknown, false, false);
  if strbeginswith(template,#$ef#$bb#$bf) then begin
    delete(template,1,3);
    FTemplate.setEncoding(eUTF8,false,false);
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
            FTemplate.setEncoding(eUTF8, false, false)
          else
            FTemplate.setEncoding(eWindows1252, false, false);
      end;
    end;
    el := TTemplateElement(el.next);
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



{$IFDEF UNITTESTS}

{$IFNDEF DEBUG}{$WARNING unittests without debug}{$ENDIF}

procedure unitTests();
var data: array[1..58]of array[1..3] of string = (
//---classic tests---
 //simple reading
 ('<a><b><htmlparser:read source="text()" var="test"/></b></a>',
 '<a><b>Dies wird Variable test</b></a>',
 'test=Dies wird Variable test'),
 ('<a><b><htmlparser:read source="text()" var="test"/></b></a>',
 '<a><b>Dies wird erneut Variable test</b><b>Nicht Test</b><b>Test</b></a>',
 'test=Dies wird erneut Variable test'),
 ('<a><b>Test:</b><b><htmlparser:read source="text()" var="test"/></b></a>',
 '<a><b>Nicht Test</b><b>Test:</b><b>Dies wird erneut Variable test2</b></a>',
 'test=Dies wird erneut Variable test2'),
 ('<a><b>Test:</b><b><htmlparser:read source="text()" var="test"/></b></a>',
 '<a><b>1</b><b>Test:</b><b>2</b><b>3</b></a>',
 'test=2'),
 ('<a><b><htmlparser:read source="@att" var="att-test"/></b></a>',
 '<a><b att="HAllo Welt!"></b></a>',
 'att-test=HAllo Welt!'),
 ('<a><b><htmlparser:read source="@att" var="regex" regex="<\d*>"/></b></a>',
 '<a><b att="Zahlencode: <675> abc"></b></a>',
 'regex=<675>'),
 ('<a><b><htmlparser:read source="@att" var="regex" regex="<(\d* \d*)>" submatch="1"/></b></a>',
 '<a><b att="Zahlencode: <123 543> abc"></b></a>',
 'regex=123 543'),
 ('<a><b><htmlparser:read source="text()" var="test"/></b></a>',
 '<a><b>1</b><b>2</b><b>3</b><b>4</b><b>5</b></a>',
 'test=1'),
 //reading with matching node text
 ('<a><b>Nur diese: <htmlparser:read source="text()" var="test" regex="\d+"/></b></a>',
 '<a><b>1</b><b>2</b><b>Nur diese: 3</b><b>4</b><b>5</b></a>',
 'test=3'),
 ('<a><b><htmlparser:read source="text()" var="test" regex="\d+"/>Nur diese: </b></a>',
 '<a><b>1</b><b>Nur diese: 2</b><b>3</b><b>4</b><b>5</b></a>',
 'test=2'),
 ('<b>Hier<htmlparser:read source="@v" var="test"/></b>',
 '<a><b v="abc">1</b><b v="def"></b>      <b>2</b><b>3</b><b v="ok">Hier</b><b v="!">5</b></a>',
 'test=ok'),
 //look ahead testing
 ('<b><htmlparser:read source="@v" var="test"/>Hier</b>',
 '<a><b v="abc">1</b><b v="def"></b>      <b>2</b><b>3</b><b v="100101">Hier</b><b v="!">5</b></a>',
 'test=100101'),
 //simple reading
 ('<b><htmlparser:read source="@v" var="test"/>Hier</b>',
 '<a><b v="abc">1</b><b v="def"></b><b>2</b><b>3</b><b v="ok">Hier</b><b v="!">5</b></a>',
 'test=ok'),
 //No reading
 ('<a><b><htmlparser:read var="test" source=" ''Saga der sieben Sonnen''"/></b></a>',
 '<a><b>456</b></a>',
 'test=Saga der sieben Sonnen'),
 //Reading concat 2-params
 ('<a><b><htmlparser:read var="test" source=" concat( ''123'', text() )"/></b></a>',
 '<a><b>456</b></a>',
 'test=123456'),
 //Reading concat 3-params
 ('<a><b><htmlparser:read var="test" source=" concat( ''abc'', text() , ''ghi'' )"/></b></a>',
 '<a><b>def</b></a>',
 'test=abcdefghi'),
 //non closed html tags
 ('<a><p><htmlparser:read var="test" source="text()"/></p></a>',
 '<a><p>Offener Paragraph</a>',
 'test=Offener Paragraph'),
 ('<a><img> <htmlparser:read var="test" source="@src"/> </img></a>',
 '<a><img src="abc.jpg"></a>',
 'test=abc.jpg'),
 //several non closed
 ('<a><img width="100"> <htmlparser:read var="test" source="@src"/> </img></a>',
 '<a><img width=120 src="abc.jpg"><img width=320 src="def.jpg"><img width=100 src="123.jpg"><img width=500 src="baum.jpg"></a>',
 'test=123.jpg'),
 //if tests (== strue)
 ('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test="''$test;''==''abc''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>',
 '<a><b>abc</b><c>dies kommt raus</c></a>',
 'test=abc'#13#10'test=dies kommt raus'),
 //if test (== false),
 ('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test="''$test;''==''abc''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>',
   '<a><b>abcd</b><c>dies kommt nicht raus</c></a>',
   'test=abcd'),
 //IF-Test (!= true)
 ('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test="''$test;''!=''abc''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>',
  '<a><b>abcd</b><c>dies kommt raus</c></a>',
  'test=abcd'#13#10'test=dies kommt raus'),
 //IF-Test (!= false)
  ('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test="''abc''!=''$test;''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>',
  '<a><b>abc</b><c>dies kommt nicht raus</c></a>',
  'test=abc'),
 //Text + If
   ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></b></a>',
   '<a><b>nicht ok<c>dies kommt nicht raus</c></b></a>',
   'test=nicht ok'),
  ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''"><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></b></a>',
   '<a><b>ok<c>dies kommt raus!</c></b></a>',
   'test=ok'#13'test=dies kommt raus!'),
  //text + if + not closed
  ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''"><img><htmlparser:read source="@src" var="test"/></img></htmlparser:if></b></a>',
   '<a><b>ok<img src="abc.png"></b></a>',
   'test=ok'#13'test=abc.png'),
   //text + if + not closed + text
  ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''"><img><htmlparser:read source="@src" var="test"/></img><htmlparser:read source="text()" var="ende"/></htmlparser:if></b></a>',
  '<a><b>ok<img src="abcd.png"></b></a>',
  'test=ok'#13'test=abcd.png'#13'ende=ok'),
  //text + if + not closed + text
 ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test="''ok''==''$test;''">  <img><htmlparser:read source="@src" var="test"/><htmlparser:read source="text()" var="ende"/></img>  </htmlparser:if></b></a>',
 '<a><b>ok<img src="abcd.png"></b></a>',
 'test=ok'#13'test=abcd.png'#13'ende='),
 //loop complete
 ('<a><htmlparser:loop><b><htmlparser:read source="text()" var="test"/></b></htmlparser:loop></a>',
 '<a><b>1</b><b>2</b><b>3</b><b>4</b><b>5</b></a>',
 'test=1'#13'test=2'#13'test=3'#13'test=4'#13'test=5'),
 //loop empty
 ('<a><x><htmlparser:read source="text()" var="test"/></x><htmlparser:loop><b><htmlparser:read source="text()" var="test"/></b></htmlparser:loop></a>',
  '<a><x>abc</x></a>',
  'test=abc'),
  ('<a><ax><b>1</b></ax><ax><b><htmlparser:read source="text()" var="test"/></b></ax></a>',
  '<a><ax>123124</ax><ax><b>525324</b></ax><ax><b>1</b></ax><ax><b>3</b></ax></a>',
  'test=3'),
 //optional elements
  ('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c></a>',
  '<a><xx></xx><c>!!!</c></a>',
  'test=!!!'),
  ('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c></a>',
  '<a><c>???</c></a>',
  'test=???'),
  ('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c></a>',
  '<a><b>1</b><c>2</c></a>',
  'test=1'#13'test=2'),
  ('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b></a>',
   '<a><b>1</b><c>2</c><b>3</b></a>',
   'test=1'#13'test=2'#13'test=3'),
  ('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c><b htmlparser-optional="true">'+'<htmlparser:read source="text()" var="test"/></b><c htmlparser-optional="true"/><d htmlparser-optional="true"/><e htmlparser-optional="true"/></a>',
    '<a><b>1</b><c>2</c><b>test*test</b></a>',
    'test=1'#13'test=2'#13'test=test*test'),
  ('<a><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b><c><htmlparser:read source="text()" var="test"/></c><b htmlparser-optional="true">'+'<htmlparser:read source="text()" var="test"/></b><c htmlparser-optional="true"/><d htmlparser-optional="true"/><htmlparser:read source="text()" var="bla"/><e htmlparser-optional="true"/></a>',
  '<a><b>1</b><c>2</c><b>hallo</b>welt</a>',
  'test=1'#13'test=2'#13'test=hallo'#13'bla=welt'),
 //delayed optional elements
  ('<a><x><b htmlparser-optional="true"><htmlparser:read source="text()" var="test"/></b></x></a>',
   '<a><x>Hallo!<a></a><c></c><b>piquadrat</b>welt</x></a>',
   'test=piquadrat'),
 //multiple loops+concat
  ('<a><s><htmlparser:read source="text()" var="test"/></s><htmlparser:loop><b><htmlparser:read source="concat(''$test;'',text())" var="test"/></b></htmlparser:loop></a>',
   '<a><s>los:</s><b>1</b><b>2</b><b>3</b></a>',
   'test=los:'#13'test=los:1'#13'test=los:12'#13'test=los:123'),
  ('<a><s><htmlparser:read source="text()" var="test"/></s><htmlparser:loop><c><htmlparser:loop><b><htmlparser:read source="concat(''$test;'',text())" var="test"/></b></htmlparser:loop></c></htmlparser:loop></a>',
   '<a><s>los:</s><c><b>a</b><b>b</b><b>c</b></c><c><b>1</b><b>2</b><b>3</b></c><c><b>A</b><b>B</b><b>C</b></c></a>',
   'test=los:'#13'test=los:a'#13'test=los:ab'#13'test=los:abc'#13'test=los:abc1'#13'test=los:abc12'#13'test=los:abc123'#13'test=los:abc123A'#13'test=los:abc123AB'#13'test=los:abc123ABC'),
 //deepNodeText()
  ('<a><x><htmlparser:read source="deepNodeText()" var="test"/></x></a>',
   '<a><x>Test:<b>in b</b><c>in c</c>!</x></a>',
   'test=Test:in bin c!'),
 //deepNodeText with optional element
  ('<a><x><htmlparser:read source="text()" var="test1"/><br htmlparser-optional="true"/><htmlparser:read source="deepNodeText()" var="test2"/></x></a>',
   '<a><x>Test:<br><b>in b</b><c>in c</c>!</x></a>',
   'test1=Test:'#13'test2=Test:in bin c!'),
  ('<a><pre><htmlparser:read source="text()" var="test2"/></pre><x><htmlparser:read source="text()" var="test1"/><br htmlparser-optional="true"/><htmlparser:read source="deepNodeText()" var="test2"/></x></a>',
   '<a><pre>not called at all</pre><x>Test:<b>in b</b><c>in c</c>!</x></a>',
   'test2=not called at all'#13'test1=Test:'#13'test2=Test:in bin c!'),
 //html script tags containing <
   ('<a><script></script><b><htmlparser:read source="text()" var="test"/></b></a>',
   '<a><script>abc<def</script><b>test<b></a>',
   'test=test'),
 //direct closed tags
   ('<a><br/><br/><htmlparser:read source="text()" var="test"/><br/></a>',
   '<a><br/><br   />abc<br /></a>',
   'test=abc'),
 //xpath conditions
   ('<html><a htmlparser-condition="filter(@cond, ''a+'') == ''aaa'' "><htmlparser:read source="text()" var="test"/></a></html>',
   '<html><a>a1</a><a cond="xyz">a2</a><a cond="a">a3</a><a cond="xaay">a4</a><a cond="aaaa">a5</a><a cond="xaaay">a6</a><a cond="xaaaay">a7</a><a cond="xaay">a8</a></html>',
   'test=a6'),


//--new tests--
   //simple read
   ('<table id="right"><tr><td><htmlparser:read source="text()" var="col"/></td></tr></table>',
    '<html><table id="right"><tr><td></td><td>other</td></tr></table></html>',
    'col='),

   //loop corner cases
   ('<htmlparser:loop><tr><td><htmlparser:read source="text()" var="col"/></td></tr></htmlparser:loop>',
    '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
    'col=Hallo'#13'col=123'#13'col=foo'#13'col=bar'#13'col=xyz'),
   ('<table><htmlparser:loop><tr><td><htmlparser:read source="text()" var="col"/></td></tr></htmlparser:loop></table>',
    '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
    'col=Hallo'),
   ('<table></table><htmlparser:loop><tr><td><htmlparser:read source="text()" var="col"/></td></tr></htmlparser:loop>',
    '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
    'col=123'#13'col=foo'#13'col=bar'#13'col=xyz'),
   ('<tr/><htmlparser:loop><tr><td><htmlparser:read source="text()" var="col"/></td></tr></htmlparser:loop>',
    '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
    'col=123'#13'col=foo'#13'col=bar'#13'col=xyz'),
   ('<htmlparser:loop><tr><td><htmlparser:read source="text()" var="col"/></td></tr></htmlparser:loop><tr/>',
    '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
    'col=Hallo'#13'col=123'#13'col=foo'#13'col=bar'),
   ('<table></table><table><htmlparser:loop><tr><td><htmlparser:read source="text()" var="col"/></td></tr></htmlparser:loop></table>',
    '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
     'col=123'#13'col=foo'#13'col=bar'#13'col=xyz'),
   ('<htmlparser:loop><htmlparser:loop><tr><td><htmlparser:read source="text()" var="col"/></td></tr></htmlparser:loop></htmlparser:loop>',
    '<html><body><table id="wrong"><tr><td>Hallo</td></tr></table><table id="right"><tr><td>123</td><td>other</td></tr><tr><td>foo</td><td>columns</td></tr><tr><td>bar</td><td>are</td></tr><tr><td>xyz</td><td>ignored</td></tr></table></html>',
    'col=Hallo'#13'col=123'#13'col=foo'#13'col=bar'#13'col=xyz'),
   ('<table><htmlparser:loop><tr><td><x htmlparser-optional="true"><htmlparser:read source="text()" var="k"/></x><htmlparser:read source="text()" var="col"/></td></tr></htmlparser:loop></table>',
    '<html><body><table id="wrong"><tr><td><x>hallo</x>Hillo</td></tr><tr><td><x>hallo2</x>Hillo2</td></tr><tr><td><x>hallo3</x>Hallo3</td></tr><tr><td>we3</td></tr><tr><td><x>hallo4</x>Hallo4</td></tr></table></html>',
    'k=hallo'#13'col=Hillo'#13'k=hallo2'#13'col=Hillo2'#13'k=hallo3'#13'col=Hallo3'#13'col=we3'#13'k=hallo4'#13'col=Hallo4'),




   //optional elements
   ('<a>as<htmlparser:read source="text()" var="a"/></a><b htmlparser-optional="true"></b>',
    '<a>asx</a><x/>',
    'a=asx'),
   ('<a>as<htmlparser:read source="text()" var="a"/></a><b htmlparser-optional="true"></b>',
    '<a>asx</a>',
    'a=asx'),

   //different text() interpretations
   ('<a><htmlparser:read source="text()" var="A"/><x/><htmlparser:read source="text()" var="B"/></a>',
    '<a>hallo<x></x>a</a>',
    'A=hallo'#13'B=a')

);

var i:longint;
    extParser:THtmlTemplateParser;
    sl:TStringList;
    j: Integer;
begin
  extParser:=THtmlTemplateParser.create;
  sl:=TStringList.Create;
  for i:=low(data)to high(data) do begin
      extParser.parseTemplate(data[i,1]);
      extParser.parseHTML(data[i,2]);
      sl.Text:=data[i,3];
      //check lines to avoid line ending trouble with win/linux
      if extParser.variableChangeLog.Count<>sl.Count then
        raise Exception.Create('Test failed: '+inttostr(i)+' got: "'+extParser.variableChangeLog.Text+'" expected: "'+data[i,3]+'"');
      for j:=0 to sl.count-1 do
        if extParser.variableChangeLog[j]<>sl[j] then
          raise Exception.Create('Test failed: '+inttostr(i)+' got: "'+extParser.variableChangeLog.Text+'" expected: "'+data[i,3]+'"');
  end;

  //---special encoding tests---
  extParser.parseTemplate('<a><htmlparser:read source="text()" var="test"/></a>');
  //no coding change utf-8 -> utf-8
  extParser.outputEncoding:=eUTF8;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><a>uu(bin:'#$C3#$84',ent:&Ouml;)uu</a></html>');
  if extParser.variables.Values['test']<>'uu(bin:'#$C3#$84',ent:'#$C3#$96')uu' then //ÄÖ
    raise Exception.create('ergebnis ungültig utf8->utf8');
  //no coding change latin1 -> latin1
  extParser.outputEncoding:=eWindows1252;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" /><a>ll(bin:'#$C4',ent:&Ouml;)ll</a></html>');
  if extParser.variables.Values['test']<>'ll(bin:'#$C4',ent:'#$D6')ll' then
    raise Exception.create('ergebnis ungültig latin1->latin1');
  //coding change latin1 -> utf-8
  extParser.outputEncoding:=eUTF8;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" /><a>lu(bin:'#$C4',ent:&Ouml;)lu</a></html>');
  if extParser.variables.Values['test']<>'lu(bin:'#$C3#$84',ent:'#$C3#$96')lu' then
    raise Exception.create('ergebnis ungültig latin1->utf8');
  //coding change utf8 -> latin1
  extParser.outputEncoding:=eWindows1252;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><a>ul(bin:'#$C3#$84',ent:&Ouml;)ul</a></html>');
  if extParser.variables.Values['test']<>'ul(bin:'#$C4',ent:'#$D6')ul' then
    raise Exception.create('ergebnis ungültig utf8->latin1');

  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=" /><a>bin:'#$C4#$D6',ent:&Ouml;</a></html>');
  extParser.outputEncoding:=eUTF8;



  extParser.free;
  sl.Free;
end;

initialization
unitTests();

{$ENDIF}


end.



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






end.

