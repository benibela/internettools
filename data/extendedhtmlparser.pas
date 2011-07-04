{**
  @abstract This units contains a template based html parser named THtmlTemplateParser

  @author Benito van der Zander (http://www.benibela.de)
}
unit extendedhtmlparser;
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

{$mode objfpc}{$H+}

//{$IFDEF DEBUG}
{$DEFINE UNITTESTS}
//{$ENDIF}

interface
uses
  Classes, SysUtils,simplehtmltreeparser,pseudoxpath,
    dRegExpr, //this should contain TRegExpr from  Andrey V. Sorokin (regexpstudio.com -- page dead, I create a mirror on benibela.de) (his file is named regexpr, but you should rename is to differentiate it from fpc regexpr)
    bbutils;


type
//**@abstract These are all possible template commands
//duplicate open/close because this simplifies the switch statements
TTemplateElementType=(tetIgnore,
                      tetHTMLOpen, tetHTMLClose, tetHTMLText,
                      tetCommandMeta, tetCommandRead,
                      tetCommandLoopOpen,tetCommandLoopClose,
                      tetCommandIfOpen, tetCommandIfClose,
                      tetCommandSwitchOpen, tetCommandSwitchClose);

(*TNotifyCallbackFunction = procedure () of object;
TVariableCallbackFunction = procedure (variable: string; value: string) of object;
TReadCallbackFunction = procedure (read: pchar; readLen:longint) of object;*)

//**Possible callback for getting the value of a variable
TReplaceFunction = procedure (variable: string; var value:string) of object;

ETemplateParseException = Exception;
EHTMLParseException = Exception;

{ TTemplateElement }
//**@abstract Interally used template tree element @exclude
TTemplateElement=class(TTreeElement)
  templateType: TTemplateElementType;
  match: TTreeElement; //this is only for template debugging issues (it will be nil iff the element was never matched, or the iff condition never satisfied)
  function isOptional: boolean;
  procedure setOptional(opt: boolean);
  procedure initialized;override;
end;

{ THtmlTemplateParser }

{**
  @abstract This is the html parser class
  You can use it simply by calling first parseTemplate to load a given template
  and then parseHTML to parse the html data. @br
  A template file is just like a html file with special commands. The parser than matches every text and tag
  of the template to text/tag in the html file, while ignoring every additional data of latter file. If no match is possible an exception is raised.@br
  The template can extract certain values from the html file into @noAutoLink(variables), and you can access these @noAutoLink(variables) with the property variables and variableChangeLog.
  Former only contains the final value of the @noAutoLink(variables), latter records every assignment during the matching of the template.@br@br

  @bold(Examples)

  @italic(Example, how to read the first <b>-tag):@br
    Template: @code(<b><htmlparser:read var="test" source="text()"></b>)@br
    Html-File: @code(<b>Hello World!</b>))@br

  This will set the variable test to "Hello World!" @br

  @italic(Example, how to read the first field of a every row of a table):@br
    Template: @code(<table> <htmlparser:loop> <tr> <td> <htmlparser:read var="readField()" source="text()"> </td> </tr> </htmlparser:loop> </table>)@br
    Html-File: @code(<table> <tr> <td> row-cell 1 </td> </tr> <tr> <td> row-cell 2 </td> </tr> ... <tr> <td> row-cell n </td> </tr> </table>)@br

    This will read row after row, and will write the first field to the change log of the variable readField() .@br

  @italic(Example, how to read the several field of a every row of a table):@br

    Template: @code(<table> <htmlparser:loop> <tr> <td> <htmlparser:read var="readField1()" source="text()"> </td> <td> <htmlparser:read var="readField2()" source="text()"> </td> <td> <htmlparser:read var="readField3()" source="text()"> </td> ... </tr> </htmlparser:loop> </table>)@br
    Html-File: @code(<table> <tr> <td> a </td> <td> b </td> <td> c </td> </tr> ... </tr> </table>)@br

    This will read readField1()=a, readField2()=b, readField3()=c...@br
    Of you can use your own names instead of readFieldX() and they are independent of the html file. So such templates can convert several pages with different structures, to the same internal data layout of your application.

  @italic(Example, how to read all rows of every table CSV like):@br
  Template: @code(<htmlparser:loop> <tr>  <htmlparser:read var="readAnotherRow()" source="deep-text(',')"> </tr> </htmlparser:loop> )@br
  Html-File: @code(... <tr> <td> a </td> <td> b </td> <td> c </td> </tr> <tr> <td> foo </td> <td> bar </td> </tr> ...)@br

  This will read all rows, and write lines like a,b,c and foo,bar to the changelog.@br

  @italic(Example, how to read the first list item starting with an unary prime number):@br
  Template: @code(<li htmlparser-condition="filter(text(), '1*:') != filter(text(), '^1?:|^(11+?)\1+:')"><htmlparser:read var="prime" source="text()"/></li>)@br
  Html-File: @code(... <li>1111: this is 4</li><li>1:1 is no prime</li><li>1111111: here is 7</li><li>11111111: 8</li> ...)@br

  This will return "1111111: here is 7", because 1111111 is the first prime in that list.@br@br

  @italic(Example, how to extract all elements of a html form):
  @preformatted(<form>
  <htmlparser:loop><htmlparser:switch>
  <input type="checkbox" htmlparser-condition="exists(@checked)"><htmlparser:read var="post" source="concat(@name,'=',@value)"/>  </input>
  <input type="radio" htmlparser-condition="exists(@checked)">   <htmlparser:read var="post" source="concat(@name,'=',@value)"/>  </input>
  <input type="hidden">                                          <htmlparser:read var="post" source="concat(@name,'=',@value)"/>  </input>
  <input type="password">                                        <htmlparser:read var="post" source="concat(@name,'=',@value)"/>  </input>
  <input type="text">                                            <htmlparser:read var="post" source="concat(@name,'=',@value)"/>  </input>
  <select><htmlparser:read var="temp" source="@name"/><option htmlparser-optional="true" htmlparser-condition="exists(@selected)"><htmlparser:read var="post" source="concat($temp;,'=',@value)"/></option></select>
  <textarea>                                                     <htmlparser:read var="post" source="concat(@name,'=',text())"/>  </textarea>
  </htmlparser:switch></htmlparser:loop>
</form>)

  Html-File: any form @br

  This example will extract from each relevant element in the form the name and value pair which is sent to the webserver.
  It is very general, and will work with all forms, independent of things like nesting deep.
  Therefore it is a little bit ugly; but if you create a template for a specific page, you usually know which elements you will find there, so the template becomes much simpler in practical cases.



  See the unit tests at the end of the file extendedhtmlparser.pas for more examples

  @bold(Syntax of a template file)

  Basically the template file is a html file, and the parser tries to match the structure of the template html file to the html file. @br
  A tag of the html file is considered as equal to a tag of the template file, if the tag names are equal, all attributes are the same (regardless of their order) and every child node of the tag in the template is also equal to a child node of the tag in the html file (in the same order and nesting).@br
  Text nodes are considered as equal, if the text in the html file starts with the whitespace trimmed text of the template file. All comparisons are performed case insensitive.@br
  The matching occurs (in the latest version) with backtracking, so it will always find the first and longest match.

  There are 5 special commands allowed:
   @unorderedList(
      @item(@code(<htmlparser:meta encoding="??"/>) @br Specifies the encoding of the template, only windows-1252 and utf-8 allowed)
      @item(@code(<htmlparser:if test="??"/>  .. </htmlparser:if>)
        @br Everything inside this tag is only used if the pseudo-XPath-expression in test equals to true)
      @item(@code(<htmlparser:loop>  .. </htmlparser:loop>)
        @br Everything inside this tag is repeated as long as possible
        @br E.g. if you write @code(<htmlparser:loop>  X </htmlparser:loop> ), it has the same effect as XXXXX with the largest possible count of X for a given html file.
        @br If there is no possible match for the loop interior the loop is completely ignored. (if you want the empty loop to raise an error, you can create a temporary variable in the loop, and check for the existence of the variable after the loop.)
        )
      @item(@code(<htmlparser:read var="??" source="??" [regex="??" [submatch="??"]]/>)
        @br The @link(pseudoxpath.TPseudoXPathParser Pseudo-XPath-expression) in source is evaluated and stored in variable of var.
        @br If a regex is given, only the matching part is saved. If submatch is given, only the submatch-th match of the regex is returned. (e.g. b will be the 2nd match of "(a)(b)(c)") (However, you should use the pxpath-function filter instead of the regex/submatch attributes, because former is more elegant)
        )
      @item(@code(<htmlparser:switch> ... </htmlparser:switch>)
        @br This tag is matched to an html tag, iff one of its direct children can be matched to that html tag.
        @br For example @code(<htmlparser:switch><a>..</a> <b>..</b></htmlparser:switch>) will match either @code(<a>..</a>) or @code(<b>..</b>), but not both. If there is an <a> and a <b> tag in the html file, only the first one will be matched (if there is no loop around the switch tag).
        @br Therefore such a switch tag is obviously not the same as two optional elements (see below) like @code(<a htmlparser-optional="true"/a> <b htmlparser-optional="true"/>), but also not the same as an optional element which excludes the next element like @code(<a htmlparser-optional="true"><htmlparser:read source="'true'" var="temp"/></a> <htmlparser:if test="$temp;!=true"> <b/> </htmlparser:if>).
            The difference is that the switch-construct gives equal priority to every of its children, but the excluding if-construct prioritizes a, and will ignore any b followed by an a.@br
            These switch-constructs are mainly used within a loop to collect the values of different tags.)
    )
    @br
    There are two special attributes allowed for html tags in the template file:
    @unorderedList(
      @item(@code(htmlparser-optional="true") @br if this is set the file is read sucessesfully even if the tag doesn't exist.@br
                                               You should never have an optional element as direct children of a loop, because the loop has lower priority as the optional element, so the parser will skip loop iterations if it can find a later match for the optional element.
                                               But it is fine to use optional tags that have an non-optional parent tag within the loop. )
      @item(@code(htmlparser-condition="pseudo xpath") @br if this is given, a tag is only accepted as matching, iff the given pxpath-expression returns 'true' (powerful, but slow))
    )



    @bold(Important changes from previous versions:)@br
    Old changes:@br
    The interface has changed:  There are no callback events anymore, because they do not make sense with backtracking, where partly matching can be reverted. Instead the property variables returns the resulting value of the @noAutolink(variables) and variableChangeLog contains a complete history of the @noAutolink(variables).@br
    The new parser is more reliable than the old. If it possible to match the template and the html file the new version will find this match. And if it does not find a match, you have a proof that no match exists. But if you used a template which relies on the fact that it is sometimes not matched, although it is possible, it will of course break in the new version.  @br
    The validation of a template has been slightly changed: now every opened tag must be closed, but in contrast you are allowed to mix entities with entity less encoding (cdata is still not supported).@br
    In the old version you could set variables before you call parseHtml, this is still possible, but you have to pass false as second parameter, or the variable states are cleared. The changelog however is always removed..@br
    The default encoding is now utf-8, so the parsed web pages will be converted to utf-8, but it will break if your template is not utf-8 and didn't specifies an encoding with the htmlparser:meta tag. (if you use the meta tag, it will also be converted to utf8)@br

}
THtmlTemplateParser=class
  protected
    FOutputEncoding: TEncoding;

    FTemplate, FHTML: TTreeParser;
    FTemplateName: string;

    FPseudoXPath: TPseudoXPathParser;

    FVariableLog: TPXPVariableChangeLog;
    FParsingExceptions: boolean;
  protected
    FCurrentTemplateName: string; //currently loaded template, only needed for debugging (a little memory waste)
    //FCurrentStack: TStringList;
    //FOnVariableRead: TVariableCallbackFunction;

    //function readTemplateElement(status:TParsingStatus):boolean; //gibt false nach dem letzten zurück
    function executePseudoXPath(str: string):TPXPValue;
    //procedure executeTemplateCommand(status:TParsingStatus;cmd: TTemplateElement;afterReading:boolean);
    //function getTemplateElementDebugInfo(element: TTemplateElement): string;

    function templateElementFitHTMLOpen(html:TTreeElement; template: TTemplateElement): Boolean;
    function matchTemplateTree(htmlParent, htmlStart, htmlEnd:TTreeElement; templateStart, templateEnd: TTemplateElement): boolean;
  public
    constructor create;
    destructor destroy; override;


    function parseHTML(html: string; keepOldVariables: boolean=false):boolean; //**< parses the given data.
    function parseHTMLFile(htmlfilename: string; keepOldVariables: boolean=false):boolean; //**< parses the given file
    procedure parseTemplate(template: string; templateName: string='<unknown>');//**< loads the given template, stores templateName for debugging issues
    procedure parseTemplateFile(templatefilename: string); //**<loads a template from a file
    //procedure addFunction(name:string;varCallFunc: TVariableCallbackFunction);overload;
    //procedure addFunction(name:string;notifyCallFunc: TNotifyCallbackFunction);overload;

    //**This replaces every $variable; in s with variables.values['variable'] or the value returned by customReplace
    function replaceVars(s:string;customReplace: TReplaceFunction=nil):string;

    //TODO: optimize variable storage
    //property variables: TStringList read Fvariables;//**<List of all variables
    property variableChangeLog: TPXPVariableChangeLog read FVariableLog; //**<All assignments to a variables during the matching of the template. You can use TStrings.GetNameValue to get the variable/value in certain line
    property ParsingExceptions: boolean read FParsingExceptions write FParsingExceptions; //**< If this is true (default) it will raise an exception if the matching fails.
    property OutputEncoding: TEncoding read FOutputEncoding write FOutputEncoding;
  end;

implementation

const //TEMPLATE_COMMANDS=[tetCommandMeta..tetCommandIfClose];
      COMMAND_CLOSED:array[tetCommandMeta..tetCommandSwitchClose] of longint=(0,0,1,2,1,2,1,2); //0: no children, 1: open, 2: close
      COMMAND_STR:array[tetCommandMeta..tetCommandSwitchClose] of string=('meta','read','loop','loop','if','if','switch','switch');

{ TTemplateElement }

function strToCommand(s:string; treeTyp: TTreeElementType): TTemplateElementType;
var tag:pchar; taglen: integer;
  t: TTemplateElementType;
begin
  if ((treeTyp = tetOpen) or (treeTyp = tetClose)) and (stribeginswith(s,'htmlparser:')) then begin
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

function THtmlTemplateParser.executePseudoXPath(str: string): TPXPValue;
begin
  //str := replaceVars(str);
  FPseudoXPath.parse(str);
  result:=FPseudoXPath.evaluate();
end;

function THtmlTemplateParser.templateElementFitHTMLOpen(html: TTreeElement;
  template: TTemplateElement): Boolean;
var
  name: string;
  condition: string;
  i,j: Integer;
begin
  if (html.typ <> tetOpen) or (template.templateType <> tetHTMLOpen) or
     not striequal(html.value, template.value) then
       exit(false);
  if template.attributes = nil then
    exit(true);
  for i:=0 to template.attributes.Count-1 do begin
    name := template.attributes.Names[i];
    if stribeginswith(name, 'htmlparser') then continue;
    if html.attributes = nil then exit(false);
    if not striequal(html.attributes.Values[name], template.attributes.ValueFromIndex[i]) then
      exit(false);
  end;
  condition := template.attributes.Values['htmlparser-condition'];
  if condition = '' then
    exit(true);
  FPseudoXPath.ParentElement := html;
  FPseudoXPath.TextElement := nil;
  exit(pxpvalueToBoolean(executePseudoXPath(condition)));
end;

function THtmlTemplateParser.matchTemplateTree(htmlParent, htmlStart, htmlEnd: TTreeElement;
  templateStart, templateEnd: TTemplateElement): Boolean;

var xpathText: TTreeElement;

  procedure HandleHTMLText;
  begin
    //if we find a text match we can assume it is a true match
    if stribeginswith(htmlStart.value, templateStart.value) then begin
      templateStart.match := htmlStart;
      templateStart := TTemplateElement(templateStart.next);
    end;
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
    if (not templateElementFitHTMLOpen(htmlStart, templateStart)) then htmlStart:=htmlStart.next
    else begin
      templateStart.match := htmlStart;
      if (not matchTemplateTree(htmlStart, htmlStart.next, htmlStart.reverse, TTemplateElement(templateStart.next), TTemplateElement(templateStart.reverse))) then htmlStart:=htmlStart.next
      else begin
        htmlStart := htmlStart.reverse.next;
        templateStart := TTemplateElement(templateStart.reverse.next);
      end;
    end;
  end;

  procedure HandleCommandRead;
  var text,vari:string;
    regexp: TRegExpr;
  begin
    FPseudoXPath.ParentElement := htmlParent;
    FPseudoXPath.TextElement := xpathText;
    text:=pxpvalueToString(executePseudoXPath(templateStart.attributes.Values['source']));

    if templateStart.attributes.Values['regex']<>'' then begin
      regexp:=TRegExpr.Create;
      regexp.Expression:=templateStart.attributes.Values['regex'];
      regexp.Exec(text);
      text:=regexp.Match[StrToIntDef(templateStart.attributes.Values['submatch'],0)];
      regexp.free;
    end;

    vari:=replaceVars(templateStart.attributes.Values['var']);

    FVariableLog.addVariable(vari, text);

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
    equal:=pxpvalueToBoolean(executePseudoXPath(condition));

    if not equal then
      templateStart := TTemplateElement(templateStart.reverse) //skip if block
     else begin
      TTemplateElement(templateStart).match := templateStart;
      templateStart := TTemplateElement(templateStart.next); //continue
     end;
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

  procedure HandleCommandSwitch;
  var curChild: TTreeElement;
  begin
    curChild:=templateStart.getFirstChild();
    templateStart.match := htmlStart;
    while curChild <> nil do begin //enumerate all child tags
      if TTemplateElement(curChild).isOptional then raise ETemplateParseException.Create('A direct child of the htmlparser:switch construct may not have the attribute htmlparser-optional (it is optional anyways)');
      if templateElementFitHTMLOpen(htmlStart, TTemplateElement(curChild)) and
          matchTemplateTree(htmlStart, htmlStart.next, htmlStart.reverse, TTemplateElement(curChild.next), TTemplateElement(curChild.reverse)) then begin
        //found match
        htmlStart := htmlStart.reverse.next;
        templateStart := TTemplateElement(templateStart.reverse.next);
        exit;
      end;
      //no match, try other matches
      curChild := curChild.getNextSibling();
    end;

    htmlStart:=htmlStart.next; //no match
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
  FVariableLog.pushAll;
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

              tetCommandSwitchOpen: HandleCommandSwitch;

              tetIgnore, tetCommandMeta, tetCommandIfClose, tetCommandSwitchClose: templateStart := TTemplateElement(templateStart.next);

              else raise ETemplateParseException.Create('Unknown template element type - internal error');
            end
        end;

  result := templateStart = templateEnd;
  if not result then
    FVariableLog.popAll;
end;

constructor THtmlTemplateParser.create;
begin
  FVariableLog := TPXPVariableChangeLog.Create;
  FTemplate := TTreeParser.Create;
  FTemplate.parsingModel:=pmStrict;
  FTemplate.treeElementClass:=TTemplateElement;
  FHTML := TTreeParser.Create;
  FHTML.parsingModel:=pmHTML;
  FHTML.readComments:=true;
  FPseudoXPath := TPseudoXPathParser.Create;
  FPseudoXPath.OnEvaluateVariable:=@FVariableLog.evaluateVariable;
  outputEncoding:=eUTF8;
  FParsingExceptions := true;
end;

destructor THtmlTemplateParser.destroy;
begin
  FVariableLog.Free;
  FTemplate.Free;
  FHTML.Free;
  FPseudoXPath.free;
  inherited destroy;
end;

function THtmlTemplateParser.parseHTML(html: string; keepOldVariables: boolean=false):boolean;
var cur,last,realLast:TTreeElement;
  i: Integer;
begin
  FHTML.parseTree(html);

  //encoding trouble
  FHTML.setEncoding(outputEncoding,true,true);
  FTemplate.setEncoding(outputEncoding, true, true);

  if FParsingExceptions then begin
    cur := FTemplate.getTree;
    while cur <> nil do begin
      TTemplateElement(cur).match := nil;
      cur := cur.next;
    end;
  end;

  if not keepOldVariables then FVariableLog.Clear;

  FPseudoXPath.RootElement := FHTML.getTree;

  result:=matchTemplateTree(FHTML.getTree, FHTML.getTree.next, FHTML.getTree.reverse, TTemplateElement(FTemplate.getTree.next), TTemplateElement(FTemplate.getTree.reverse));

  if not result and FParsingExceptions then begin
    cur := FTemplate.getTree;
    realLast := cur;
    last := cur;
    while cur <> nil do begin
      case TTemplateElement(cur).templateType of
        tetHTMLOpen, tetHTMLText: begin
          if TTemplateElement(cur).match = nil then
            raise EHTMLParseException.create('Matching of template '+FTemplateName+' failed.'#13#10'Couldn''t find a match for: '+cur.toString+#13#10'Previous element is:'+reallast.toString+#13#10'Last match was:'+last.toString+' with '+TTemplateElement(last).match.toString);
          last:=cur;
        end;
        tetCommandIfOpen: begin
          if TTemplateElement(cur).match = nil then cur := cur.reverse;
          last:=cur;
        end;
      end;

      realLast := cur;
      cur := cur.next;
    end;
    raise EHTMLParseException.create('Matching of template '+FTemplateName+' failed. for an unknown reason');
  end;
//TODODO  for i:=1 to variableLogStart do FVariableLog.Delete(0); //remove the old variables from the changelog
end;

function THtmlTemplateParser.parseHTMLFile(htmlfilename: string; keepOldVariables: boolean=false):boolean;
begin
  result:=parseHTML(strLoadFromFile(htmlfilename), keepOldVariables);
end;

procedure THtmlTemplateParser.parseTemplate(template: string;
  templateName: string);
var el: TTemplateElement;
    encoding: string;
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
        encoding:=el.attributes.Values['encoding'];
        if striequal(encoding,'utf8') or striequal(encoding,'utf-8') then
          FTemplate.setEncoding(eUTF8, false, false)
        else if striequal(el.attributes.Values['encoding'],'latin1') or striequal(el.attributes.Values['encoding'],'iso88591') or
                striequal(el.attributes.Values['encoding'],'iso-8859-1') or striequal(el.attributes.Values['encoding'],'windows1252') then
          FTemplate.setEncoding(eWindows1252, false, false)
        else
         raise ETemplateParseException.create('Unknown/unsupported encoding: '+encoding);
        if el.attributes.count > 1 then
          raise ETemplateParseException.create('Additional attributes in meta-tag: '+el.tostring);
      end else
        raise ETemplateParseException.create('Empty/wrong meta-tag: '+el.tostring);

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
      value:=pxpvalueToString(variableChangeLog.getVariableValue(temp));
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
var data: array[1..144] of array[1..3] of string = (
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
 ('<a><b><htmlparser:read source="comment()" var="test"/></b></a>',
 '<a><b><!--cCc--></b><b>2</b><b>3</b><b>4</b><b>5</b></a>',
 'test=cCc'),
 ('<a><b><htmlparser:read'#9'source="text()"'#13'var="test"/></b></a>',
 '<a><b>Dies wird'#9'Variable test</b></a>',
 'test=Dies wird'#9'Variable test'),
 ('<a><b'#13'attrib'#10'='#9'"test"><htmlparser:read'#9'source="text()"'#13'var="test"/></b></a>',
 '<a><b'#9'attrib           =         '#10'  test>Dies'#9'wird'#9'Variable test</b></a>',
 'test=Dies'#9'wird'#9'Variable test'),
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
 ('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test=''"$test;"="abc"''><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>',
 '<a><b>abc</b><c>dies kommt raus</c></a>',
 'test=abc'#13#10'test=dies kommt raus'),
 //if test (== false),
 ('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test=''"$test;"="abc"''><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>',
   '<a><b>abcd</b><c>dies kommt nicht raus</c></a>',
   'test=abcd'),
 //IF-Test (!= true)
 ('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test=''"$test;"!="abc"''><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>',
  '<a><b>abcd</b><c>dies kommt raus</c></a>',
  'test=abcd'#13#10'test=dies kommt raus'),
 //IF-Test (!= false)
  ('<a><b><htmlparser:read source="text()" var="test"/></b><htmlparser:if test=''"abc"!="$test;"''><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></a>',
  '<a><b>abc</b><c>dies kommt nicht raus</c></a>',
  'test=abc'),
 //Text + If
   ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test=''"ok"="$test;"''><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></b></a>',
   '<a><b>nicht ok<c>dies kommt nicht raus</c></b></a>',
   'test=nicht ok'),
  ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test=''"ok"="$test;"''><c><htmlparser:read source="text()" var="test"/></c></htmlparser:if></b></a>',
   '<a><b>ok<c>dies kommt raus!</c></b></a>',
   'test=ok'#13'test=dies kommt raus!'),
  //text + if + not closed
  ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test=''"ok"="$test;"''><img><htmlparser:read source="@src" var="test"/></img></htmlparser:if></b></a>',
   '<a><b>ok<img src="abc.png"></b></a>',
   'test=ok'#13'test=abc.png'),
   //text + if + not closed + text
  ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test=''"ok"="$test;"''><img><htmlparser:read source="@src" var="test"/></img><htmlparser:read source="text()" var="ende"/></htmlparser:if></b></a>',
  '<a><b>ok<img src="abcd.png"></b></a>',
  'test=ok'#13'test=abcd.png'#13'ende=ok'),
  //text + if + not closed + text
 ('<a><b><htmlparser:read source="text()" var="test"/><htmlparser:if test=''"ok"="$test;"''>  <img><htmlparser:read source="@src" var="test"/><htmlparser:read source="text()" var="ende"/></img>  </htmlparser:if></b></a>',
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
  ('<a><s><htmlparser:read source="text()" var="test"/></s><htmlparser:loop><b><htmlparser:read source="concat($test;,text())" var="test"/></b></htmlparser:loop></a>',
   '<a><s>los:</s><b>1</b><b>2</b><b>3</b></a>',
   'test=los:'#13'test=los:1'#13'test=los:12'#13'test=los:123'),
  ('<a><s><htmlparser:read source="text()" var="test"/></s><htmlparser:loop><c><htmlparser:loop><b><htmlparser:read source=''concat("$test;",text())'' var="test"/></b></htmlparser:loop></c></htmlparser:loop></a>',
   '<a><s>los:</s><c><b>a</b><b>b</b><b>c</b></c><c><b>1</b><b>2</b><b>3</b></c><c><b>A</b><b>B</b><b>C</b></c></a>',
   'test=los:'#13'test=los:a'#13'test=los:ab'#13'test=los:abc'#13'test=los:abc1'#13'test=los:abc12'#13'test=los:abc123'#13'test=los:abc123A'#13'test=los:abc123AB'#13'test=los:abc123ABC'),
 //deep-ode-text()
  ('<a><x><htmlparser:read source="deep-text()" var="test"/></x></a>',
   '<a><x>Test:<b>in b</b><c>in c</c>!</x></a>',
   'test=Test:in bin c!'),
 //deepNodeText with optional element
  ('<a><x><htmlparser:read source="text()" var="test1"/><br htmlparser-optional="true"/><htmlparser:read source="deep-text()" var="test2"/></x></a>',
   '<a><x>Test:<br><b>in b</b><c>in c</c>!</x></a>',
   'test1=Test:'#13'test2=Test:in bin c!'),
  ('<a><pre><htmlparser:read source="text()" var="test2"/></pre><x><htmlparser:read source="text()" var="test1"/><br htmlparser-optional="true"/><htmlparser:read source="deep-text()" var="test2"/></x></a>',
   '<a><pre>not called at all</pre><x>Test:<b>in b</b><c>in c</c>!</x></a>',
   'test2=not called at all'#13'test1=Test:'#13'test2=Test:in bin c!'),
//root node()
('<a><x htmlparser-optional="true"><htmlparser:read source="/a/lh/text()" var="test"/></x></a>',
'<a><lb>ab</lb><x>mia</x><lh>xy</lh></a>',
'test=xy'),
('<a><x htmlparser-optional="true"><htmlparser:read source="/a/lh/text()" var="test"/></x></a>',
'<a><lb>ab</lb><lh>xy</lh></a>',
''),
('<a><x htmlparser-optional="true"><htmlparser:read source="/a/lb/text()" var="test"/></x></a>',
'<a><lb>ab</lb><x>mia</x><lh>xy</lh></a>',
'test=ab'),
//Search
('<a><x><htmlparser:read source="//lh/text()" var="test"/></x></a>',
'<a><lb>ab</lb><x>mia</x><lh>xy</lh></a>',
'test=xy'),
 //html script tags containing <
   ('<a><script></script><b><htmlparser:read source="text()" var="test"/></b></a>',
   '<a><script>abc<def</script><b>test<b></a>',
   'test=test'),
   ('<a><script><htmlparser:read source="text()" var="sitself"/></script><b><htmlparser:read source="text()" var="test"/></b></a>',
   '<a><script>abc<def</script><b>test<b></a>',
   'sitself=abc<def'#13'test=test'),
 //direct closed tags
   ('<a><br/><br/><htmlparser:read source="text()" var="test"/><br/></a>',
   '<a><br/><br   />abc<br /></a>',
   'test=abc'),
 //xpath conditions
   ('<html><a htmlparser-condition="filter(@cond, ''a+'') = ''aaa'' "><htmlparser:read source="text()" var="test"/></a></html>',
   '<html><a>a1</a><a cond="xyz">a2</a><a cond="a">a3</a><a cond="xaay">a4</a><a cond="aaaa">a5</a><a cond="xaaay">a6</a><a cond="xaaaay">a7</a><a cond="xaay">a8</a></html>',
   'test=a6'),


//--new tests--
   //simple read
   ('<table id="right"><tr><td><htmlparser:read source="text()" var="col"/></td></tr></table>',
    '<html><table id="right"><tr><td></td><td>other</td></tr></table></html>',
    'col='),
   ('<html><script><htmlparser:read source="text()" var="col"/></script></html>',
    '<html><script><!--abc--></script></html>',
    'col=<!--abc-->'),
   ('<html><script><htmlparser:read source="text()" var="col"/></script></html>',
    '<html><script>--<!--a--b--c-->--</script></html>',
    'col=--<!--a--b--c-->--'),

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
   //optional elements: test that the first optional element has the highest priority
   ('<a>as<htmlparser:read source="text()" var="a"/></a> <b htmlparser-optional="true"><htmlparser:read source="''found''" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="''found''" var="c"/></c>',
    '<a>asx</a>',
    'a=asx'),
   ('<a>as<htmlparser:read source="text()" var="a"/></a> <b htmlparser-optional="true"><htmlparser:read source="''found''" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="''found''" var="c"/></c>',
    '<a>asx</a><b/>',
    'a=asx'#13'b=found'),
   ('<a>as<htmlparser:read source="text()" var="a"/></a> <b htmlparser-optional="true"><htmlparser:read source="''found''" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="''found''" var="c"/></c>',
    '<a>asx</a><c/>',
    'a=asx'#13'c=found'),
   ('<a>as<htmlparser:read source="text()" var="a"/></a> <b htmlparser-optional="true"><htmlparser:read source="''found''" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="''found''" var="c"/></c>',
    '<a>asx</a><b/><c/>',
    'a=asx'#13'b=found'#13'c=found'),
   ('<a>as<htmlparser:read source="text()" var="a"/></a> <b htmlparser-optional="true"><htmlparser:read source="''found''" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="''found''" var="c"/></c>',
    '<a>asx</a><c/><b/><c/>',
    'a=asx'#13'b=found'#13'c=found'),
   ('<a>as<htmlparser:read source="text()" var="a"/></a> <b htmlparser-optional="true"><htmlparser:read source="''found''" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="''found''" var="c"/></c>',
    '<a>asx</a><c/><b/>',
    'a=asx'#13'b=found'),
    //optional elements: test that the first optional element has the highest priority even in loops
    ('<a>as<htmlparser:read source="text()" var="a"/></a> <htmlparser:loop> <b htmlparser-optional="true"><htmlparser:read source="text()" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="text()" var="c"/></c> </htmlparser:loop>',
     '<a>asx</a><b>B1</b><b>B2</b><b>B3</b>',
     'a=asx'#13'b=B1'#13'b=B2'#13'b=B3'),
    ('<a>as<htmlparser:read source="text()" var="a"/></a> <htmlparser:loop> <b htmlparser-optional="true"><htmlparser:read source="text()" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="text()" var="c"/></c> </htmlparser:loop>',
     '<a>asx</a><c>C1</c><c>C2</c><c>C3</c>',
     'a=asx'#13'c=C1'#13'c=C2'#13'c=C3'),
    ('<a>as<htmlparser:read source="text()" var="a"/></a> <htmlparser:loop> <b htmlparser-optional="true"><htmlparser:read source="text()" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="text()" var="c"/></c> </htmlparser:loop>',
     '<a>asx</a><b>B1</b><b>B2</b><b>B3</b><c>C1</c><c>C2</c><c>C3</c>',
     'a=asx'#13'b=B1'#13'c=C1'), //TODO: is this really the expected behaviour? it searches a <b> and then a <c>, and then the file reaches eof.
    ('<a>as<htmlparser:read source="text()" var="a"/></a> <htmlparser:loop> <b htmlparser-optional="true"><htmlparser:read source="text()" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="text()" var="c"/></c> </htmlparser:loop>',
     '<a>asx</a><c>C1</c><c>C2</c><c>C3</c><b>B1</b><b>B2</b><b>B3</b>',
     'a=asx'#13'b=B1'#13'b=B2'#13'b=B3'), //it searches a <b>, then a <c>, but after the <b> only <c>s are coming
    ('<a>as<htmlparser:read source="text()" var="a"/></a> <htmlparser:loop> <b htmlparser-optional="true"><htmlparser:read source="text()" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="text()" var="c"/></c> </htmlparser:loop>',
     '<a>asx</a><b>B1</b><c>C1</c><b>B2</b><c>C2</c><b>B3</b><c>C3</c>',
     'a=asx'#13'b=B1'#13'c=C1'#13'b=B2'#13'c=C2'#13'b=B3'#13'c=C3'),
     ('<a>as<htmlparser:read source="text()" var="a"/></a> <htmlparser:loop> <b htmlparser-optional="true"><htmlparser:read source="text()" var="b"/></b>  <c htmlparser-optional="true"><htmlparser:read source="text()" var="c"/></c> </htmlparser:loop>',
      '<a>asx</a><b>B1</b><c>C1</c><c>C2</c><b>B3</b><c>C3</c>',
      'a=asx'#13'b=B1'#13'c=C1'#13'b=B3'#13'c=C3'),

     //switch
     //trivial tests
     ('<a><htmlparser:switch><b><htmlparser:read var="v" source="''bBb''"/></b><c><htmlparser:read var="v" source="''cCc''"/></c></htmlparser:switch></a>',
      '<a><b></b></a>',
      'v=bBb'),
     ('<a><htmlparser:switch><b><htmlparser:read var="v" source="''bBb''"/></b><c><htmlparser:read var="v" source="''cCc''"/></c></htmlparser:switch></a>',
      '<a><c></c></a>',
      'v=cCc'),
     ('<a><htmlparser:loop><htmlparser:switch><b><htmlparser:read var="b" source="text()"/></b><c><htmlparser:read var="c" source="text()"/></c></htmlparser:switch></htmlparser:loop></a>',
      '<a><b>1</b><c>2</c><b>4</b><b>5</b><c>6</c><d>ign</d><b>7</b>bla<b>8</b>blub</a>',
      'b=1'#13'c=2'#13'b=4'#13'b=5'#13'c=6'#13'b=7'#13'b=8'),
     ('<a><htmlparser:loop><htmlparser:switch><b><htmlparser:read var="b" source="text()"/></b><c><htmlparser:read var="c" source="text()"/></c></htmlparser:switch></htmlparser:loop></a>',
      '<a><b>1</b><nestene><c>rose</c><consciousness><b>obvious</b><b>ardi</b></consciousness><c>blub</c></nestene></a>',
      'b=1'#13'c=rose'#13'b=obvious'#13'b=ardi'#13'c=blub'),
     ('<a><htmlparser:loop><htmlparser:switch><b><htmlparser:read var="b" source="text()"/></b><c><htmlparser:read var="c" source="text()"/></c></htmlparser:switch></htmlparser:loop></a>',
      '<a><b>1</b><nestene><c>rose</c><consciousness><b>obvious</b><b>ardi</b></consciousness><c>blub</c></nestene></a>',
      'b=1'#13'c=rose'#13'b=obvious'#13'b=ardi'#13'c=blub'),
      //recursive
      ('<a><htmlparser:loop><htmlparser:switch><b><x><htmlparser:read var="bx" source="text()"/></x></b><b><y><htmlparser:read var="by" source="text()"/></y></b></htmlparser:switch></htmlparser:loop></a>',
       '<a><b><x>tx</x></b><n><b><y>ty</y></b>non<b>sense<ll><y>TY</y></ll></b></n><b><y>AY</y></b><c>dep</c><b><x>X</x></b></a>',
       'bx=tx'#13'by=ty'#13'by=TY'#13'by=AY'#13'bx=X'),
      ('<a><htmlparser:loop><htmlparser:switch><b><x><htmlparser:read var="bx" source="text()"/></x></b><b><y><htmlparser:read var="by" source="text()"/></y></b></htmlparser:switch></htmlparser:loop></a>',
       '<a><b><x>tx</x><n><b><y>ty</y></b>non<b>sense<ll><y>TY</y></ll></b></n><b><y>AY</y></b><c>dep</c><b><x>X</x></b></b></a>',
       'bx=tx'), //carefully: here the first </b> is missing/off

   //different text() interpretations
   ('<a><htmlparser:read source="text()" var="A"/><x/><htmlparser:read source="text()" var="B"/></a>',
    '<a>hallo<x></x>a</a>',
    'A=hallo'#13'B=a'),
   ('<table id="right"><htmlparser:loop><tr><td><htmlparser:read source="../text()" var="col"/></td></tr></htmlparser:loop></table>',
    '<table id="right"><tr>pre<td>123</td><td>other</td></tr><tr>ff<td>foo</td><td>columns</td></tr><tr>gg<td>bar</td><td>are</td></tr><tr>hh<td>xyz</td><td>ignored</td></tr></table>',
    'col=pre'#10'col=ff'#10'col=gg'#10'col=hh'),

    //case insensitiveness
    ('<A><htmlparser:read source="text()" var="A"/><x/><htmlparser:read source="text()" var="B"/></A>',
     '<a>hallo<x></x>a</a>',
     'A=hallo'#13'B=a'),
    ('<A att="HALLO"> <htmlparser:read source="@aTT" var="A"/></A>',
     '<a ATT="hallo">xyz</a>',
     'A=hallo'),
    ('<a ATT="olP"> <htmlparser:read source="@aTT" var="A"/></A>',
     '<A att="oLp">xyz</a>',
     'A=oLp')

     //examples taken from http://msdn.microsoft.com/en-us/library/ms256086.aspx
     ,('',
      '<?xml version="1.0"?>'#13#10 +
      '<?xml-stylesheet type="text/xsl" href="myfile.xsl" ?>'#13#10 +
      '<bookstore specialty="novel">'#13#10 +
      '  <book style="autobiography">'#13#10 +
      '    <author>'#13#10 +
      '      <first-name>Joe</first-name>'#13#10 +
      '      <last-name>Bob</last-name>'#13#10 +
      '      <award>Trenton Literary Review Honorable Mention</award>'#13#10 +
      '    </author>'#13#10 +
      '    <price>12</price>'#13#10 +
      '  </book>'#13#10 +
      '  <book style="textbook">'#13#10 +
      '    <author>'#13#10 +
      '      <first-name>Mary</first-name>'#13#10 +
      '      <last-name>Bob</last-name>'#13#10 +
      '      <publication>Selected Short Stories of'#13#10 +
      '        <first-name>Mary</first-name>'#13#10 +
      '        <last-name>Bob</last-name>'#13#10 +
      '      </publication>'#13#10 +
      '    </author>'#13#10 +
      '    <editor>'#13#10 +
      '      <first-name>Britney</first-name>'#13#10 +
      '      <last-name>Bob</last-name>'#13#10 +
      '    </editor>'#13#10 +
      '    <price>55</price>'#13#10 +
      '  </book>'#13#10 +
      '  <magazine style="glossy" frequency="monthly">'#13#10 +
      '    <price>2.50</price>'#13#10 +
      '    <subscription price="24" per="year"/>'#13#10 +
      '  </magazine>'#13#10 +
      '  <book style="novel" id="myfave">'#13#10 +
      '    <author>'#13#10 +
      '      <first-name>Toni</first-name>'#13#10 +
      '      <last-name>Bob</last-name>'#13#10 +
      '      <degree from="Trenton U">B.A.</degree>'#13#10 +
      '      <degree from="Harvard">Ph.D.</degree>'#13#10 +
      '      <award>Pulitzer</award>'#13#10 +
      '      <publication>Still in Trenton</publication>'#13#10 +
      '      <publication>Trenton Forever</publication>'#13#10 +
      '    </author>'#13#10 +
      '    <price intl="Canada" exchange="0.7">6.50</price>'#13#10 +
      '    <excerpt>'#13#10 +
      '      <p>It was a dark and stormy night.</p>'#13#10 +
      '      <p>But then all nights in Trenton seem dark and'#13#10 +
      '      stormy to someone who has gone through what'#13#10 +
      '      <emph>I</emph> have.</p>'#13#10 +
      '      <definition-list>'#13#10 +
      '        <my:title>additional title</my:title>'#13#10 +
      '        <term>Trenton</term>'#13#10 +
      '        <definition>misery</definition>'#13#10 +
      '      </definition-list>'#13#10 +
      '    </excerpt>'#13#10 +
      '  </book>'#13#10 +
      '  <my:book xmlns:my="uri:mynamespace" style="leather" price="29.50">'#13#10 +
      '    <my:title>Who''s Who in Trenton</my:title>'#13#10 +
      '    <my:author>Robert Bob</my:author>'#13#10 +
      '  </my:book>'#13#10 +
      '</bookstore>'#13#10
     ,'')

      ,('<book style="autobiography"><htmlparser:read source="./author" var="test"/></book>','','test=JoeBobTrenton Literary Review Honorable Mention')
      ,('<book style="autobiography"><htmlparser:read source="author" var="test2"/></book>','','test2=JoeBobTrenton Literary Review Honorable Mention')
      ,('<book style="autobiography"><htmlparser:read source="//author" var="test3"/></book>','','test3=JoeBobTrenton Literary Review Honorable Mention')
      ,('<book style="autobiography"><htmlparser:read source="string-join(//author,'','')" var="test"/></book>','','test=JoeBobTrenton Literary Review Honorable Mention,MaryBobSelected Short Stories ofMaryBob,ToniBobB.A.Ph.D.PulitzerStill in TrentonTrenton Forever')
      ,('<bookstore><htmlparser:read source="/bookstore/@specialty" var="test"/></bookstore>','','test=novel')
      ,('<bookstore><htmlparser:read source="book[/bookstore/@specialty=@style]/@id" var="test"/></bookstore>','','test=myfave')
      ,('<bookstore><book><htmlparser:read source="author/first-name" var="test"/></book></bookstore>','','test=Joe')
      ,('<htmlparser:read source="string-join(bookstore//my:title,'','')" var="test"/>','','test=additional title,Who''s Who in Trenton')
      ,('<htmlparser:read source="string-join( bookstore//book/excerpt//emph,'','')" var="test"/>','','test=I')
      ,('<bookstore><book><htmlparser:read source="string-join( author/*,'','')" var="test"/></book></bookstore>','','test=Joe,Bob,Trenton Literary Review Honorable Mention')
      ,('<bookstore><book><htmlparser:read source="string-join( author/*,'','')" var="test"/></book></bookstore>','','test=Joe,Bob,Trenton Literary Review Honorable Mention')
      ,('<bookstore><htmlparser:read source="string-join( book/*/last-name,'','')" var="test"/></bookstore>','','test=Bob,Bob,Bob,Bob')
      ,('<bookstore><book style="textbook"><htmlparser:read source="string-join( */*,'','')" var="test"/></book></bookstore>','','test=Mary,Bob,Selected Short Stories ofMaryBob,Britney,Bob')
      ,('<htmlparser:read source="string-join(*[@specialty]/node-name(.),'','')" var="test"/>','','test=bookstore')
      ,('<bookstore><book><htmlparser:read source="@style" var="test"/></book></bookstore>','','test=autobiography')
      ,('<bookstore><htmlparser:read source="//price/@exchange" var="test"/></bookstore>  ','','test=0.7')
      ,('<bookstore><htmlparser:read source="//price/@exchange/total" var="test"/></bookstore>  ','','test=')
      ,('<bookstore><htmlparser:read source="string-join(book[@style]/price/text(),'','')" var="test"/></bookstore>  ','','test=12,55,6.50')
      ,('<bookstore><htmlparser:read source="string-join(book/@style,'','')" var="test"/></bookstore>  ','','test=autobiography,textbook,novel')
      ,('<bookstore><htmlparser:read source="string-join(@*,'','')" var="test"/></bookstore>  ','','test=novel')
      ,('<bookstore><book><author><htmlparser:read source="string-join( ./first-name,'','')" var="test"/></author></book></bookstore>  ','','test=Joe')
      ,('<bookstore><book><author><htmlparser:read source="string-join( first-name,'','')" var="test"/></author></book></bookstore>  ','','test=Joe')
      ,('<bookstore><book style="textbook"><htmlparser:read source="string-join( author[1],'','')" var="test"/></book></bookstore>  ','','test=MaryBobSelected Short Stories ofMaryBob')
      ,('<bookstore><book style="textbook"><htmlparser:read source="string-join( author[first-name][1],'','')" var="test"/></book></bookstore>  ','','test=MaryBobSelected Short Stories ofMaryBob')
      ,('<bookstore><htmlparser:read source="book[last()]//text()" var="test"/></bookstore>  ','','test=Toni')
      ,('<bookstore><htmlparser:read source="string-join(book/author[last()]/first-name,'','')" var="test"/></bookstore>','','test=Joe,Mary,Toni')
      ,('<bookstore><htmlparser:read source="string-join((book/author)[last()]/first-name,'','')" var="test"/></bookstore>','','test=Toni')
      ,('<bookstore><htmlparser:read source="string-join( book[excerpt]/@style,'','')" var="test"/></bookstore>','','test=novel')
      ,('<bookstore><htmlparser:read source="string-join( book[excerpt]/title,'','')" var="test"/></bookstore>','','test=')
      ,('<bookstore><htmlparser:read source="string-join(  book[excerpt]/author[degree] ,'','')" var="test"/></bookstore>','','test=ToniBobB.A.Ph.D.PulitzerStill in TrentonTrenton Forever')
      ,('<bookstore><htmlparser:read source="string-join(   book[author/degree]/@style   ,'','')" var="test"/></bookstore>','','test=novel')
      ,('<bookstore><htmlparser:read source="string-join( book/author[degree][award] /../@style   ,'','')" var="test"/></bookstore>','','test=novel')
      ,('<bookstore><htmlparser:read source="string-join( book/author[degree and award]  /  ../@style   ,'','')" var="test"/></bookstore>','','test=novel')
      ,('<bookstore><htmlparser:read source="string-join(book/author[(degree or award) and publication]/../@style,'','')" var="test"/></bookstore>','','test=novel')
      ,('<bookstore><htmlparser:read source="string-join(book/author[degree and not(publication)]/../@style,'','')" var="test"/></bookstore>','','test=')
      ,('<bookstore><htmlparser:read source="string-join(book/author[not(degree or award) and publication]/../@style,'','')" var="test"/></bookstore>','','test=textbook')
      ,('<bookstore><htmlparser:read source="string-join(book/author[last-name = ''Bob'']/first-name,'','')" var="test"/></bookstore>','','test=Joe,Mary,Toni')
      ,('<bookstore><htmlparser:read source="string-join(book/author[last-name[1] = ''Bob'']/first-name,'','')" var="test"/></bookstore>','','test=Joe,Mary,Toni')
      ,('<bookstore><htmlparser:read source="string-join(book/author[last-name[position()=1] = ''Bob'']/first-name,'','')" var="test"/></bookstore>','','test=Joe,Mary,Toni')
      //more skipped

      //from wikipedia
      ,('','<?xml version="1.0" encoding="utf-8" standalone="yes" ?>' +
       '<dok>' +
       '    <!-- ein XML-Dokument -->' +
       '    <kap title="Nettes Kapitel">' +
       '        <pa>Ein Absatz</pa>' +
       '        <pa>Noch ein Absatz</pa>' +
       '        <pa>Und noch ein Absatz</pa>' +
       '        <pa>Nett, oder?</pa>' +
       '    </kap>' +
       '    <kap title="Zweites Kapitel">' +
       '        <pa>Ein Absatz</pa>' +
       '    </kap>' +
       '</dok>','' )
      ,('<dok><kap><htmlparser:read source="string-join( /dok ,'';'')" var="test"/></kap></dok>','','test=Ein AbsatzNoch ein AbsatzUnd noch ein AbsatzNett, oder?Ein Absatz')
      ,('<dok><kap><htmlparser:read source="string-join( /* ,'';'')" var="test"/></kap></dok>','','test=Ein AbsatzNoch ein AbsatzUnd noch ein AbsatzNett, oder?Ein Absatz')
      ,('<dok><kap><htmlparser:read source="string-join( //dok/kap ,'';'')" var="test"/></kap></dok>','','test=Ein AbsatzNoch ein AbsatzUnd noch ein AbsatzNett, oder?;Ein Absatz')
      ,('<dok><kap><htmlparser:read source="string-join( //dok/kap[1] ,'';'')" var="test"/></kap></dok>','','test=Ein AbsatzNoch ein AbsatzUnd noch ein AbsatzNett, oder?')
      ,('<dok><kap><htmlparser:read source="string-join( //pa,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?;Ein Absatz')
      ,('<dok><kap><htmlparser:read source="string-join( //kap[@title=''Nettes Kapitel'']/pa,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?')
      ,('<dok><kap><htmlparser:read source="string-join( child::*,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?')
      ,('<dok><kap><htmlparser:read source="string-join( child::pa,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?')
      ,('<dok><kap><htmlparser:read source="string-join( child::text(),'';'')" var="test"/></kap></dok>','','test=')
      ,('<dok><kap><pa><htmlparser:read source="string-join( text(),'';'')" var="test"/></pa></kap></dok>','','test=Ein Absatz')
      ,('<dok><kap><pa><htmlparser:read source="string-join( ./*,'';'')" var="test"/></pa></kap></dok>','','test=')
      ,('<dok><kap><htmlparser:read source="string-join( ./*,'';'')" var="test"/></kap></dok>','','test=Ein Absatz;Noch ein Absatz;Und noch ein Absatz;Nett, oder?')


);


var i:longint;
    extParser:THtmlTemplateParser;
    sl:TStringList;
  procedure checklog(s:string);
  var j: Integer;
  begin
      sl.Text:=s;
      //check lines to avoid line ending trouble with win/linux
      if extParser.variableChangeLog.Count<>sl.Count then
        raise Exception.Create('Test failed: '+inttostr(i)+': ' +' got: "'+extParser.variableChangeLog.debugTextRepresentation+'" expected: "'+s+'"');
      for j:=0 to sl.count-1 do
        if (extParser.variableChangeLog.getVariableName(j)<>sl.Names[j]) or
           (pxpvalueToString(extParser.variableChangeLog.getVariableValue(j))<>sl.ValueFromIndex[j])     then
          raise Exception.Create('Test failed: '+ inttostr(i)+': '+data[i][1] + #13#10' got: "'+extParser.variableChangeLog.debugTextRepresentation+'" expected: "'+s+'"');
  end;
var previoushtml: string;
    procedure performTest(const template, html, expected: string);
    begin
      if html<>'' then previoushtml:=html;
      if template='' then exit;
      extParser.parseTemplate(template);
      extParser.parseHTML(previoushtml);
      checklog(expected);
    end;

begin
  extParser:=THtmlTemplateParser.create;
  sl:=TStringList.Create;
  for i:=low(data)to high(data) do performTest(data[i,1],data[i,2],data[i,3]);

  //---special encoding tests---
  extParser.parseTemplate('<a><htmlparser:read source="text()" var="test"/></a>');
  //no coding change utf-8 -> utf-8
  extParser.outputEncoding:=eUTF8;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><a>uu(bin:'#$C3#$84',ent:&Ouml;)uu</a></html>');
  if extParser.variableChangeLog.ValuesString['test']<>'uu(bin:'#$C3#$84',ent:'#$C3#$96')uu' then //ÄÖ
    raise Exception.create('ergebnis ungültig utf8->utf8');
  //no coding change latin1 -> latin1
  extParser.outputEncoding:=eWindows1252;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" /><a>ll(bin:'#$C4',ent:&Ouml;)ll</a></html>');
  if extParser.variableChangeLog.ValuesString['test']<>'ll(bin:'#$C4',ent:'#$D6')ll' then
    raise Exception.create('ergebnis ungültig latin1->latin1');
  //coding change latin1 -> utf-8
  extParser.outputEncoding:=eUTF8;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" /><a>lu(bin:'#$C4',ent:&Ouml;)lu</a></html>');
  if extParser.variableChangeLog.ValuesString['test']<>'lu(bin:'#$C3#$84',ent:'#$C3#$96')lu' then
    raise Exception.create('ergebnis ungültig latin1->utf8');
  //coding change utf8 -> latin1
  extParser.outputEncoding:=eWindows1252;
  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><a>ul(bin:'#$C3#$84',ent:&Ouml;)ul</a></html>');
  if extParser.variableChangeLog.ValuesString['test']<>'ul(bin:'#$C4',ent:'#$D6')ul' then
    raise Exception.create('ergebnis ungültig utf8->latin1');

  extParser.parseHTML('<html><head><meta http-equiv="Content-Type" content="text/html; charset=" /><a>bin:'#$C4#$D6',ent:&Ouml;</a></html>');
  extParser.outputEncoding:=eUTF8;



  //---special keep variables test---
  i:=-2;
  extParser.variableChangeLog.Clear;
  extParser.variableChangeLog.ValuesString['Hallo']:='diego';
  extParser.parseTemplate('<a><htmlparser:read source="text()" var="hello"/></a>');
  extParser.parseHTML('<a>maus</a>',true);
  if extParser.variableChangeLog.ValuesString['hello']<>'maus' then
    raise Exception.Create('invalid var');
  if extParser.variableChangeLog.ValuesString['Hallo']<>'diego' then
    raise Exception.Create('invalid var');
  checklog('Hallo=diego'#13'hello=maus');
  extParser.parseTemplate('<a><htmlparser:read source="text()" var="Hallo"/></a>');
  extParser.parseHTML('<a>maus</a>',true);
  if extParser.variableChangeLog.ValuesString['hello']<>'maus' then
    raise Exception.Create('invalid var');
  if extParser.variableChangeLog.ValuesString['Hallo']<>'maus' then
    raise Exception.Create('invalid var');
  checklog('Hallo=diego'#13'hello=maus'#13'Hallo=maus');



  extParser.free;
  sl.Free;
end;

initialization
unitTests();

{$ENDIF}


(*         deprecated things

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
          //if (lastElement<>nil) (and (nextElement.{<>lastElement.next})* then
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

*)




end.

