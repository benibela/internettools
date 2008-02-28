unit extendedhtmlparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,simplehtmlparser,regexpr,bbutils;




type

  EHTMLParseException=class(exception)
  end;
  ETemplateParseException=class(exception)
  end;

  { TTemplateElement }

  TTemplateElementType=(tetCommandRead,tetCommandLoop,tetCommandIf, tetHTML,tetText);
  TTemplateElement= class
    typ: TTemplateElementType;
    closeTag: boolean;
    text: string; //Name bzw. Inhalt für tetText
    attributes: TStringList;  //nil für tetText
    //children: TList;
    reverse: TTemplateElement; //Schließen/Öffnen
    next,rnext: TTemplateElement;
    id:longint; //Nur für debugging zwecke
    function toStr:string;
    procedure freeAll;
    destructor destroy;override;
  end;

const TEMPLATE_COMMANDS=[tetCommandRead..tetCommandIf];
      COMMAND_CLOSED:array[tetCommandRead..tetCommandIf] of boolean=(false,true,true);
      COMMAND_STR:array[tetCommandRead..tetCommandIf] of string=('read','loop','if');
type

  { TParsingStatus }

  TParsingStatus=class
    lastElement: TTemplateElement;
    nextElement: TTemplateElement;
    lastopenedElement, latestElement: TTemplateElement; //debugging
    elementStack: TStringList;
    destructor destroy;override;
  end;

  TNotifyCallbackFunction = procedure () of object;
  TVariableCallbackFunction = procedure (variable: string; value: string) of object;
  TReadCallbackFunction = procedure (read: pchar; readLen:longint) of object;

  TReplaceFunction = procedure (variable: string; var value:string) of object;
  
  { THtmlTemplateParser }

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

    FParsingCompleted: boolean;
    FlastText:string;
    //FCurrentStack: TStringList;
    FVariables,FNotifyFunctions,FVariableFunctions: TStringList;
    FOldProperties: THTMLProperties;
    FParsingAlternatives: TFPList;
    FAutoCloseTag: boolean;
    LastEventIsText:boolean;

    FOnEnterTag, FOnLeaveTag, FOnTextRead: TReadCallbackFunction;
    FOnVariableRead: TVariableCallbackFunction;

    procedure finishHtmlParsing(status:TParsingStatus);
    function readTemplateElement(status:TParsingStatus):boolean; //gibt false nach dem letzten zurück
    function parsePseudoXPath(str: string):string;
    procedure executeTemplateCommand(status:TParsingStatus;cmd: TTemplateElement;beforeReading:boolean);
    function enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
    function leaveTag(tagName: pchar; tagNameLen: longint):boolean;
    function textEvent(text: pchar; textLen: longint):boolean;
  public
    constructor create;
    destructor destroy; override;

    procedure parseHTML(html: string);
    procedure parseTemplate(template: string);
    procedure parseTemplateFile(templatefilename: string);
    procedure addFunction(name:string;varCallFunc: TVariableCallbackFunction);overload;
    procedure addFunction(name:string;notifyCallFunc: TNotifyCallbackFunction);overload;

    function replaceVars(s:string;customReplace: TReplaceFunction=nil):string;

    property variables: TStringList read Fvariables;
    property onEnterTag: TReadCallbackFunction read FOnEnterTag write FOnEnterTag;
    property onLeaveTag: TReadCallbackFunction read FOnLeaveTag write FOnLeaveTag;
    property onTextRead: TReadCallbackFunction read FOnTextRead write FOnTextRead;
    property onVariableRead: TVariableCallbackFunction read FOnVariableRead write FOnVariableRead;
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
    if (latestElement=nil) or ((nextElement<>nil) and (nextElement.id>latestElement.id)) then
      latestElement:=nextElement;
    if (nextElement<>nil) and (nextElement.typ = tetHTML) and not nextElement.closeTag then
      lastopenedElement:=nextElement;
  end;
end;

function THtmlTemplateParser.parsePseudoXPath(str: string): string;
var comma,comparison:longint;
    pos:pchar;
  procedure raiseParsingError(s:string);
  begin
    raise Exception.Create(s+#13#10'in: '+strcopy2(@str[1],pos-1)+' [<- fehler] '+strcopy2(pos,@str[length(str)]));
  end;
  function nextToken:string;
  const SYMBOLS = ['''','(','=','!','<','>',')',','];
  var start:pchar;
  begin
    while pos^ in WHITE_SPACE do pos+=1;
    if pos^ = #0 then exit('');
    start:=pos;
    if pos^='''' then begin
      repeat
        pos+=1;
      until pos^ in ['''',#0];
      pos+=1;
    end else if pos^ in SYMBOLS then begin
      pos+=1;
      exit((pos-1)^);
    end else begin
      repeat
        pos+=1;
      until pos^ in SYMBOLS + WHITE_SPACE + [#0];
    end;
    result:=strcopy2(start,pos-1);
  end;
  procedure expect(c:char);
  begin
    while pos^ in WHITE_SPACE do pos+=1;
    if pos^ <> c then
      raise Exception.Create(c+' erwartet aber '+pos^+' gefunden'#13#10+strcopy2(@str[1],pos));
    pos+=1;
  end;
var values: array[1..8] of string;
    actions: array[1..8] of (aNormal,aConcat,aCompareEqual,aCompareUnequal);
    currentValue,currentAction: longint;
  procedure processValues;
  begin
    while currentAction>1 do
      case actions[currentAction] of
        aNormal: currentAction-=1;
        aConcat: begin
          currentValue-=1;
          values[currentValue]+=values[currentValue+1];
          break;
        end;
        aCompareEqual, aCompareUnequal: begin
          if SameText(values[currentValue-1],values[currentValue]) = (actions[currentAction]=aCompareEqual) then
            values[currentValue-1]:='true'
           else
            values[currentValue-1]:='false';
          currentValue-=1;
          currentAction-=1;
        end;
      end;
  end;
  procedure readValue(s:string);
  begin
    values[currentValue]:=s;
//    processValues;
  end;
var word:string;
begin
  //Möglichkeiten:
  //func(<1>,<2>,<3>,...)
  //<1> [==,!=, =] <2>
  //@attrib
  //'...'
  str:=trim(str);
  if str='' then exit('');
  if str='text()' then exit(FlastText);
  currentValue:=1;
  currentAction:=1;
  actions[1]:=aNormal;
  values[1]:='';

  pos:=@str[1];
  
  while pos^<>#0 do begin
    word:=nextToken();
    if word='' then break;
    case word[1] of
      '''': readValue(copy(word,2,length(word)-2));
      '@': readValue(getProperty(copyfrom(word,2),FOldProperties));
      '=': begin
        if pos^='=' then pos+=1; //auch == erlauben
        currentAction+=1;
        currentValue+=1;
        values[currentValue]:='';
        actions[currentAction]:=aCompareEqual;
      end;
      '!': begin
        expect('=');
        currentAction+=1;
        currentValue+=1;
        values[currentValue]:='';
        actions[currentAction]:=aCompareUnequal;
      end;
      '(': if actions[currentAction]=aConcat then begin
        currentAction+=1;
        currentValue+=1;
        values[currentValue]:='';
        actions[currentAction]:=aNormal;
      end else raiseParsingError('Klammerung nicht implementiert');
      ',': begin
        processValues;
        if actions[currentAction]=aConcat then begin
          currentAction+=1;
          currentValue+=1;
          values[currentValue]:='';
          actions[currentAction]:=aNormal;
        end else raiseParsingError('Komma außerhalb Funktionsparameter');
      end;
      ')': begin
        processValues;
        if actions[currentAction]=aConcat then currentAction-=1
        else raiseParsingError('Schließende Klammer ohne Funktion');
      end;
      else if SameText(word,'text') then begin
        expect('(');expect(')');
        readValue(FlastText);
      end else if SameText(word,'concat') then begin
        currentAction+=1;
        actions[currentAction]:=aConcat;
      end else
        raise Exception.Create('Unbekannter Pseudo-XPath-Befehl: '+word+' in '#13#10+strcopy2(@str[1],pos));
    end;
  end;
  processValues;
  result:=values[1];
 { if str[1] = '@' then begin //Ausdruck beginnt mit Attribut
    comparison:=pos('=',str);
    if comparison<=0 then comparison:=pos('!',str);
    if comparison<=0 then begin;
    for i:=2 to length(str) do
      if str[i] in
  end;
 for i:=1 to str do
  last:=@str[1];
  while pos^<>#0 do begin
    if pos^ in [' ','=','(']
  end;
  else
  else if str[1]='@' then
    exit()
  else if str[1]='''' then
    exit(copy(str,2,length(str)-2))
  else if strlibeginswith(str,'concat') then begin
    delete(str,1,pos('(',str));
    result:='';
    comma:=pos(',',str);
    while comma>0 do begin
      result+=parsePseudoXPath(copy(str,1,comma-1));
      delete(str,1,comma);
      comma:=pos(',',str);
      if comma=0 then comma:=rpos(')',str);
    end;
  end else raise ETemplateParseException.Create('Ungültige Datenquelle '+str+' im Template');}
end;

procedure THtmlTemplateParser.executeTemplateCommand(status:TParsingStatus;cmd: TTemplateElement;beforeReading:boolean);
var text,condition:string;
    comparisonPos:longint;
    equal:boolean;
    regexp: TRegExpr;
    ls,rs:string;
begin

  if beforeReading then exit;begin
    //Lesen muss nach dem nächsten Element ausgeführt werden,
    //weil vorher der Text des aktuellen Elements nicht
    //bekannt ist
    case cmd.typ of
      tetCommandRead: begin
        text:=parsePseudoXPath(replaceVars(cmd.attributes.Values['source']));

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
          if (htmlEncoding=eUTF8) and (outputEncoding=eWindows1252) then
            text:=Utf8ToAnsi(text)
           else if (htmlEncoding=eWindows1252) and (outputEncoding=eUTF8) then
            text:=AnsiToUtf8(text);

        variables.Values[cmd.attributes.Values['var']]:=text;
        if Assigned(FOnVariableRead) then FOnVariableRead(cmd.attributes.Values['var'],text);
      end;
    end;
//  end else begin
    case cmd.typ of
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
        
        equal:=parsePseudoXPath(replaceVars(condition))='true';
        
        if not equal then begin
          status.nextElement:=cmd.reverse;
          status.lastElement:=cmd.reverse;
          //debug history:
          if (status.latestElement=nil) or ((status.nextElement<>nil) and (status.nextElement.id>status.latestElement.id)) then
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
            if (latestElement=nil) or ((nextElement<>nil) and (nextElement.id>latestElement.id)) then
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
            if (latestElement=nil) or ((nextElement<>nil) and (nextElement.id>latestElement.id)) then
              latestElement:=nextElement;
          end;
        end;
      end;
    end;
  end;

end;

function THtmlTemplateParser.enterTag(tagName: pchar; tagNameLen: longint;
  properties: THTMLProperties): boolean;

  function fit(element:TTemplateElement):boolean;
  var i,j,found,ok:longint;
      Name:string;
  begin
    if not strliequal(tagName,element.text,tagNameLen) then
      exit(false);
    if element.attributes=nil then
      exit(true);
    for i := 0 to element.attributes.Count-1 do begin
      found:=-1;
      Name:=element.attributes.Names[i];
      for j:=0 to high(properties) do
        if strliequal(properties[j].name,name,properties[j].nameLen) then begin
          if strliequal(properties[j].value,element.attributes.ValueFromIndex[i],properties[j].valueLen) then
            found:=i;
          break;
        end;
      if found=-1 then exit(false);
    end;
    exit(true);
  end;

var i,j:longint;
    element: TTemplateElement;
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
            if htmlEncoding=eWindows1252 then element.text:=Utf8ToAnsi(element.text)
            else if htmlEncoding=eUTF8 then element.text:=AnsiToUtf8(element.text)
            else raise exception.Create('Unbekannte Codierung, kann nicht passieren');
          end else begin //HTML-Namen sind ASCII und deshalb in UTF-8 und W1252 gleich
            if element.attributes<>nil then
              if htmlEncoding=eWindows1252 then element.attributes.text:=Utf8ToAnsi(element.attributes.text)
              else if htmlEncoding=eUTF8 then element.attributes.text:=AnsiToUtf8(element.attributes.text)
              else raise exception.Create('Unbekannte Codierung, kann nicht passieren');
          end;
          element:=element.next;
        end;
        templateEncoding:=htmlEncoding;
      end;
    end;
  FAutoCloseTag:=strliequal(tagName,'br',tagNameLen) or
                 strliequal(tagName,'input',tagNameLen) or
                 strliequal(tagName,'hr',tagNameLen)or
                 strliequal(tagName,'img',tagNameLen)or
                 strliequal(tagName,'p',tagNameLen);

  result:=true;
  for i:=0 to FParsingAlternatives.Count-1 do
    with TParsingStatus(FParsingAlternatives[i]) do begin
      if fit(nextElement) then begin
        elementStack.AddObject(pcharToStringSimple(tagName,tagNameLen),nextElement);
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
        elementStack.AddObject(pcharToStringSimple(tagName,tagNameLen),nil);
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
      if elementStack.Objects[closed]<>nil then
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

      //if strliequal(tagName,tagNameLen,FNextTemplateElement.text) then
      elementStack.Delete(closed);
    end;
  end;
  if FParsingAlternatives.count=0 then exit(false);
end;


function THtmlTemplateParser.textEvent(text: pchar; textLen: longint): boolean;
var alt,i: longint;
begin
  if assigned(FOnTextRead) then FOnTextRead(text,textLen);
  result:=true;
  LastEventIsText:=true;
{  for i:=0 to textLen-1 do
    if not ((text+i)^ in WHITE_SPACE) then begin
      FlastText:=trim(pcharToString(text+i,textlen-i));
      break;
    end;                                    }
  FlastText:=trim(decodeHTMLEntities(text,textlen,htmlEncoding));
  if FlastText='' then
    FlastText:=decodeHTMLEntities(text,textlen,htmlEncoding);

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

function THtmlTemplateParser.newTemplateElement(text: pchar; textLen: longint):TTemplateElement;
begin
  if FRootTemplate=nil then begin
    FRootTemplate:=TTemplateElement.create;
    FCurrentTemplateElement:=FRootTemplate;
  end else begin
    FCurrentTemplateElement.next:=TTemplateElement.Create;
    FCurrentTemplateElement:=FCurrentTemplateElement.next;
  end;
  FCurrentTemplateElement.text:=pcharToStringSimple(text,textLen);
  FTemplateCount+=1;
  FCurrentTemplateElement.id:=FTemplateCount;
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

function THtmlTemplateParser.strToCommand(tagName: pchar; tagNameLen: longint
  ): TTemplateElementType;
var i:longint;
begin
  Result:=tetHTML;
  if strlibeginswith(tagName,tagNameLen,'htmlparser:') then begin
    tagName+=length('htmlparser:');
    tagNameLen-=length('htmlparser:');
    for Result:=low(COMMAND_STR) to high(COMMAND_STR) do
      if strliequal(tagName,COMMAND_STR[Result],tagNameLen) then
        exit();
    raise ETemplateParseException.Create('Unbekannter Templatebefehl: htmlparser:'+pcharToStringSimple(tagName,tagNameLen))
  end;
end;

function THtmlTemplateParser.templateEnterTag(tagName: pchar;
  tagNameLen: longint; properties: THTMLProperties): boolean;
var nte: TTemplateElement; //New Template ELement
    i:longint;
begin
  if strliequal(tagName,'htmlparser:meta',tagNameLen) then begin
    if getProperty('encoding',properties)='utf-8' then templateEncoding:=eUTF8
    else templateEncoding:=eWindows1252;
    exit;
  end;
  //workaround, htmlparser:read and if needs text
  if (strliequal(tagName,'htmlparser:read',tagNameLen)) or
     (strliequal(tagName,'htmlparser:if',tagNameLen)) then
    {if pos('text()',LowerCase(getProperty('source',properties)))>0 then }begin
      nte:=newTemplateElement('',0);
      nte.typ:=tetText;
      rememberNewHTMLElement(nte);
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
  if length(properties)>0 then begin
    FCurrentTemplateElement.attributes:=TStringList.Create;
    for i:=0 to high(properties) do
      with properties[i] do
        FCurrentTemplateElement.attributes.Add(trim(pcharToStringSimple(name,nameLen))+'='+
                                               trim(pcharToStringSimple(value,valueLen)));
  end;

end;

function THtmlTemplateParser.templateLeaveTag(tagName: pchar; tagNameLen: longint):boolean;
var nte: TTemplateElement; //New Template ELement
    command: TTemplateElementType;
begin
  Result:=true;
  command:=strToCommand(tagName,tagNameLen);
  if command in TEMPLATE_COMMANDS then
    if not COMMAND_CLOSED[command] then
      exit;

  if (FTemplateElementStack.Count = 0) then
    raise Exception.Create('Nicht geöffneter Tag '+pcharToStringSimple(tagName,tagNameLen)+' wurde im Template geschlossen.');

  if not strliequal(tagname,TTemplateElement(FTemplateElementStack[FTemplateElementStack.Count-1]).text,tagNameLen) then
    raise Exception.Create('Der Tag '+pcharToStringSimple(tagName,tagNameLen)+' wurde im Template geschlossen, obwohl "'+TTemplateElement(FTemplateElementStack[FTemplateElementStack.Count-1]).text+'" dran wäre.');

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
       raise EHTMLParseException.create('Die HTML Datei ist kürzer als das Template'#13#10+
                                        'last: '+TParsingStatus(FParsingAlternatives[0]).lastElement.toStr+#13#10+
                                        'next: '+TParsingStatus(FParsingAlternatives[0]).nextElement.toStr+#13#10+
                                        'latest: '+TParsingStatus(FParsingAlternatives[0]).latestElement.toStr+#13#10+
                                        'lastopened: '+TParsingStatus(FParsingAlternatives[0]).lastopenedElement.toStr+#13#10+
                                        'Zahl der Alternativen: '+IntToStr(FParsingAlternatives.Count));
  finally
    for i:=0 to FParsingAlternatives.count-1 do
      TParsingStatus(FParsingAlternatives[i]).free;
    FParsingAlternatives.Clear;
  end;
end;

procedure THtmlTemplateParser.parseTemplate(template: string);
begin
  //FVariables.clear;
  if template='' then
    raise ETemplateParseException.Create('Kein Template gefunden');
  FTemplateCount:=0;
  if FRootTemplate<>nil then FRootTemplate.freeAll;
  FRootTemplate:=nil;
  FTemplateElementStack.Clear;
  FLastHTMLTemplateElement:=nil;
  templateEncoding:=eWindows1252;
  if strbeginswith(template,#$ef#$bb#$bf) then begin
    delete(template,1,3);
    templateEncoding:=eUTF8;
  end else if strbeginswith(template,#$fe#$ff) or strbeginswith(template,#$ff#$fe) or
    strbeginswith(template,#00#00#$fe#$ef) then
    raise Exception.Create('Ungültiger Codierung BOM im Template');
  simplehtmlparser.parseHTML(template,@templateEnterTag,@templateLeaveTag,@templateTextEvent);
  if FRootTemplate = nil then
    raise ETemplateParseException.Create('Ungültiges/Leeres Template');
end;

procedure THtmlTemplateParser.parseTemplateFile(templatefilename: string);
begin
  parseTemplate(loadFileToStr(templatefilename));
end;

procedure THtmlTemplateParser.addFunction(name: string;varCallFunc: TVariableCallbackFunction);
begin
  FVariableFunctions.AddObject(name,tobject(@varCallFunc));
end;

procedure THtmlTemplateParser.addFunction(name: string;notifyCallFunc: TNotifyCallbackFunction);
begin
  FNotifyFunctions.AddObject(name,tobject(@notifyCallFunc));
end;

function THtmlTemplateParser.replaceVars(s: string;customReplace: TReplaceFunction=nil): string;
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


{ TTemplateElement }

function TTemplateElement.toStr: string;
begin
  if self=nil then exit('nil');
  if closeTag then  result:='/'+text
  else  result:=text;
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
    result:=result+'<!--'+inttostr(el.id)+'-->'#13#10;
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
              IntTostr(TParsingStatus(parser.FParsingAlternatives[i]).nextElement.id)+'; '
    end else
      Result:=Result+'nil;';
//  result+='   |'+parser.FlastText;
end;

function TTemplateHTMLParserLogClass.conv(t: pchar; tl: longint): string;
begin
  result:=StringReplace(StringReplace(decodeHTMLEntities(t,tl,eWindows1252),#13,' ',[rfReplaceAll]),#10,' ',[rfReplaceAll]);
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



{$IFDEF UNITTESTS}
procedure unitTest(extParser: THtmlTemplateParser;testID:longint;logClass: TTemplateHTMLParserLogClass);
var sl: TStringList;
i:longint;

begin
  case testID of
    1: begin //Verschiedene Lesetests
      extParser.parseTemplate('<a><b><htmlparser:read source="text()" var="test"/></b></a>');
      extParser.parseHTML('<a><b>Dies wird Variable test</b></a>');
      if extParser.variables.Values['test']<>'Dies wird Variable test' then
        raise Exception.create('ungültiges Ergebnis');
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
    29: if FileExists('U:\components\pascal\html\tests\test8.template') then begin
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
    end else raise Exception.Create('Input für Test nicht vorhanden');

    30  : if FileExists('T:\test.template') then begin //Freier Test
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

const
    pseudoXpathTests: array[1..16] of string = ('''Test''', '''a == b''', '''a'' == ''b''', '''abc'' == ''abc''',  '''123'' != ''abc''', 'concat(''a'',''b'',''c'')', 'concat(''one'')',   'concat(''hallo'', '' '', ''welt'') == ''hallo welt''', 'concat(''a'',''b'',concat(''c'',''d''))','concat(''a'',concat(''x'',''y'',''z''),''b'',''c'')', '''$test;''==''abc''', 'concat  (  ''a'',  ''b'',  ''c''  )', 'concat(''cond is '',''abc''==''abc'')', 'concat(''>'',''123''!=''test'',''<'')',   'concat(concat(''123'',''abc'')==''123abc'',''-#-'')',  'concat(''('',''abc''==concat(''a'',''b'',''c''),'')'')');
    pseudoXpathResults: array[1..16] of string = ('Test',     'a == b',      'false',              'true',             'true',           'abc',                'one',                               'true',                                                  'abcd',                                        'axyzbc',                                      'false',                   'abc',                                'cond is true',                              '>true<',                                'true-#-',                                            '(true)');

var i:longint;
    extParser:THtmlTemplateParser;
    log:TTemplateHTMLParserLogClass;
    sl:TStringList;
begin
  extParser:=THtmlTemplateParser.create;
  
  for i:=1 to high(pseudoXpathTests) do
    if extParser.parsePseudoXPath(pseudoXpathTests[i])<>pseudoXpathResults[i] then
      raise Exception.Create('XPath Test failed: '+IntToStr(i)+ #13#10+extParser.parsePseudoXPath(pseudoXpathTests[i]));
  
  sl:=TStringList.Create;
  log:=TTemplateHTMLParserLogClass.Create;
  log.parser:=extParser;
  extParser.onEnterTag:=@log.et;
  extParser.onLeaveTag:=@log.lt;
  extParser.onTextRead:=@log.tr;
  extParser.onVariableRead:=@log.vr;
  for i:=1 to 30 do begin
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
  log.free;  extParser.free; sl.free;
end;

initialization
unitTests();

{$ENDIF}


end.

