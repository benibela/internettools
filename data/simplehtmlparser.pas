unit simplehtmlparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 


type
  THTMLProperty=record
    name, value: pchar;
    nameLen, valueLen: longint;
  end;
  THTMLProperties=array of THTMLProperty;
  TEnterTagEvent=function (tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean of object;
  TLeaveTagEvent=function (tagName: pchar; tagNameLen: longint):boolean of object;
  TTextEvent=function (text: pchar; textLen: longint):boolean of object;

  function pcharStartEqual(p1,p2:pchar;l1,l2: longint):boolean;

  procedure parseHTML(const html:string;
                      enterTagEvent: TEnterTagEvent; leaveTagEvent: TLeaveTagEvent;
                      textEvent: TTextEvent);



  function existPropertyWithValue(propertyName,propertyValue: string; properties:THTMLProperties):boolean;
  function getProperty(propertyName: string; properties:THTMLProperties):string;
  function findLinkWithText(const html:string;text: string):string;
  function findLinkWithProperty(const html:string;prop,value: string):string;
  function findTagPropertyValueWithProperty(const html:string;tag,prop_to_get,prop_must_match,value: string):string;
implementation
uses bbutils;

function pcharStartEqual(p1,p2:pchar;l1,l2: longint):boolean;
begin
  if l1>l2 then l1:=l2
  else l2:=l1;
  result:=strlequal(p1,p2,l1,l2);
end;





procedure parseHTML(const html:string;
                    enterTagEvent: TEnterTagEvent; leaveTagEvent: TLeaveTagEvent;
                    textEvent: TTextEvent);
var pos,marker,htmlEnd: pchar;
    valueStart:char;
    tempLen:longint;
    properties:THTMLProperties;
    inScriptTag: boolean; //no start tags allowed in script
begin
  pos:=@html[1];
  htmlEnd:=@html[length(html)];
  marker:=pos;
  inScriptTag:= false;
  while (pos<=htmlEnd) do begin
    case pos^ of
      '<': begin //Start or end of a tag
        if (marker<>pos)and(assigned(textEvent)) then
          if not textEvent(marker,pos-marker) then exit;

        inc(pos);
        case pos^ of
          '!':begin //comment start
            inScriptTag:=false;
            inc(pos);
            if (pos^='D') and ((pos+1)^='O') and ((pos+2)^='C') and
               ((pos+3)^='T') and ((pos+4)^='Y')and ((pos+5)^='P')and ((pos+6)^='E')  then begin//doctype
              while (pos<=htmlEnd) and (pos^ <> '>') do inc(pos);
              inc(pos);
             end else begin
              while (pos<=htmlEnd) and ((pos^<>'-') or ((pos+1)^<>'-') or ((pos+2)^<>'>')) do
                inc(pos);
              inc(pos,3);
             end;
             marker:=pos;
          end;
          '/': begin //tag end
            inc(pos);
            marker:=pos;
            while (pos<=htmlEnd) and not (pos^ in [' ','>']) do inc(pos);
            if assigned(leaveTagEvent) then
              if not leaveTagEvent(marker,pos-marker) then exit;
            if inScriptTag and (strliequal(marker,'script',pos-marker)) then
              inScriptTag:=false;
            while (pos<=htmlEnd) and (pos^ <> '>') do inc(pos);
            inc(pos);
            marker:=pos;
          end;
          else if not inScriptTag  then begin //tag start
            marker:=pos;
            setlength(properties,0);
            while (pos<=htmlEnd) and not (pos^ in ['>',' ']) do
              inc(pos);
            tempLen:=pos-marker;
            //read properties
            while (pos<=htmlEnd) and  (pos^ in WHITE_SPACE) do begin
              while (pos<=htmlEnd) and  (pos^ in WHITE_SPACE) do inc(pos);
              if pos^='>' then break;
              if pos>htmlEnd then exit;
              //new property
              setlength(properties,length(properties)+1);
              with properties[high(properties)] do begin
                //search start of name
                name:=pos;
                while (pos<=htmlEnd) and not (pos^ in (WHITE_SPACE + ['=','>'])) do inc(pos);
                nameLen:=pos-name;
                //find value start
                while (pos<=htmlEnd) and (pos^ in (WHITE_SPACE)) do inc(pos);
                if pos^ <> '=' then begin
                   value:=name;
                   valueLen:=0;
                   dec(pos);
                end else begin
                  inc(pos);
                  while (pos<=htmlEnd) and (pos^ in (WHITE_SPACE)) do inc(pos);
                  if not (pos^ in ['''','"']) then begin
                    //value not in ""
                    value:=pos;
                    while (pos<=htmlEnd) and not (pos^ in (WHITE_SPACE + ['>'])) do
                      if (pos^<>'/') or ((pos+1)^<>'>') then inc(pos)
                      else break;
                    valueLen:=pos-value;
                   end else if pos^ = '>' then begin
                     value:=name;
                     valueLen:=0;
                     break;
                   end else begin
                    //value in ""
                    valueStart:=pos^;
                    inc(pos);
                    value:=pos;
                    while (pos<=htmlEnd) and (pos^<>valueStart) do inc(pos);
                    valueLen:=pos-value;
                    inc(pos);
                  end;
                end;
              end;
            end;
            while (marker[tempLen-1] in ['/', ' ']) and (tempLen>0) do
              dec(tempLen);
            if assigned(enterTagEvent) then
              if not enterTagEvent(marker,tempLen,properties) then exit;
            inScriptTag:=strliequal(marker,'script',tempLen);
            if (pos^ = '/') or ((pos-1)^ = '/' ) then begin
              if assigned(leaveTagEvent) then
                if not leaveTagEvent(marker,tempLen) then exit;
              if pos^ = '/'  then inc(pos);
            end;{ else if ((marker^ in ['b','B']) and
                          ((marker+1)^ in ['r','R'])) or
                        ((marker^ in ['m','M']) and
                          ((marker+1)^ in ['e','E']) and
                          ((marker+2)^ in ['t','T']) and
                          ((marker+3)^ in ['a','A'])) then
              leaveTagEvent(marker,tempLen)};
            if pos^='>' then inc(pos);
            marker:=pos;
          end;
        end;
      end;
      else inc(pos);
    end;
  end;
end;

function existPropertyWithValue(propertyName,propertyValue: string; properties:THTMLProperties):boolean;
var i:integer;
begin
  for i:=0 to high(properties) do
    if strliequal(properties[i].name,@propertyName[1],properties[i].nameLen,length(propertyName)) and
       strliequal(properties[i].value,@propertyValue[1],properties[i].valueLen,length(propertyValue)) then
      exit(true);
  result:=false;
end;

function getProperty(propertyName: string; properties:THTMLProperties):string;
var i:integer;
begin
  for i:=0 to high(properties) do
    if strliequal(properties[i].name,@propertyName[1],properties[i].nameLen,length(propertyName)) then begin
      setlength(result,properties[i].valueLen);
      move(properties[i].value[0],result[1],properties[i].valueLen);
      exit;
    end;
  result:='';
end;

type TTempSearchClassText=class
  result: string;
  //tag,prop_to_get,prop_must_match,value: string;
  lastLinkURL: THTMLProperty;
  lastLink: boolean;
  searchedText:string;
  function enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
  function readText(text: pchar; textLen: longint):boolean;
end;


function TTempSearchClassText.enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
var i:integer;
begin
  result:=true;
  if (tagNameLen=1) and (tagName^ in ['a','A']) then begin
    for i:=0 to high(properties) do
      with properties[i] do
        if strliequal(name,'href'#0,nameLen,4) then begin
          lastLink:=true;
          lastLinkURL:=properties[i];
          exit;
        end;
    lastLink:=false ;
  end else lastLink:=false;
end;
function TTempSearchClassText.readText(text: pchar; textLen: longint):boolean;
begin
  if lastLink and strlequal(text,@searchedText[1],textLen,length(searchedText)) then begin
    setlength(self.result,lastLinkURL.valueLen);
    move(lastLinkURL.value[0],self.result[1],lastLinkURL.valueLen);
    exit(false);
  end else result:=true;
end;

function findLinkWithText(const html:string;text: string):string;
var temp:TTempSearchClassText;
begin
  temp:=TTempSearchClassText.create;
  temp.result:='';
  temp.lastLink:=false;
  temp.searchedText:=text;

  parseHTML(html,@temp.enterTag,nil,@temp.readText);
  result:=temp.result;
  temp.free;
end;

type TTempSearchClass=class
  result: string;
  tag,prop_to_get,prop_must_match,value: string;
  function enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
end;

function TTempSearchClass.enterTag(tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
var i,j:integer;
begin
  result:=true;
  if strliequal(tagName,@tag[1],tagNameLen,length(tag)) then begin
    //search property prop_must_match with value value
    for i:=0 to high(properties) do
      if strlequal(properties[i].name,@prop_must_match[1],properties[i].nameLen,length(prop_must_match)) and
         strlequal(properties[i].value,@value[1],properties[i].valueLen,length(value)) then begin
        //search property prop_to_get when prop_must_match is found
        for j:=0 to high(properties) do
          if strliequal(properties[j].name,@prop_to_get[1],properties[j].nameLen,length(prop_to_get)) then begin
            //returns it value
            setlength(self.result,properties[j].valueLen);
            move(properties[j].value[0],self.result[1],properties[j].valueLen);
            exit(false);
          end;
        break;
      end;
  end;
end;


function findTagPropertyValueWithProperty(const html:string;tag,prop_to_get,prop_must_match,value: string):string;
var temp:TTempSearchClass;
begin
  temp:=TTempSearchClass.create;
  temp.result:='';
  temp.tag:=tag;
  temp.prop_to_get:=prop_to_get;
  temp.prop_must_match:=prop_must_match;
  temp.value:=value;

  parseHTML(html,@temp.enterTag,nil,nil);
  result:=temp.result;
  temp.free;
end;

function findLinkWithProperty(const html:string;prop,value: string):string;
begin
  result:=findTagPropertyValueWithProperty(html,'a','href',prop,value);
end;
end.

