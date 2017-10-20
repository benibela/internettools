{**
  @abstract This is a simple html parser

  @author Benito van der Zander (http://www.benibela.de)
}
unit simplehtmlparser;

{$mode objfpc}{$H+}

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
interface

uses
  Classes, SysUtils; 


type
  TParsingOptions = set of (poRespectHTMLCDATAElements, poRespectProcessingInstructions);
  THTMLProperty=record
    name, value: pchar;
    nameLen, valueLen: longint;
  end;
  THTMLProperties=array of THTMLProperty;
  TParsingResult = (prContinue, prStop);
  TEnterTagEvent=function (tagName: pchar; tagNameLen: SizeInt; properties: THTMLProperties):TParsingResult of object;
  TLeaveTagEvent=function (tagName: pchar; tagNameLen: SizeInt):TParsingResult of object;
  TCommentEvent=function (comment: pchar; commentLen: SizeInt):TParsingResult of object;
  TTextFlags = set of (tfCDATA);
  TTextEvent=function (text: pchar; textLen: SizeInt; textFlags: TTextFlags):TParsingResult of object;

  function pcharStartEqual(p1,p2:pchar;l1,l2: SizeInt):boolean;


  //**This parses html data
  //**@param html The html data
  //**@param enterTag Event to be called when a tag is entered
  //**@param leaveTag Event to be called when a tag is leaved
  //**@param textRead Event to be called when text between tags is read
  //**@bold(Notice:) You can pass nil for every callback function and if one of them returns prStop, the parsing is aborted.
  procedure parseHTML(const html:string;
                      enterTagEvent: TEnterTagEvent; leaveTagEvent: TLeaveTagEvent;
                      textEvent: TTextEvent; commentEvent: TCommentEvent = nil);

  //**This parses html/xml data
  //**@param html Input
  //**@param options Set of options to modify the low-level parsing behaviour.  (Set it to [poScriptIsCDATA] for html and [] for xml)
  //**@param enterTag Event to be called when a tag is entered
  //**@param leaveTag Event to be called when a tag is leaved
  //**@param textRead Event to be called when text between tags is read
  //**@param commentEvent Event to be called when a comment is read
  //**@bold(Notice:) You can pass nil for every callback function and if one of them returns prStop, the parsing is aborted.
  procedure parseML(const html:string; const options: TParsingOptions;
                    enterTagEvent: TEnterTagEvent; leaveTagEvent: TLeaveTagEvent;
                    textEvent: TTextEvent;
                    commentEvent: TCommentEvent = nil;
                    processingInstruction: TTextEvent = nil
                    );


  function existPropertyWithValue(propertyName,propertyValue: string; properties:THTMLProperties):boolean;
  function getProperty(propertyName: string; properties:THTMLProperties):string;
  function findLinkWithText(const html:string;text: string):string;
  function findLinkWithProperty(const html:string;prop,value: string):string;
  function findTagPropertyValueWithProperty(const html:string;tag,prop_to_get,prop_must_match,value: string):string;

  function htmlElementIsCDATA(const marker: pchar; const tempLen: integer): boolean;
implementation
uses bbutils;

function pcharStartEqual(p1,p2:pchar;l1,l2: SizeInt):boolean;
begin
  if l1>l2 then l1:=l2
  else l2:=l1;
  result:=strlequal(p1,p2,l1,l2);
end;


procedure parseHTML(const html:string;
                    enterTagEvent: TEnterTagEvent; leaveTagEvent: TLeaveTagEvent;
                    textEvent: TTextEvent; commentEvent: TCommentEvent = nil);
begin
  parseML(html, [poRespectHTMLCDATAElements], enterTagEvent, leaveTagEvent, textEvent, commentEvent);
end;



procedure parseML(const html:string; const options: TParsingOptions;
                    enterTagEvent: TEnterTagEvent; leaveTagEvent: TLeaveTagEvent;
                    textEvent: TTextEvent; commentEvent: TCommentEvent = nil;
                    processingInstruction: TTextEvent = nil);
var pos,marker,htmlEnd,cdataTagStartMarker: pchar;
    valueStart:char;
    tempLen:longint;
    properties:THTMLProperties;
    cdataTag: boolean;
  procedure handleProcessingInstruction;
  begin
    inc(pos);
    marker := pos;
    while (pos < htmlEnd) and ((pos^ <> '?') or ((pos+1)^ <> '>')) do inc(pos);
    if Assigned(processingInstruction) then processingInstruction(marker, pos - marker, []);
    inc(pos, 2);
    marker := pos;
  end;

begin
  if html='' then exit;
  pos:=@html[1];
  htmlEnd:=@html[length(html)];
  marker:=pos;
  cdataTagStartMarker := nil; //hide warning
  while (pos<=htmlEnd) do begin
    case pos^ of
      '<': begin //Start or end of a tag
        if (marker<>pos)and(assigned(textEvent)) then
          if textEvent(marker,pos-marker, []) = prStop then
            exit;

        inc(pos);
        case pos^ of
          '!':begin //comment start
            inc(pos);
            if (pos^ in ['D','d']) and ((pos+1)^ in ['O','o']) and ((pos+2)^ in ['C','c'])
               and ((pos+3)^ in ['T','t']) and ((pos+4)^ in ['Y','y'])and ((pos+5)^ in ['P','p'])
               and ((pos+6)^ in ['E','e'])  then begin//doctype
              while (pos<=htmlEnd) and (pos^ <> '>') do inc(pos);
              inc(pos);
             end else if (pos^ in ['[']) and (strliBeginsWith(pos+1, htmlEnd-pos, 'CDATA[')) then begin //cdata
               pos += 7;
               marker:=pos;
               while (pos <= htmlEnd) and ((pos^ <> ']') or ((pos+1)^ <> ']') or ((pos+2)^ <> '>')) do
                 pos+=1;
               if (marker<>pos) and (Assigned(textEvent)) then
                 if textEvent(marker,pos-marker, [tfCDATA]) = prStop then exit;
               pos += 3;
             end else begin
              marker := pos;
              if (pos^ = '-') and ((pos+1)^ = '-') then marker+=2; //don't pass -- at begin, but also pass <![endif]--> as comment
              while (pos<=htmlEnd) and ((pos^<>'-') or ((pos+1)^<>'-') or ((pos+2)^<>'>')) do
                inc(pos);
              if assigned(commentEvent) and (commentEvent(marker, pos-marker) = prStop) then
                exit;
              inc(pos,3);
             end;
             marker:=pos;
          end;
          '/': begin //tag end
            inc(pos);
            marker:=pos;
            while (pos<=htmlEnd) and not (pos^ in (['>']+WHITE_SPACE)) do inc(pos);
            if assigned(leaveTagEvent) then
              if leaveTagEvent(marker,pos-marker) = prStop then
                exit;
            while (pos<=htmlEnd) and (pos^ <> '>') do inc(pos);
            inc(pos);
            marker:=pos;
          end;
          else if (pos^ = '?') and (poRespectProcessingInstructions in options) then handleProcessingInstruction
          else begin //tag start
            marker:=pos;

            inc(pos);
            setlength(properties,0);
            while (pos<=htmlEnd) and not (pos^ in (['/','>','?']+WHITE_SPACE)) do
              inc(pos);
            tempLen:=pos-marker;
            //read properties
            while (pos<=htmlEnd) and  (pos^ in WHITE_SPACE) do inc(pos);
            while (pos<=htmlEnd)
                   and (pos^ <> '>')
                   and ((pos^ <> '/') or ((pos+1)^ <> '>'))
                   and ((marker^ <> '?') or (pos^ <> '?') or ((pos+1)^ <> '>') )
                   do begin
              if pos>htmlEnd then exit;
              //new property
              setlength(properties,length(properties)+1);
              with properties[high(properties)] do begin
                //search start of name
                name:=pos;
                while (pos<=htmlEnd) and not (pos^ in (WHITE_SPACE + ['=','>'])) and ((pos^ <> '/') or ((pos+1)^ <> '>'))  do inc(pos);
                nameLen:=pos-name;
                //find value start
                while (pos<=htmlEnd) and (pos^ in (WHITE_SPACE)) do inc(pos);
                if pos^ <> '=' then begin
                   value:=name;
                   valueLen:=0; //attribute without value
                end else begin
                  inc(pos);
                  while (pos<=htmlEnd) and (pos^ in (WHITE_SPACE)) do inc(pos);
                  if not (pos^ in ['''','"']) then begin
                    //value not in ""
                    value:=pos;
                    while (pos<=htmlEnd) and not (pos^ in (WHITE_SPACE + ['>'])) do
                      if (pos^='/') and ((pos+1)^='>') then break //it is possible to use unescaped slashs in html attributes
                      else inc(pos);
                    valueLen:=pos-value;
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
              while (pos<=htmlEnd) and  (pos^ in WHITE_SPACE) do inc(pos);
            end;
            //while (marker[tempLen-1] in ['/', ' ']) and (tempLen>0) do
            //  dec(tempLen);
            if assigned(enterTagEvent) then
              if enterTagEvent(marker,tempLen,properties) = prStop then
                exit;

            cdataTag:=false;
            if pos^ = '?' then inc(pos);
            if (pos^ = '/')  then begin
              if assigned(leaveTagEvent) then
                if leaveTagEvent(marker,tempLen) = prStop then
                  exit;
              if pos^ = '/'  then inc(pos);
            end else if poRespectHTMLCDATAElements in options then begin
              cdataTag:=htmlElementIsCDATA(marker, tempLen);
              cdataTagStartMarker := marker;
            end;
            if pos^='>' then inc(pos);
            marker:=pos;
            //parse cdata script tag
            if cdataTag then begin
              while (pos+tempLen<=htmlEnd) and
                    ((pos^<>'<') or ((pos+1)^<>'/')              //check for                   </
                      or not ((pos+2)^ in ['a'..'z','A'..'Z'])   //continued check for         </ [:alpha:]
                      or not ((pos+tempLen+2)^ in [#9,#$A,#$C,' ','/','>'])
                      or not strliEqual(cdataTagStartMarker, pos+2, tempLen) ) do //check for  </script    (or whatever opened that implicit cdata thing)
                inc(pos);
              if Assigned(textEvent) then
                if textEvent(marker, pos-marker, [tfCDATA]) = prStop then
                  exit;
              marker:=pos;
            end;
          end;
        end;
      end;
      else inc(pos);
    end;
  end;
  if (marker<>pos)and(assigned(textEvent)) then
    textEvent(marker,pos-marker, []);
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
  function enterTag(tagName: pchar; tagNameLen: SizeInt; properties: THTMLProperties):TParsingResult;
  function readText(text: pchar; textLen: SizeInt; {%H-}tf: TTextFlags):TParsingResult;
end;


function TTempSearchClassText.enterTag(tagName: pchar; tagNameLen: SizeInt; properties: THTMLProperties):TParsingResult;
var i:integer;
begin
  result:=prContinue;
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
function TTempSearchClassText.readText(text: pchar; textLen: SizeInt; tf: TTextFlags):TParsingResult;
begin
  if lastLink and strlequal(text,@searchedText[1],textLen,length(searchedText)) then begin
    setlength(self.result,lastLinkURL.valueLen);
    move(lastLinkURL.value[0],self.result[1],lastLinkURL.valueLen);
    result := prStop;
  end else result:= prContinue;
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
  function enterTag(tagName: pchar; tagNameLen: SizeInt; properties: THTMLProperties):TParsingResult;
end;

function TTempSearchClass.enterTag(tagName: pchar; tagNameLen: SizeInt; properties: THTMLProperties):TParsingResult;
var i,j:SizeInt;
begin
  result:=prContinue;
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
            exit(prStop);
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

function htmlElementIsCDATA(const marker: pchar; const tempLen: integer): boolean;
begin
  //    If the parent of current node is a style, script, xmp, iframe, noembed, noframes, or plaintext element, or if the parent of current node is noscript element
  result := strliequal(marker,'style',tempLen) or strliequal(marker,'script',tempLen) or strliequal(marker,'xmp',tempLen) or strliequal(marker,'iframe',tempLen)
            or strliequal(marker,'noembed',tempLen) or strliequal(marker,'noframes',tempLen) or strliequal(marker,'plaintext',tempLen);
end;

function findLinkWithProperty(const html:string;prop,value: string):string;
begin
  result:=findTagPropertyValueWithProperty(html,'a','href',prop,value);
end;
end.

