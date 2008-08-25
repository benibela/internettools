{**
  @abstract This file @bold(doesn't) contain a xml parser.@br
  But you can use the procedure parseXML to parse a xml file stored in a string. (although you should not expect, that this can actually parse the xml data)

  $Revision$
  @lastmod $Date$
  @author Benito van der Zander (http://www.benibela.de)
*}
unit simplexmlparser;
{
Copyright (C) 2008 Benito van der Zander (BeniBela)
                   benito@benibela.de
                   www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simplehtmlparser,bbutils;

type
  TProperty=record
    name, value: string;
  end;
  TProperties=array of TProperty;

  TEnterTagEvent=function (tagName: string; properties: TProperties):boolean of object;
  TLeaveTagEvent=function (tagName: string):boolean of object;
  TTextEvent=function (text: string):boolean of object;


//**Perhaps this parses xml data, perhaps it doesn't. Who knows...
//**@param xml The xml data
//**@param enterTag Event to be called when a tag is entered
//**@param leaveTag Event to be called when a tag is leaved
//**@param textRead Event to be called when text between tags is read
//**@param outputEncoding Encoding to be used in the parameters passed to the event
procedure parseXML(xml:string; enterTag:TEnterTagEvent; leaveTag: TLeaveTagEvent; textRead: TTextEvent;
                   outputEncoding: TEncoding);

function getProperty(propertyName: string; properties:TProperties):string;
procedure addProperty(propertyName,value: string;var properties:TProperties);
procedure setProperty(propertyName,value: string;var properties:TProperties);
implementation
type

{ THTMLEventHandler }

THTMLEventHandler=class
protected
  function convToStr(p:pchar;l:longint):string;
public
  alreadyReadSomething: boolean;
  enterTag: TEnterTagEvent;
  leaveTag: TLeaveTagEvent;
  textRead: TTextEvent;
  
  fileEncoding,outputEncoding: TEncoding;
  function enterTagEvent (tagName: pchar; tagNameLen: longint; properties: THTMLProperties):boolean;
  function leaveTagEvent(tagName: pchar; tagNameLen: longint):boolean;
  function textEvent(text: pchar; textLen: longint):boolean;
end;

{ THTMLEventHandler }

function THTMLEventHandler.convToStr(p: pchar; l: longint): string;
begin
  result:=decodeHTMLEntities(p,l,fileEncoding);
  result:=changeEncoding(result,fileEncoding,outputEncoding);
end;

function THTMLEventHandler.enterTagEvent(tagName: pchar; tagNameLen: longint;
  properties: THTMLProperties): boolean;
var xmlProperties: TProperties;
    i:longint;
    tn:string;
begin
  result:=true;
  if not assigned(enterTag) then exit(true);
  setlength(xmlProperties,Length(properties));
  for i := 0 to high(xmlProperties) do begin
    xmlProperties[i].name:=convToStr(properties[i].name,properties[i].nameLen);
    xmlProperties[i].value:=convToStr(properties[i].value,properties[i].valueLen);
  end;
  tn:=convToStr(tagName,tagNameLen);
  if not alreadyReadSomething then begin
    alreadyReadSomething:=true;
    if lowercase(tn)='?xml' then
      fileEncoding:=nameToEncoding(getProperty('encoding',xmlProperties));
  end;
  result:=enterTag(tn,xmlProperties);
  
end;

function THTMLEventHandler.leaveTagEvent(tagName: pchar; tagNameLen: longint
  ): boolean;
begin
  if not assigned(leaveTag) then exit(true);
  result:=leaveTag(convToStr(tagName,tagNameLen));
end;

function THTMLEventHandler.textEvent(text: pchar; textLen: longint): boolean;
begin
  result:=true;
  if not assigned(textRead) then exit(true);
  result:=textRead(convToStr(text,textLen));
end;

procedure parseXML(xml:string; enterTag: TEnterTagEvent; leaveTag: TLeaveTagEvent;
  textRead: TTextEvent;outputEncoding: TEncoding);
var handler: THTMLEventHandler;
begin
  handler:=THTMLEventHandler.Create;
  handler.outputEncoding:=outputEncoding;
  handler.fileEncoding:=eUnknown;
  handler.enterTag:=enterTag;
  handler.leaveTag:=leaveTag;
  handler.textRead:=textRead;
  parseHTML(xml,@handler.enterTagEvent,@handler.leaveTagEvent,@handler.textEvent);
  handler.free;
end;


function getProperty(propertyName: string; properties:TProperties):string;
var i:longint;
begin
  propertyName:=LowerCase(propertyName);
  for i:=0 to high(properties) do
    if LowerCase(properties[i].name)=propertyName then
      exit(properties[i].value);
  result:='';
end;

procedure addProperty(propertyName, value: string; var properties: TProperties);
begin
  SetLength(properties,length(properties)+1);
  properties[high(properties)].name:=propertyName;
  properties[high(properties)].value:=value;
end;

procedure setProperty(propertyName, value: string; var properties: TProperties);
var i:longint;
begin
  propertyName:=LowerCase(propertyName);
  for i:=0 to high(properties) do
    if LowerCase(properties[i].name)=propertyName then begin
      properties[i].value:=value;
      exit;
    end;
  addProperty(propertyName,value,properties);
end;

end.

