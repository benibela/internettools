{**
  @abstract This file @bold(doesn't) contain a xml parser.@br
  But you can use the procedure parseXML to parse a xml file stored in a string. (although you should not expect, that this can actually parse the xml data)

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

  TEnterTagEvent=function (tagName: string; properties: TProperties):TParsingResult of object;
  TLeaveTagEvent=function (tagName: string):TParsingResult of object;
  TTextEvent=function (text: string):TParsingResult of object;


//**Perhaps this parses xml data, perhaps it doesn't. Who knows...
//**@param xml The xml data
//**@param enterTag Event to be called when a tag is entered
//**@param leaveTag Event to be called when a tag is leaved
//**@param textRead Event to be called when text between tags is read
//**@param outputEncoding Encoding to be used in the parameters passed to the event
procedure parseXML(xml:string; enterTag:TEnterTagEvent; leaveTag: TLeaveTagEvent; textRead: TTextEvent;
                   outputEncoding: TSystemCodePage);

function getProperty(propertyName: string; properties:TProperties; const def: string = ''):string;
procedure addProperty(propertyName,value: string;var properties:TProperties);
procedure setProperty(propertyName,value: string;var properties:TProperties);
implementation
type

{ THTMLEventHandler }

THTMLEventHandler=class
protected
  function convToStr(p:pchar;l:longint; flags: TTextFlags = []):string;
public
  alreadyReadSomething: boolean;
  enterTag: TEnterTagEvent;
  leaveTag: TLeaveTagEvent;
  textRead: TTextEvent;
  
  fileEncoding,outputEncoding: TSystemCodePage;
  function enterTagEvent (tagName: pchar; tagNameLen: SizeInt; properties: THTMLProperties):TParsingResult;
  function leaveTagEvent(tagName: pchar; tagNameLen: SizeInt):TParsingResult;
  function textEvent(text: pchar; textLen: SizeInt; flags: TTextFlags):TParsingResult;
end;

{ THTMLEventHandler }

function THTMLEventHandler.convToStr(p: pchar; l: longint; flags: TTextFlags = []): string;
begin
  if not (tfCDATA in flags) then
    result:=strDecodeHTMLEntities(p,l,fileEncoding)
   else
    result:=strFromPchar(p,l);
  result:=strChangeEncoding(result,fileEncoding,outputEncoding);
end;

function THTMLEventHandler.enterTagEvent(tagName: pchar; tagNameLen: SizeInt;
  properties: THTMLProperties): TParsingResult;
var xmlProperties: TProperties;
    i:longint;
    tn:string;
begin
  result:=prContinue;
  if not assigned(enterTag) then exit(prContinue);
  setlength(xmlProperties,Length(properties));
  for i := 0 to high(xmlProperties) do begin
    xmlProperties[i].name:=convToStr(properties[i].name,properties[i].nameLen);
    xmlProperties[i].value:=convToStr(properties[i].value,properties[i].valueLen);
  end;
  tn:=convToStr(tagName,tagNameLen);
  if not alreadyReadSomething then begin
    alreadyReadSomething:=true;
    if lowercase(tn)='?xml' then
      fileEncoding:=strEncodingFromName(getProperty('encoding',xmlProperties));
  end;
  result:=enterTag(tn,xmlProperties);
  
end;

function THTMLEventHandler.leaveTagEvent(tagName: pchar; tagNameLen: SizeInt
  ): TParsingResult;
begin
  if not assigned(leaveTag) then exit(prContinue);
  result:=leaveTag(convToStr(tagName,tagNameLen));
end;

function THTMLEventHandler.textEvent(text: pchar; textLen: SizeInt; flags: TTextFlags): TParsingResult;
begin
  result:=prContinue;
  if not assigned(textRead) then exit();
  result:=textRead(convToStr(text,textLen,flags));
end;

procedure parseXML(xml:string; enterTag: TEnterTagEvent; leaveTag: TLeaveTagEvent;
  textRead: TTextEvent;outputEncoding: TSystemCodePage);
var handler: THTMLEventHandler;
begin
  handler:=THTMLEventHandler.Create;
  try
    handler.outputEncoding:=outputEncoding;
    handler.fileEncoding:=CP_NONE;
    handler.enterTag:=enterTag;
    handler.leaveTag:=leaveTag;
    handler.textRead:=textRead;
    parseHTML(xml,@handler.enterTagEvent,@handler.leaveTagEvent,@handler.textEvent);
  finally
    handler.free;
  end;
end;


function getProperty(propertyName: string; properties:TProperties; const def: string = ''):string;
var i:longint;
begin
  propertyName:=LowerCase(propertyName);
  for i:=0 to high(properties) do
    if LowerCase(properties[i].name)=propertyName then
      exit(properties[i].value);
  result:=def;
end;

procedure addProperty(propertyName, value: string; var properties: TProperties);
begin
  SetLength(properties,length(properties)+1);
  properties[high(properties)].name:=propertyName;
  properties[high(properties)].value:=value;
end;

procedure setProperty(propertyName, value: string; var properties: TProperties);
var i:longint;
  propertyNameLW: String;
begin
  propertyNameLW:=LowerCase(propertyName);
  for i:=0 to high(properties) do
    if LowerCase(properties[i].name)=propertyNameLW then begin
      properties[i].value:=value;
      exit;
    end;
  addProperty(propertyName,value,properties);
end;

end.

