{Copyright (C) 2006  Benito van der Zander

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
unit internetAccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
type
  PInternetConfig=^TInternetConfig;
  TInternetConfig=record
    userAgent: string;
    tryDefaultConfig, useProxy: boolean;
    proxyHTTPName, proxyHTTPPort: string;
    proxyHTTPSName, proxyHTTPSPort: string;

    connectionCheckPage: string;
  end;
  { TCustomInternetAccess }

  { TInternetAccess }

  TProgressEvent=procedure (sender: TObject; progress,maxprogress: longint) of object;
  TInternetAccess=class
//  protected
  public
    internetConfig: PInternetConfig;
    cookies: array of record
      name, value:string;
    end;
    procedure setCookie(name,value:string);
    procedure parseHeaderForCookies(header: string);
    function makeCookieHeader:string;
  public
    constructor create();
    function post(totalUrl: string;data:string):string;
    function post(protocol,host,url: string;data:string):string;virtual;abstract;
    procedure get(totalUrl: string;stream:TStream;progressEvent:TProgressEvent=nil);
    function get(totalUrl: string;progressEvent:TProgressEvent=nil):string;
    procedure get(protocol,host,url: string;stream:TStream;progressEvent:TProgressEvent=nil);
    function get(protocol,host,url: string;progressEvent:TProgressEvent=nil):string;virtual;abstract;
    function existsConnection():boolean;virtual;
    function needConnection():boolean;virtual;abstract;
    procedure closeOpenedConnections();virtual;abstract;
    function urlEncodeData(data: string): string;
  end;
  EInternetException=class(Exception)
    details:string;
  end;
  TInternetAccessClass=class of TInternetAccess;

var defaultInternetConfiguration: TInternetConfig;
implementation
uses bbutils,bbdebugtools;
//==============================================================================
//                            TInternetAccess
//==============================================================================
procedure decodeURL(const totalURL: string; out protocol, host, url: string);
var slash,points: integer;
begin
  url:=totalURL;
  protocol:=copy(url,1,pos('://',totalURL)-1);
  delete(url,1,length(protocol)+3);
  slash:=pos('/',url);
  points:=pos(':',url);
  if (points=0) or (points>slash) then points:=slash
  else
    case strToInt(copy(url,points+1,slash-points-1)) of
      80: if protocol<>'http' then
            raise EInternetException.create('Protocol value (80) doesn''t match protocol name ('+protocol+')'#13#10'URL: '+totalURL);
      443: if protocol<>'https' then
            raise EInternetException.create('Protocol value (443) doesn''t match protocol name  ('+protocol+')'#13#10'URL: '+totalURL);
      else raise EInternetException.create('Unknown protocol');
    end;
  host:=copy(url,1,points-1);
  delete(url,1,slash-1);
end;

procedure TInternetAccess.setCookie(name, value: string);
var i:longint;
begin
  for i:=0 to high(cookies) do
    if SameText(cookies[i].name,name) then begin
      cookies[i].value:=value;
      exit;
    end;
  setlength(cookies,length(cookies)+1);
  cookies[high(cookies)].name:=name;
  cookies[high(cookies)].value:=value;
end;

procedure TInternetAccess.parseHeaderForCookies(header: string);
var i,mark:longint;
    name,value:string;
begin
  i:=1;
  while i<length(header) do begin
    if strlibeginswith(@header[i],length(header)-i,'Set-Cookie:') then begin
      i+=length('Set-Cookie:');
      //Name getrimmt finden
      while header[i] = ' ' do i+=1;
      mark:=i;
      while not (header[i] in ['=',' ',#0]) do i+=1;
      name:=copy(header,mark,i-mark);

      //Wert finden
      while not (header[i] in ['=',#0]) do i+=1;
      i+=1;
      mark:=i;
      if header[i]='"' then begin//quoted-str allowed??
        i+=1;
        while not (header[i] in ['"', #0]) do i+=1;
        i+=1;
      end else
        while not (header[i] in [';', #0]) do i+=1;
      value:=copy(header,mark,i-mark);

      setCookie(name,value);

    end else i+=1;
  end;
end;

function TInternetAccess.makeCookieHeader: string;
var i:longint;
begin
  result:='';
  if length(cookies)=0 then exit;
  result:='Cookie: '+cookies[0].name+'='+cookies[0].value;
  for i:=1 to high(cookies) do
    result+='; '+cookies[i].name+'='+cookies[i].value;
  result+=#13#10;
end;

constructor TInternetAccess.create();
begin
  raise eabstracterror.create('Abstract internet class created (TInternetAccess)');
end;

function TInternetAccess.post(totalUrl: string;data:string):string;
var protocol, host, url: string;
begin
  decodeURL(totalUrl,protocol,host,url);
  result:=post(protocol,host,url,data);
end;

procedure TInternetAccess.get(totalUrl: string; stream: TStream;progressEvent:TProgressEvent=nil);
var buffer:string;
begin
  assert(stream<>nil);
  buffer:=get(totalUrl,progressEvent);
  stream.WriteBuffer(buffer[1],sizeof(buffer[1])*length(buffer));
end;

function TInternetAccess.get(totalUrl: string;progressEvent:TProgressEvent=nil):string;
var protocol, host, url: string;
begin
  decodeURL(totalUrl,protocol,host,url);
  result:=get(protocol,host,url,progressEvent);
end;

procedure TInternetAccess.get(protocol, host, url: string; stream: TStream;progressEvent:TProgressEvent=nil);
var buffer:string;
begin
  assert(stream<>nil);
  buffer:=get(protocol,host,url,progressEvent);
  stream.WriteBuffer(buffer[1],sizeof(buffer[1])*length(buffer));
end;


function TInternetAccess.existsConnection(): boolean;
begin
  result:=false;
  try
    if (internetConfig=nil) or (internetConfig^.connectionCheckPage='') then
      result:=get('http','www.google.de','/')<>''
     else
      result:=get('http',internetConfig^.connectionCheckPage,'/')<>'';
  except
  end;
end;

function TInternetAccess.urlEncodeData(data: string): string;
const ENCODE_TABLE:array[1..19,0..1] of string=(('%','%25'),
                                               (#9,'%09'), //tab
                                               (#10,'%0A'),//new line and carriage return (13,10)
                                               (#13,'%0D'),
                                               (' ','%20'),
                                               ('"','%22'),
                                               ('<','%3C'),
                                               ('>','%3E'),
                                               ('#','%23'),
                                               ('$','%24'),
                                               ('&','%26'),
                                               ('+','%2B'),
                                               (',','%2C'),
                                               ('/','%2F'),
                                               (':','%3A'),
                                               (';','%3B'),
                                               ('=','%3D'),
                                               ('?','%3F'),
                                               ('@','%40'));
var i:integer;
begin
  result:=data;
  for i:=low(ENCODE_TABLE) to high(ENCODE_TABLE) do
    result:=StringReplace(result,ENCODE_TABLE[i,0],ENCODE_TABLE[i,1],[rfReplaceAll]);
end;
end.

