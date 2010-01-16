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
{** @abstract(You can use this unit to configure and create internet connections)

    In the moment it only supports http/s connections, but perhaps this will change
    in the future (e.g. to also support ftp)}
unit internetaccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
type
  PInternetConfig=^TInternetConfig;
  //**@abstract(Internet configuration)
  //**You don't have to set it, but the user would prefer to have those options
  TInternetConfig=record
    userAgent: string; //**< the user agent used when connecting
    tryDefaultConfig: boolean; //**< should the system default configuration be used (not always supported, currently it only works with wininet)
    useProxy: Boolean; //**< should a proxy be used
    proxyHTTPName, proxyHTTPPort: string; //**< proxy used for http
    proxyHTTPSName, proxyHTTPSPort: string; //**< proxy used for https (not always supported, currently only with wininet)

    connectionCheckPage: string; //**< url we should open to check if an internet connection exists (e.g. http://google.de)
  end;
  { TCustomInternetAccess }

  { TInternetAccess }
  //**Event to monitor the progress of a download (measured in bytes)
  TProgressEvent=procedure (sender: TObject; progress,maxprogress: longint) of object;
  //**@abstract(Abstract base class for connections)
  //**There are two child classes TW32InternetAccess and TSynapseInternetAccess which
  //**you should assign once to defaultInternetAccessClass and then use this class
  //**variable@br
  //**If a transfer fails it will raise a EInternetException
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
    constructor create();virtual;
    //**post the (url encoded) data to the given url and returns the resulting document
    //**as string
    function post(totalUrl: string;data:string):string;
    //**post the (url encoded) data to the url given as three parts and returns the page as string
    //** (override this if you want to sub class it)
    function post(protocol,host,url: string;data:string):string;virtual;abstract;
    //**get the url as stream and optionally monitors the progress with a progressEvent
    procedure get(totalUrl: string;stream:TStream;progressEvent:TProgressEvent=nil);
    //**get the url as string and optionally monitors the progress with a progressEvent
    function get(totalUrl: string;progressEvent:TProgressEvent=nil):string;
    //**get the url as stream and optionally monitors the progress with a progressEvent
    procedure get(protocol,host,url: string;stream:TStream;progressEvent:TProgressEvent=nil);
    //**get the url as string and optionally monitors the progress with a progressEvent
    function get(protocol,host,url: string;progressEvent:TProgressEvent=nil):string;virtual;abstract;
    //**checks if an internet connection exists
    function existsConnection():boolean;virtual;
    //**call this to open a connection (very unreliable). It will return true on success
    function needConnection():boolean;virtual;abstract;
    //**Should close all connections (doesn't work)
    procedure closeOpenedConnections();virtual;abstract;
    //**Encodes the passed string in the url encoded format
    function urlEncodeData(data: string): string;
  end;
  EInternetException=class(Exception)
    details:string;
  end;
  TInternetAccessClass=class of TInternetAccess;

var defaultInternetConfiguration: TInternetConfig; //**< default configuration, used by all our classes
    defaultInternetAccessClass:TInternetAccessClass; //**< default internet access, here you can store which internet library the program should use
implementation
uses bbutils;
//==============================================================================
//                            TInternetAccess
//==============================================================================
procedure decodeURL(const totalURL: string; out protocol, host, url: string);
var slash,points: integer;
    port:string;
begin
  url:=totalURL;
  protocol:=copy(url,1,pos('://',totalURL)-1);
  delete(url,1,length(protocol)+3);
  slash:=pos('/',url);
  points:=pos(':',url);
  if (points=0) or (points>slash) then points:=slash
  else begin
    port:=copy(url,points+1,slash-points-1);
    case strToInt(port) of
      80,8080: begin
        if protocol<>'http' then
            raise EInternetException.create('Protocol value ('+port+') doesn''t match protocol name ('+protocol+')'#13#10'URL: '+totalURL);
        if port<>'80' then
            points:=slash; //keep non standard port
      end;
      443: if protocol<>'https' then
            raise EInternetException.create('Protocol value (443) doesn''t match protocol name  ('+protocol+')'#13#10'URL: '+totalURL);
      else raise EInternetException.create('Unknown port in '+totalURL);
    end;
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
