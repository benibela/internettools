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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils; 
type
  PInternetConfig=^TInternetConfig;
  //**@abstract(Internet configuration)
  //**You don't have to set it, but the user would prefer to have those options

  { TInternetConfig }

  TInternetConfig=record
    userAgent: string; //**< the user agent used when connecting
    tryDefaultConfig: boolean; //**< should the system default configuration be used (not always supported, currently it only works with wininet)
    useProxy: Boolean; //**< should a proxy be used
    proxyHTTPName, proxyHTTPPort: string; //**< proxy used for http
    proxyHTTPSName, proxyHTTPSPort: string; //**< proxy used for https (not always supported, currently only with wininet)

    connectionCheckPage: string; //**< url we should open to check if an internet connection exists (e.g. http://google.de)

    logToPath: string;

    procedure setProxy(proxy: string);
  end;
  { TCustomInternetAccess }

  { TInternetAccess }
  THTTPConnectMethod=(hcmGet, hcmPost);
  //**Event to monitor the progress of a download (measured in bytes)
  TProgressEvent=procedure (sender: TObject; progress,maxprogress: longint) of object;
  //**Event to intercept transfers end/start
  TTransferStartEvent=procedure (sender: TObject; var method: THTTPConnectMethod; var protocol,host,url, data:string) of object;
  TTransferEndEvent=procedure (sender: TObject; method: THTTPConnectMethod; protocol,host,url, data:string; var result: string) of object;
  //**@abstract(Abstract base class for connections)
  //**There are two child classes TW32InternetAccess and TSynapseInternetAccess which
  //**you should assign once to defaultInternetAccessClass and then use this class
  //**variable@br
  //**If a transfer fails it will raise a EInternetException
  TInternetAccess=class
  private
    FOnTransferEnd: TTransferEndEvent;
    FOnTransferStart: TTransferStartEvent;
    function transfer(method: THTTPConnectMethod; protocol,host,url, data:string):string;
  protected
    FOnProgress:TProgressEvent;
    function doTransfer(method: THTTPConnectMethod; protocol,host,url, data:string):string;virtual;abstract;
    function GetLastHTTPHeaders: TStringList; virtual; abstract;
  protected
    //** Cookies receive from/to-send the server (only for backends that does not support cookies natively (i.e.. win32). Synapse has its own cookies)
    cookies: array of record
      name, value:string;
    end;
    procedure setCookie(name,value:string);
    procedure parseHeaderForCookies(header: string);
    function makeCookieHeader:string;
  public
    class function parseHeaderForLocation(header: string): string; static;
  public
    //in
    internetConfig: PInternetConfig;
    additionalHeaders: TStringList; //**< Defines additional headers that should be send to the server
  public
    //out
    lastHTTPResultCode: longint;    //**< HTTP Status code of the last request
    property lastHTTPHeaders: TStringList read GetLastHTTPHeaders; //**< HTTP headers received by the last request
    function getLastHTTPHeader(header: string): string; //**< Reads a certain HTTP header received by the last request
  public
    constructor create();virtual;
    //**post the (url encoded) data to the given url and returns the resulting document
    //**as string
    function post(totalUrl: string; data:string):string;
    //**post the (url encoded) data to the url given as three parts and returns the page as string
    //** (override this if you want to sub class it)
    function post(protocol,host,url: string; data:string):string;
    //**get the url as stream and optionally monitors the progress with a progressEvent
    procedure get(totalUrl: string; stream:TStream);
    //**get the url as string and optionally monitors the progress with a progressEvent
    function get(totalUrl: string):string;
    //**get the url as stream and optionally monitors the progress with a progressEvent
    procedure get(protocol,host,url: string; stream:TStream);
    //**get the url as string and optionally monitors the progress with a progressEvent
    function get(protocol,host,url: string):string;
    //**checks if an internet connection exists
    function existsConnection():boolean;virtual;
    //**call this to open a connection (very unreliable). It will return true on success
    function needConnection():boolean;virtual;abstract;
    //**Should close all connections (doesn't work)
    procedure closeOpenedConnections();virtual;abstract;
    //**Encodes the passed string in the url encoded format
    class function urlEncodeData(data: string): string;
    //**Encodes all var=... pairs of data in the url encoded format
    class function urlEncodeData(data: TStringList): string;

    function internalHandle: TObject; virtual; abstract;
  published
    property OnTransferStart: TTransferStartEvent read FOnTransferStart write FOnTransferStart;
    property OnTransferEnd: TTransferEndEvent read FOnTransferEnd write FOnTransferEnd;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;
  EInternetException=class(Exception)
    details:string;
  end;
  TInternetAccessClass=class of TInternetAccess;


procedure decodeURL(const totalURL: string; out protocol, host, url: string);

type TRetrieveType = (rtEmpty, rtRemoteURL, rtFile, rtXML);

(***
  Guesses the type of given string@br@br

  E.g. for 'http://' it returns rtRemoteURL, for '/tmp' rtFile and for '<abc/>' rtXML@br.
  Internally used by simpleinternet.retrieve to determine how to actually retrieve the data.
*)
function guessType(const data: string): TRetrieveType;


var defaultInternetConfiguration: TInternetConfig; //**< default configuration, used by all our classes
  defaultInternetAccessClass:TInternetAccessClass = nil; //**< default internet access, here you can store which internet library the program should use
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
  protocol:=copy(url,1,pos('://',url)-1);
  delete(url,1,length(protocol)+3);
  slash:=pos('/',url);
  if slash = 0 then slash := length(url) + 1;
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
  if url = '' then url := '/';
end;

function guessType(const data: string): TRetrieveType;
var trimmed: string;
begin
  trimmed:=TrimLeft(data);
  if trimmed = '' then exit(rtEmpty);

  if strBeginsWith(trimmed, 'http://') or strBeginsWith(trimmed, 'https://') then
    exit(rtRemoteURL);

  if strBeginsWith(trimmed, 'file://') then
    exit(rtFile);

  if strBeginsWith(trimmed, '<') then
    exit(rtXML);

  exit(rtFile);
end;


procedure saveAbleURL(var url:string);
var temp:integer;
begin
  for temp:=1 to length(url) do
    if url[temp] in ['/','?','&',':','\','*','"','<','>','|'] then
      url[temp]:='#';
end;

procedure writeString(dir,url,value: string);
var tempdebug:TFileStream;
begin
  saveAbleURL(url);
  url:=copy(url,1,200); //cut it of, or it won't work on old Windows with large data
  url:=dir+url;
  try
  if fileexists(url) then
    tempdebug:=TFileStream.create(url+'_____'+inttostr(random(99999999)),fmCreate)
   else
    tempdebug:=TFileStream.create(Utf8ToAnsi(url),fmCreate);
  if value<>'' then
    tempdebug.writebuffer(value[1],length(value))
   else
    tempdebug.Write('empty',5);
  tempdebug.free;
  except
  end;
end;

{ TInternetConfig }

procedure TInternetConfig.setProxy(proxy: string);
var
  portPos: SizeInt;
  port: String;
begin
  proxy:=trim(proxy);;
  if proxy='' then begin
    useProxy:=false;
    exit;
  end;
  portPos := pos(':', proxy);
  port := copy(proxy,portPos+1, length(proxy));
  if portPos > 0 then proxy := copy(proxy,1,portPos-1)
  else port := '8080';

  proxyHTTPName:=proxy;
  proxyHTTPSName:=proxy;
  proxyHTTPPort:=port;
  proxyHTTPSPort:=port;
  useProxy:=true;
end;



function TInternetAccess.transfer(method: THTTPConnectMethod; protocol, host, url, data: string):string;
begin
  if internetConfig=nil then raise Exception.create('No internet configuration set');
  if assigned(FOnTransferStart) then
    FOnTransferStart(self, method, protocol, host, url, data);
  result:=doTransfer(method,protocol,host,url,data);
  if internetConfig^.logToPath<>'' then
    writeString(internetConfig^.logToPath, protocol+'://'+host+url+'<-DATA:'+data,result);
  if assigned(FOnTransferEnd) then
    FOnTransferEnd(self, method, protocol, host, url, data, Result);
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
  //log('Header: ');  log(header);
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
        while not (header[i] in [';', #13, #10,#0]) do i+=1;
      value:=copy(header,mark,i-mark);

      setCookie(name,value);

      //log('=>Cookie'+name);      log('value:'+value);
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

class function TInternetAccess.parseHeaderForLocation(header: string): string;
var i,mark:longint;
begin
  //log('PHFL Header: ');  log(header);
  i:=1;
  while i<length(header) do begin
    if strlibeginswith(@header[i],length(header)-i,'Location:') then begin
      i+=length('Location:');
      //Name getrimmt finden
      while header[i] = ' ' do i+=1;

      mark:=i;
      while not (header[i] in [#13,#10]) do i+=1;
      result:=copy(header,mark,i-mark);

      //log('====> Redirection to:'+result);
      exit;
    end;
    while (i<=length(header)) and not (header[i] in [#13,#10]) do i+=1;
    if (i <= length(header)) and (header[i] = #13) then i+=1;
    if (i <= length(header)) and (header[i] = #10) then i+=1;
  end;
end;

function TInternetAccess.getLastHTTPHeader(header: string): string;
var
  headers: TStringList;
  i: Integer;
begin
  header := header + ':';
  headers := GetLastHTTPHeaders;
  for i:= 0 to headers.count - 1 do
    if striBeginsWith(headers[i], header) then
      exit(trim(strCopyFrom(headers[i], length(header) + 1)));
  exit('');
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

function TInternetAccess.post(protocol, host, url: string; data: string
  ): string;
begin
  result:=transfer(hcmPost,protocol,host,url,data);
end;

procedure TInternetAccess.get(totalUrl: string; stream: TStream);
var buffer:string;
begin
  assert(stream<>nil);
  buffer:=get(totalUrl);
  stream.WriteBuffer(buffer[1],sizeof(buffer[1])*length(buffer));
end;

function TInternetAccess.get(totalUrl: string):string;
var protocol, host, url: string;
begin
  decodeURL(totalUrl,protocol,host,url);
  result:=get(protocol,host,url);
end;

procedure TInternetAccess.get(protocol, host, url: string; stream: TStream);
var buffer:string;
begin
  assert(stream<>nil);
  buffer:=get(protocol,host,url);
  stream.WriteBuffer(buffer[1],sizeof(buffer[1])*length(buffer));
end;

function TInternetAccess.get(protocol, host, url: string): string;
begin
  result:=transfer(hcmGet, protocol, host, url, '');
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

class function TInternetAccess.urlEncodeData(data: string): string;
const ENCODE_TABLE:array[1..19,0..1] of string=(('%','%25'),
                                               (#9,'%09'), //tab
                                               (#10,'%0A'),//new line and carriage return (13,10)
                                               (#13,'%0D'),
                                               ('"','%22'),
                                               ('<','%3C'),
                                               ('>','%3E'),
                                               ('#','%23'),
                                               ('$','%24'),
                                               ('&','%26'),
                                               ('+','%2B'),
//                                               (' ','%20'),
                                               (' ','+'),
                                               (',','%2C'),
                                               ('/','%2F'),
                                               (':','%3A'),
                                               (';','%3B'),
                                               ('=','%3D'),
                                               ('?','%3F'),
                                               ('@','%40')
(*                                               ('ü', '%FC'),
                                               ('ö', '%F6'),
                                               ('ä', '%E4'),
                                               ('Ü', '%DC'),
                                               ('Ö', '%D6'),
                                               ('Ä', '%C4')*)
                                               );
var i:integer;
begin
  result:=data;
  for i:=low(ENCODE_TABLE) to high(ENCODE_TABLE) do
    result:=StringReplace(result,ENCODE_TABLE[i,0],ENCODE_TABLE[i,1],[rfReplaceAll]);
end;

class function TInternetAccess.urlEncodeData(data: TStringList): string;
var
 i: Integer;
begin
  Result:='';
  for i:=0 to data.Count-1 do begin
    if result <> '' then result+='&';
    result+=urlEncodeData(data.Names[i])+'='+urlEncodeData(data.ValueFromIndex[i]);
  end;
end;

end.
