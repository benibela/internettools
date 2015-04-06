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
  Classes, SysUtils, bbutils;
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
    proxySOCKSName, proxySOCKSPort: string; //**< socks proxy

    connectionCheckPage: string; //**< url we should open to check if an internet connection exists (e.g. http://google.de)

    checkSSLCertificates: boolean; //**< If ssl certificates should be checked in https connections (currently only for w32internetaccess)

    logToPath: string;

    procedure setProxy(proxy: string);
  end;
  { TDecodedUrl }

  TDecodedUrlParts = set of (dupProtocol, dupUsername, dupPassword, dupHost, dupPort, dupPath, dupParams, dupLinkTarget);
const
  DecodedUrlPartsALL = [dupProtocol, dupUsername, dupPassword, dupHost, dupPort, dupPath, dupParams, dupLinkTarget];
type
  //** @abstract(A record storing a decoded url.)
  //** Use decodeUrl to create it.@br
  //** It only splits the string into parts, so parts that are url encoded (username, password, path, params, linktarget) will still be url encoded. @br
  //** path, params, linktarget include their delimiter, so an empty string denotes the absence of these parts.
  TDecodedUrl = record
    protocol, username, password, host, port, path, params, linktarget: string;
    function combined(use: TDecodedUrlParts = DecodedUrlPartsALL): string;
    function combinedExclude(doNotUse: TDecodedUrlParts = []): string; inline;
    function resolved(rel: string): TDecodedUrl;
    function serverConnectionOnly: TDecodedUrl;
    procedure prepareSelfForRequest(const lastConnectedURL: TDecodedUrl);
  end;

  { TMIMEMultipartData }

  TMIMEMultipartSubData = record
    data: string;
    headers: TStringArray;
  end;
  PMIMEMultipartSubData = ^TMIMEMultipartSubData;

  TMIMEMultipartData = record //encodes the data corresponding to RFC 1341 (preliminary)
    data: array of TMIMEMultipartSubData;
    function getFormDataIndex(const name: string): integer;
    procedure add(const sdata: string; const headers: string = '');
    procedure addFormData(const name, sdata: string; headers: string = '');
    procedure addFormDataFile(const name, filename: string; headers: string = '');
    procedure addFormData(const name, sdata, filename, contenttype, headers: string);
    function compose(out boundary: string; boundaryHint: string = '4g=Y+CxK-.y7=-B-=X'): string;
    procedure parse(sdata, boundary: string);
    procedure clear;
  end;

  { TCustomInternetAccess }

  { TInternetAccess }
  //**Event to monitor the progress of a download (measured in bytes)
  TProgressEvent=procedure (sender: TObject; progress,maxprogress: longint) of object;
  //**Event to intercept transfers end/start
  TTransferStartEvent=procedure (sender: TObject; var method: string; var url: TDecodedUrl; var data:string) of object;
  TTransferEndEvent=procedure (sender: TObject; method: string; var url: TDecodedUrl; data:string; var result: string) of object;
  //**@abstract(Abstract base class for connections)
  //**This class defines the interface methods for http requests, like get, post or request.@br
  //**If a method fails, it will raise a EInternetException@br@br
  //**Since this is an abstract class, you cannot use it directly, but need one of the implementing child classes
  //**TW32InternetAccess, TSynapseInternetAccess, TAndroidInternetAccess or TMockInternetAccess. @br
  //**The recommended usage is to assign one of the child classes to defaultInternetAccessClass and
  //**then create an actual internet access class with @code(defaultInternetAccessClass.create()). @br
  //**Then it is trivial to swap between different implementations on different platforms, and the depending units
  //**(e.g. simpleinternet or xquery) will use the implementation you have choosen.
  TInternetAccess=class
  private
    FOnTransferEnd: TTransferEndEvent;
    FOnTransferStart: TTransferStartEvent;
  protected
    FOnProgress:TProgressEvent;
    function doTransfer(method: string; const url: TDecodedUrl;  data:string):string;virtual;abstract;
    function GetLastHTTPHeaders: TStringList; virtual; abstract;
  protected
    //** Cookies receive from/to-send the server (only for backends that does not support cookies natively (i.e.. win32). Synapse has its own cookies)
    cookies: array of record
      name, value:string;
    end;
    procedure setCookie(name,value:string);
    procedure parseHeaderForCookies(header: string);
    function makeCookieHeader:string;
    procedure init; //constructor, since .create is "abstract" and can not be called
  public
    class function parseHeaderForLocation(header: string): string; static;
  public
    //in
    internetConfig: PInternetConfig;
    additionalHeaders: TStringList; //**< Defines additional headers that should be send to the server
    ContentTypeForData: string; //**< Defines the Content-Type that is used to transmit data. Usually @code(application/x-www-form-urlencoded) or @code(multipart/form-data; boundary=...). @br This is overriden by a Content-Type set in additionalHeaders.
    multipartFormData: TMIMEMultipartData;
    function getFinalMultipartFormData: string;
  public
    //out
    lastHTTPResultCode: longint;    //**< HTTP Status code of the last request
    lastUrl: String;
    property lastHTTPHeaders: TStringList read GetLastHTTPHeaders; //**< HTTP headers received by the last request
    function getLastHTTPHeader(header: string): string; //**< Reads a certain HTTP header received by the last request
    function getLastContentType: string; //**< Same as getLastHTTPHeader('Content-Type') but easier to remember and without magic string
  public
    constructor create();virtual;
    //**post the (url encoded) data to the given url and returns the resulting document
    //**as string
    function post(totalUrl: string; data:string):string;
    //**post the (url encoded) data to the url given as three parts and returns the page as string
    //** (override this if you want to sub class it)
    function post(protocol,host,url: string; data:string):string;
    //**get the url as stream
    procedure get(totalUrl: string; stream:TStream);
    //**get the url as string
    function get(totalUrl: string):string;
    //**get the url as stream
    procedure get(protocol,host,url: string; stream:TStream);
    //**get the url as string
    function get(protocol,host,url: string):string;

    //**performs a http request
    function request(method, fullUrl, data:string):string;
    function request(method, protocol,host,url, data:string):string;
    function request(method: string; url: TDecodedUrl; data:string):string;



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

  { EInternetException }

  EInternetException=class(Exception)
    details:string;
    errorCode: integer;
    constructor create(amessage: string);
    constructor create(amessage: string; aerrorCode: integer);
  end;
  TInternetAccessClass=class of TInternetAccess;



//procedure decodeURL(const totalURL: string; out protocol, host, url: string);
//** Splits a url into parts
//** normalize performs some normalizations (e.g. foo//bar -> foo/bar)
function decodeURL(const totalURL: string; normalize: boolean = true): TDecodedUrl;

type TRetrieveType = (rtEmpty, rtRemoteURL, rtFile, rtXML);

(***
  Guesses the type of given string@br@br

  E.g. for 'http://' it returns rtRemoteURL, for '/tmp' rtFile and for '<abc/>' rtXML@br.
  Internally used by simpleinternet.retrieve to determine how to actually retrieve the data.
*)
function guessType(const data: string): TRetrieveType;


var defaultInternetConfiguration: TInternetConfig; //**< default configuration, used by all our classes
  defaultInternetAccessClass:TInternetAccessClass = nil; //**< default internet access, here you can store which internet library the program should use

const ContentTypeUrlEncoded: string = 'application/x-www-form-urlencoded';
const ContentTypeMultipart: string = 'multipart/form-data'; //; boundary=
implementation

//==============================================================================
//                            TInternetAccess
//==============================================================================
(*procedure decodeURL(const totalURL: string; out protocol, host, url: string);
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
end;      *)

function decodeURL(const totalURL: string; normalize: boolean): TDecodedUrl;
var url: String;
    userPos: SizeInt;
    slashPos: SizeInt;
    p: SizeInt;
    paramPos: SizeInt;
    targetPos: SizeInt;
    nextSep: SizeInt;
    temp: String;
begin
  result.protocol:='http';
  result.port:='';

  url:=totalURL;
  if pos('://', url) > 0 then
    result.protocol := strSplitGet('://', url);

  userPos := pos('@', url);
  slashPos := pos('/', url);
  paramPos := pos('?', url);
  targetPos := pos('#', url);
  nextSep := length(url)+1;
  if ((slashPos <> 0) and (slashPos < nextSep)) then nextSep:=slashPos;
  if ((paramPos <> 0) and (paramPos < nextSep)) then nextSep:=paramPos;
  if ((targetPos <> 0) and (targetPos < nextSep)) then nextSep:=targetPos;

  if (userPos > 0) and ((userPos < nextSep) or (nextSep = 0)) then begin //username:password@...
    nextSep -= userPos;
    result.username := strSplitGet('@', url);
    if strContains(result.username, ':') then begin
      temp:=strSplitGet(':', result.username);
      result.password:=result.username;
      result.username:=temp;
    end;
  end;

  result.host := copy(url, 1, nextSep-1);
  url := strCopyFrom(url, nextSep);

  if strBeginsWith(result.host, '[') then begin  //[::1 IPV6 address]
    delete(result.host, 1, 1);
    p := pos(']', result.host);
    if p > 0 then begin
      result.port:=strCopyFrom(result.host, p+1);
      result.host:=copy(result.host, 1, p-1);
      if strBeginsWith(result.port, ':') then delete(result.port, 1, 1);
    end;
  end else begin //host:port
    p := pos(':', result.host);
    if p > 0 then begin
      result.port:=strCopyFrom(result.host, p+1);
      result.host:=copy(result.host, 1, p-1);
    end;
  end;

  if paramPos > 0 then begin
    result.path := strSplitGet('?', url);
    if targetPos > 0 then begin
      result.params := '?' + strSplitGet('#', url);
      result.linktarget:='#'+url;
    end else result.params := '?' + url;
  end else if targetPos > 0 then begin
    result.path := strSplitGet('#', url);
    result.linktarget:='#'+url;
  end else result.path := url;

  if normalize then begin
    p := 2;
    while (p <= length(result.path)) do
      if (result.path[p] = '/') and (result.path[p-1] = '/') then delete(result.path, p, 1)
      else p += 1;
  end;
end;

function guessType(const data: string): TRetrieveType;
var trimmed: string;
begin
  trimmed:=TrimLeft(data);
  if trimmed = '' then exit(rtEmpty);

  if striBeginsWith(trimmed, 'http://') or striBeginsWith(trimmed, 'https://') then
    exit(rtRemoteURL);

  if striBeginsWith(trimmed, 'file://') then
    exit(rtFile);

  if striBeginsWith(trimmed, '<') then
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

{ TMIMEMultipartData }

procedure TMIMEMultipartData.addFormData(const name, sdata: string; headers: string);
begin
  headers := 'Content-Disposition: form-data; name="'+name+'"' + #13#10 + headers; //todo: name may encoded with [RFC2045]/rfc2047
  add(sdata, headers);
end;

procedure TMIMEMultipartData.addFormDataFile(const name, filename: string; headers: string);
begin
  headers := 'Content-Disposition: form-data; name="'+name+'"; filename="'+filename+'"' + #13#10 + headers; //todo: name may encoded with [RFC2045]/rfc2047; filename may be approximated or encoded with 2045
  add(strLoadFromFileUTF8(filename), headers);
end;

function indexOfHeader(const sl: TStringArray; name: string): integer;
var
  i: Integer;
begin
  name := trim(name) + ':';
  for i:=0 to high(sl) do
    if striBeginsWith(sl[i], name) then
      exit(i);
  exit(-1);
end;

procedure TMIMEMultipartData.addFormData(const name, sdata, filename, contenttype, headers: string);
var
  splittedHeaders: TStringArray;
  disposition: String;
begin
  splittedHeaders := strSplit(headers, #13#10, false);

  if (indexOfHeader(splittedHeaders, 'Content-Type') < 0) and (contenttype <> '') then
    arrayInsert(splittedHeaders, 0, 'Content-Type: ' + contenttype);
  if indexOfHeader(splittedHeaders, 'Content-Disposition') < 0 then begin
    disposition := 'Content-Disposition: form-data; name="'+name+'"';
    if filename <> '' then disposition += '; filename="'+filename+'"'; //todo: name may encoded with [RFC2045]/rfc2047; filename may be approximated or encoded with 2045
    arrayInsert(splittedHeaders, 0, disposition);
  end;

  SetLength(data, length(data) + 1);
  data[high(data)].headers := splittedHeaders;
  data[high(data)].data := sdata;
end;

function TMIMEMultipartData.getFormDataIndex(const name: string): integer;
var
  i,j: Integer;
begin
  for i := 0 to high(data) do
     for j := 0 to high(data[i].headers) do
        if striBeginsWith(data[i].headers[j], 'Content-Disposition:') then
          if name = striBetween(data[i].headers[j], 'name="', '"') then
            exit(i);
  exit(-1);
end;

procedure TMIMEMultipartData.add(const sdata: string; const headers: string);
begin
  SetLength(data, length(data) + 1);
  data[high(data)].headers := strSplit(headers, #13#10, false);
  data[high(data)].data := sdata;
end;

function TMIMEMultipartData.compose(out boundary: string; boundaryHint: string): string;

const ALLOWED_BOUNDARY_CHARS: string = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ''''()+,-./:=?';
var joinedHeaders: TStringArray;
    encodedData: TStringArray;
    i: Integer;
    ok: Boolean;
begin
  SetLength(joinedHeaders, length(data));
  SetLength(encodedData, length(data));
  for i := 0 to high(data) do begin
    joinedHeaders[i] := trim(strJoin(data[i].headers, #13#10)); //trim to remove additional #13#10 at the end
 { todo: this actually breaks it.
   firefox only sets content-type for file, nothing else
      if indexOfHeader(data[i].headers, 'Content-Type') < 0 then begin
      if joinedHeaders[i] <> '' then joinedHeaders[i] += #13#10;
      joinedHeaders[i] += 'Content-Type: application/octet-stream'; //todo: use text for plain text
    end;
    if indexOfHeader(data[i].headers, 'Content-transfer-encoding') < 0 then begin
      if joinedHeaders[i] <> '' then joinedHeaders[i] += #13#10;
      joinedHeaders[i] += 'Content-transfer-encoding: binary'; //todo: binary is not allowed for mails (use base64, or 8-bit with line wrapping)
    end;    }
    encodedData[i] := data[i].data;
  end;


  boundary := boundaryHint;
  repeat
    repeat
      ok := true;
      for i := 0 to high(data) do begin
        ok := ok and not strContains(joinedHeaders[i], boundary) and not strContains(encodedData[i], boundary);
        if not ok then break;
      end;
      if not ok then
        boundary += ALLOWED_BOUNDARY_CHARS[ Random(length(ALLOWED_BOUNDARY_CHARS)) + 1 ];
    until ok or (length(boundary) >= 70 {max length});
    if not ok then begin
      boundary := boundaryHint;
      if length(boundary) > 8 then SetLength(boundary, 8); //if boundary hint has max length, we can only use a (random) prefix
    end;
  until ok;

  result := '';
  for i := 0 to high(data) do begin
    result += #13#10'--' +boundary + #13#10;
    result += joinedHeaders[i] + #13#10;
    result += #13#10; //separator
    result += encodedData[i];
  end;

  result += #13#10'--' + boundary + '--';
end;

procedure TMIMEMultipartData.parse(sdata, boundary: string);
var
  p,q,r: Integer;
begin
  boundary := #13#10'--'+boundary;

  p := 1;
  q := strIndexOf(sdata, boundary, p) + length(boundary);
  while (q + 2 <= length(sdata)) and strBeginsWith(@sdata[q], #13#10) do begin
    r := strIndexOf(sdata, #13#10#13#10, q);
    SetLength(data, length(data) + 1);
    data[high(data)].headers := strSplit(strSlice(sdata, q+2, r-1), #13#10, false);
    p := r + 4;
    q := strIndexOf(sdata, boundary, p);
    data[high(data)].data := strSlice(sdata, p,  q- 1);
    q += length(boundary);
  end;

end;

procedure TMIMEMultipartData.clear;
begin
  setlength(data, 0);
end;

{ EInternetException }

constructor EInternetException.create(amessage: string);
begin
  inherited Create(amessage);
  errorCode:=-1;
end;

constructor EInternetException.create(amessage: string; aerrorCode: integer);
begin
  inherited Create(amessage);
  Self.errorCode:=aerrorCode;
end;

{ TDecodedUrl }

function TDecodedUrl.combined(use: TDecodedUrlParts): string;
begin
  result := '';
  if (dupProtocol in use) and (protocol <> '') then result += protocol+'://';
  if (dupUsername in use) and (username <> '') then begin
    result += username;
    if (dupPassword in use) and (password <> '') then result += ':'+password;
    Result+='@';
  end;
  if dupHost in use then begin
    if strContains(host, ':') then result += '['+host+']'
    else result += host;
    if (dupPort in use) and (port <> '') then result += ':'+port;
  end;
  if dupPath in use then result += path;
  if dupParams in use then result += params;
  if dupLinkTarget in use then result += linktarget;
end;

function TDecodedUrl.combinedExclude(doNotUse: TDecodedUrlParts): string;
begin
  result := combined(DecodedUrlPartsALL - doNotUse);
end;

function TDecodedUrl.resolved(rel: string): TDecodedUrl;
begin
  if (pos('://',rel) > 0) then result := decodeURL(rel)
  else result := decodeURL(strResolveURI(rel, combined));
end;

function TDecodedUrl.serverConnectionOnly: TDecodedUrl;
begin
  result.protocol := protocol;
  result.username := username;
  result.password := password;
  result.host := host;
  result.port := port;
end;

procedure TDecodedUrl.prepareSelfForRequest(const lastConnectedURL: TDecodedUrl);
begin
  if path = '' then path:='/';
  if (lastConnectedUrl.username <> '') and (lastConnectedUrl.password <> '')
     and (username = '') and (password = '')
     and (lastConnectedUrl.host = host) and (lastConnectedUrl.port = port) and (lastConnectedUrl.protocol = protocol) then begin
    //remember username/password from last connection (=> allows to follows urls within passwort protected areas)
    username := lastConnectedUrl.username;
    password := lastConnectedUrl.password;
  end;
  linktarget := '';
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



function TInternetAccess.request(method, protocol, host, url, data: string):string;
begin
  if not strBeginsWith(url, '/') then url := '/' + url;
  result := request(method, protocol+'://'+host+url,data);
end;

function TInternetAccess.request(method, fullUrl, data: string): string;
begin
  result := request(method, decodeURL(fullUrl), data);
end;

function TInternetAccess.request(method: string; url: TDecodedUrl; data: string): string;
begin
  if internetConfig=nil then raise Exception.create('No internet configuration set');
  if assigned(FOnTransferStart) then
    FOnTransferStart(self, method, url, data);
  result:=doTransfer(method,url,data);
  if internetConfig^.logToPath<>'' then
    writeString(internetConfig^.logToPath, url.combined+'<-DATA:'+data,result);
  if assigned(FOnTransferEnd) then
    FOnTransferEnd(self, method, url, data, Result);
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

procedure TInternetAccess.init;
begin
  internetConfig:=@defaultInternetConfiguration;
  if defaultInternetConfiguration.userAgent='' then
    defaultInternetConfiguration.userAgent:='Mozilla/3.0 (compatible)';

  additionalHeaders := TStringList.Create;
  additionalHeaders.nameValueSeparator := ':';

  ContentTypeForData := ContentTypeUrlEncoded;
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

function TInternetAccess.getFinalMultipartFormData: string;
var
  boundary: String;
begin
  boundary := '';
  result := multipartFormData.compose(boundary);
  ContentTypeForData := ContentTypeMultipart + '; boundary=' + boundary;
  multipartFormData.clear;
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

function TInternetAccess.getLastContentType: string;
begin
  result := getLastHTTPHeader('Content-Type');
end;

constructor TInternetAccess.create();
begin
  raise eabstracterror.create('Abstract internet class created (TInternetAccess)');
end;

function TInternetAccess.post(totalUrl: string;data:string):string;
begin
  result := request('POST', totalUrl, data);
end;

function TInternetAccess.post(protocol, host, url: string; data: string
  ): string;
begin
  result:=request('POST',protocol,host,url,data);
end;

procedure TInternetAccess.get(totalUrl: string; stream: TStream);
var buffer:string;
begin
  assert(stream<>nil);
  buffer:=get(totalUrl);
  stream.WriteBuffer(buffer[1],sizeof(buffer[1])*length(buffer));
end;

function TInternetAccess.get(totalUrl: string):string;
begin
  result:=request('GET', totalUrl, '');
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
  result:=request('GET', protocol, host, url, '');
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

