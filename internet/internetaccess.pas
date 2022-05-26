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
{** @abstract(You can use this unit to configure and create internet connections.)

    Currently it only supports HTTP/S connections, but this might change in future (e.g. to also support ftp)}
unit internetaccess;

{$I ../internettoolsconfig.inc}
{$WARN 5043 off : Symbol "$1" is deprecated}
interface

uses
  Classes, SysUtils, bbutils;
type
  PInternetConfig=^TInternetConfig;
  {** @abstract(Internet configuration)

  Not all options are supported by all backends. Compatibility matrix:

      @table(
        @rowHead( @cell(option)               @cell(wininet (w32)) @cell(synapse) @cell(OkHttp for Android) @cell((Android) Apache HttpComponents) )
        @row(     @cell(useragent)            @cell(yes)           @cell(yes)     @cell(yes)            @cell(yes) )
        @row(     @cell(tryDefaultConfig)     @cell(yes)           @cell(no)      @cell(no)             @cell(no) )
        @row(     @cell(http proxy)           @cell(yes)           @cell(yes)     @cell(no)             @cell(yes) )
        @row(     @cell(https proxy)          @cell(yes)           @cell(should use http proxy) @cell(no)    @cell(should use http proxy) )
        @row(     @cell(socks proxy)          @cell(yes)           @cell(yes)     @cell(no)             @cell(no) )
        @row(     @cell(proxy user/pass)      @cell(same auth for all proxies)@cell(separate for http and socks)@cell(no)@cell(no) )
        @row(     @cell(checkSSLCertificates) @cell(yes)           @cell(yes)     @cell(no, depends on Android)   @cell(no, depends on Android) )
        @row(     @cell(cafile/capath)        @cell(no, uses system CA) @cell(yes)     @cell(no)   @cell(no) )
      )

  You can always set more options on the internalHandle returned by TInternetAccess.
  }
  TInternetConfig=record
    userAgent: string; //**< the user agent used when connecting
    tryDefaultConfig: boolean; //**< should the system default configuration be used
    useProxy: Boolean; //**< should a proxy be used
    proxyHTTPName, proxyHTTPPort: string; //**< proxy used for HTTP
    proxyHTTPSName, proxyHTTPSPort: string; //**< proxy used for HTTPS
    proxySOCKSName, proxySOCKSPort: string; //**< socks proxy
    proxyUsername, proxyPassword: string;

    connectionCheckPage: string; //**< url we should open to check if an internet connection exists (e.g. http://google.de)

    checkSSLCertificates: boolean; //**< If ssl certificates should be checked in HTTPS connections
    CAFile, CAPath: string;  //**< CA certificates when using OpenSSL

    logToPath: string;

    procedure setProxy(proxy: string);
    procedure searchCertificates;
    function equalsUserAgent(const otherConfig: TInternetConfig): boolean;
    function equalsProxy(const otherConfig: TInternetConfig): boolean;
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

  TInternetAccessDataBlock = TPCharView;

  TMIMEMultipartSubData = record
    data: string;
    headers: TStringArray;
    function getFormDataName: string;
  end;
  PMIMEMultipartSubData = ^TMIMEMultipartSubData;

  //**encodes the data corresponding to RFC 1341 (preliminary)
  TMIMEMultipartData = record
    data: array of TMIMEMultipartSubData;
    function getFormDataIndex(const name: string): integer;
    procedure add(const sdata: string; const headers: string = '');
    procedure addFormData(const name, sdata: string; headers: string = '');
    //procedure TMIMEMultipartData.addFormData(const name, sdata: string; headers: TStringArray);
    procedure addFormDataFile(const name, filename: string; headers: string = '');
    procedure addFormData(const name, sdata, filename, contenttype, headers: string);
    function compose(out boundary: string; boundaryHint: string = '---------------------------1212jhjg2ypsdofx0235p2z5as09'): string;
    procedure parse(sdata, boundary: string);
    procedure clear;

    const HeaderSeparator = #13#10;
    //const ALLOWED_BOUNDARY_CHARS: string = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ''''()+,-./:=?'; //all allowed
    const ALLOWED_BOUNDARY_CHARS: string = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-'; //might be preferable to use those only
    class function buildHeaders(const name, filename, contenttype, headers: string): TStringArray; static;
    class function insertMissingNameToHeaders(const name: string; headers: TStringArray): TStringArray; static;
    class function nameFromHeader(const header: string): string; static;
    class function indexOfHeader(const sl: TStringArray; name: string): sizeint; static;
    class function HeaderForBoundary(const boundary: string): string; static;
  end;

  THeaderKind = (iahUnknown, iahContentType, iahContentDisposition, iahAccept, iahReferer, iahLocation, iahSetCookie, iahCookie, iahUserAgent);
  THTTPHeaderList = class(TStringList)
  public
    constructor Create;
    function getHeaderValue(kind: THeaderKind): string;
    function getHeaderValue(header: string): string; //**< Reads a certain HTTP header received by the last @noAutoLink(request)
    function getContentDispositionFileNameTry(out filename: string): Boolean;
    procedure add(const name, value: string);
    property headerValues[header: string]: string read GetheaderValue; default;
  protected
    class function parseCharacterSetEncodedHeaderRFC5987AsUtf8Try(value: TPCharView; out utf8: string): Boolean; static;
    class function parseContentDispositionFileNameTry(const contentDisposition: string; out filename: string): Boolean; static;
  end;

  TCookieFlags = set of (cfHostOnly, cfSecure {, cfHttpOnly});
  TCookieManager = record
    cookies: array of record
      domain, path, name, value: string;
      flags: TCookieFlags;
    end;
    procedure clear;
    procedure setCookie(domain, path: string; const name,value:string; flags: TCookieFlags);
    procedure parseHeadersForCookies(const source: TDecodedUrl; headers: THTTPHeaderList; allowUnsecureXidelExtensions: boolean = false);
    function makeCookieHeader(const target: TDecodedUrl):string;
    function makeCookieHeaderValueOnly(const target: TDecodedUrl):string;
    function serializeCookies: string;
    procedure loadFromFile(const fn: string);
    procedure saveToFile(const fn: string);
  end;

  TInternetAccess = class;
  TTransferContentInflater = class;

  TInternetAccessReaction = (iarAccept, iarFollowRedirectGET, iarFollowRedirectKeepMethod, iarRetry, iarReject);
     //if headerKind is iahUnknown header contains the entire header line name: value, otherwise only the value
  THeaderEnumCallback = procedure (data: pointer; headerKindHint: THeaderKind; const name, value: string);
  //**Event to monitor the progress of a download (measured in bytes)
  TProgressEvent=procedure (sender: TObject; progress,maxprogress: longint) of object;
  //**Event to intercept transfers end/start
  TTransferClearEvent = procedure() of object;
  TTransferBlockWriteEvent = TStreamLikeWriteNativeInt;

  TTransfer = record
    //in
    method: string;
    url: string;
    decodedUrl: TDecodedUrl;
    data: TPCharView;
    ownerAccess: TInternetAccess;

    //during transfer
    writeBlockCallback: TTransferBlockWriteEvent;
    inflater: TTransferContentInflater;
    currentSize, contentLength: sizeint;

    //out
    HTTPResultCode: longint;
    HTTPErrorDetails: string;
    receivedHTTPHeaders: THTTPHeaderList;

    procedure beginTransfer(onClear: TTransferClearEvent; onReceivedBlock: TTransferBlockWriteEvent; const amethod: string; const aurl: TDecodedUrl; const adata: TInternetAccessDataBlock);
    procedure writeBlock(const Buffer; Count: Longint);
    procedure endTransfer;
  end;

  TTransferStartEvent=procedure (sender: TObject; const method: string; const url: TDecodedUrl; const data: TInternetAccessDataBlock) of object;
  TTransferReactEvent=procedure (sender: TInternetAccess; var transfer: TTransfer; var reaction: TInternetAccessReaction) of object;
  TTransferEndEvent=procedure (sender: TObject; var transfer: TTransfer) of object;

  TTransferContentInflater = class
    //constructor create(); virtual; abstract;
    //procedure writeCompressedBlock(const Buffer; Count: Longint); virtual; abstract;
    class procedure injectDecoder(var transfer: TTransfer; const encoding: string); virtual; abstract;
    procedure endTransfer; virtual; abstract;
  end;
  TTransferInflaterClass = class of TTransferContentInflater;

  //**URL Encoding encodes every special character @code(#$AB) by @code(%AB). This model describes which characters are special:
  TUrlEncodingModel = (
    ueHTMLForm,     //**< Encode for application/x-www-form-urlencoded as defined in HTML 5 standard
    ueHTMLMultipartFieldName,  //**< Encode for  multipart/form-data field names as defined in HTML 5 standard
    ueURLPath,      //**< Encode for the path part of an URL
    ueURLQuery,     //**< Encode for the query part of an URL
    ueXPathURI,     //**< Encode for the XPath/XQuery function fn:encode-for-uri as defined in the XPath standard
    ueXPathHTML4,   //**< Encode for the XPath/XQuery function fn:escape-html-uri as defined in the XPath standard (they quote the the HTML4 standard)
    ueXPathFromIRI);//**< Encode for the XPath/XQuery function fn:iri-to-uri as defined in the XPath standard

  //**@abstract(Abstract base class for connections)
  //**This class defines the interface methods for HTTP requests, like @link(get), post or request.@br
  //**If a method fails, it will raise a EInternetException@br@br
  //**Since this is an abstract class, you cannot use it directly, but need to use one of the implementing child classes
  //**TW32InternetAccess, TSynapseInternetAccess, TAndroidInternetAccess or TMockInternetAccess. @br
  //**The recommended usage is to assign one of the child classes to defaultInternetAccessClass and
  //**then create an actual internet access class with @code(defaultInternetAccessClass.create()). @br
  //**Then it is trivial to swap between different implementations on different platforms, and the depending units
  //**(e.g. simpleinternet or xquery ) will use the implementation you have choosen. Default options will be taken from the global defaultInternetConfig record.
  TInternetAccess=class
  private
    FOnTransferEnd: TTransferEndEvent;
    FOnTransferReact: TTransferReactEvent;
    FOnTransferStart: TTransferStartEvent;
    FOnProgress:TProgressEvent;
    procedure setLastUrl(AValue: string);
  protected
    fconfig: TInternetConfig;
    procedure setConfig(internetConfig: PInternetConfig); virtual;
    function getConfig: PInternetConfig;
  protected
    //active transfer
    lastTransfer: TTransfer;
    //**Override this if you want to sub class it
    procedure doTransferUnchecked(var transfer: TTransfer);virtual;abstract;
    procedure doTransferChecked(onClear: TTransferClearEvent; onReceivedBlock: TTransferBlockWriteEvent; method: string; url: TDecodedUrl; data: TInternetAccessDataBlock; remainingRedirects: integer);
    function getLastErrorDetails(): string;
    function getLastHTTPHeaders: THTTPHeaderList;
    function getLastHTTPResultCode: longint;
    function getLastUrl: string;
    //utility functions to minimize platform dependent code
    procedure enumerateAdditionalHeaders(const atransfer: TTransfer; callback: THeaderEnumCallback; callbackData: pointer);
  public
    class function parseHeaderLineKind(const line: string): THeaderKind; static;
  protected
    class function parseHeaderLineValue(const line: string): string; static;
    class function parseHeaderLineName(const line: string): string; static;
    class function makeHeaderLine(const name, value: string): string; static;
    class function makeHeaderLine(const kind: THeaderKind; const value: string): string; static;
    class function makeHeaderName(const kind: THeaderKind): string; static;
    //constructor, since .create is "abstract" and can not be called
  public
    //in
    additionalHeaders: THTTPHeaderList; //**< Defines additional headers that should be send to the server
    ContentTypeForData: string; //**< Defines the Content-Type that is used to transmit data. Usually @code(application/x-www-form-urlencoded) or @code(multipart/form-data; boundary=...). @br This is overriden by a Content-Type set in additionalHeaders.
    multipartFormData: TMIMEMultipartData;
    function getFinalMultipartFormData: string;
  public
    //out
    property lastHTTPResultCode: longint read getlastHTTPResultCode;    //**< HTTP Status code of the last @noAutoLink(request)
    property lastUrl: string read getLastUrl write setLastUrl; //**< Last retrieved URL
    property lastHTTPHeaders: THTTPHeaderList read getLastHTTPHeaders; //**< HTTP headers received by the last @noAutoLink(request)
    function getLastContentType: string; //**< Same as getLastHTTPHeader('Content-Type') but easier to remember and without magic string
  public
    //** Cookies receive from/to-send the server
    cookies: TCookieManager;

    property config: PInternetConfig read getConfig write setConfig;

    constructor create();virtual;
    constructor create(const internetConfig: TInternetConfig);virtual;
    destructor Destroy; override;

    //**post the (raw) data to the given url and returns the resulting document
    //**as string
    function post(const totalUrl, data:string):string;
    //**post the (raw) data to the url given as three parts and returns the page as string
    function post(const protocol,host,url: string; data:string):string;
    //**get the url as stream
    procedure get(const totalUrl: string; stream:TStream);
    //**get the url as string
    function get(const totalUrl: string):string;
    //**get the url as stream
    procedure get(const protocol,host,url: string; stream:TStream);
    //**get the url as string
    function get(const protocol,host,url: string):string;

    //**performs a HTTP @noAutoLink(request)
    function request(const method, fullUrl, data:string):string;
    //**performs a HTTP @noAutoLink(request)
    function request(method, protocol,host,url, data:string):string;
    //**performs a HTTP @noAutoLink(request)
    function request(method: string; url: TDecodedUrl; data:string):string;
    //**performs a HTTP @noAutoLink(request)
    procedure request(const method: string; const url: TDecodedUrl; const uploadData:string; outStream: TStream);
    //**performs a HTTP @noAutoLink(request)
    procedure request(const method: string; const url: TDecodedUrl; const uploadData: TInternetAccessDataBlock; const onClear: TTransferClearEvent; const onReceivedBlock: TTransferBlockWriteEvent);



    //**checks if an internet connection exists
    function existsConnection():boolean; virtual; deprecated;
    //**call this to open a connection (very unreliable). It will return true on success
    function needConnection():boolean; virtual; deprecated;
    //**Should close all connections (doesn''t work)
    procedure closeOpenedConnections(); virtual; deprecated;

    //**Encodes the passed string in the url encoded format
    class function urlEncodeData(const data: string; encodingModel: TUrlEncodingModel = ueHTMLForm): string; static;
    //**Encodes all var=... pairs of data in the url encoded format
    class function urlEncodeData(data: TStringList; encodingModel: TUrlEncodingModel = ueHTMLForm): string; static;

    //**parses a string like 200=accept,400=abort,300=redirect
    class function reactFromCodeString(const codes: string; actualCode: integer; var reaction: TInternetAccessReaction): string; static;

    function internalHandle: TObject; virtual; abstract;

  published
    property OnTransferStart: TTransferStartEvent read FOnTransferStart write FOnTransferStart;
    property OnTransferReact: TTransferReactEvent read FOnTransferReact write FOnTransferReact;
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
//** @param(normalize performs some normalizations (e.g. foo//bar -> foo/bar))
function decodeURL(const totalURL: string; normalize: boolean = true): TDecodedUrl;
function decodeURL(const protocol, host, url: string; normalize: boolean = true): TDecodedUrl;

type TRetrieveType = (rtEmpty, rtRemoteURL, rtFile, rtXML, rtJSON);

(***
  Guesses the type of a given string@br@br

  E.g. for 'http://' it returns rtRemoteURL, for '/tmp' rtFile and for '<abc/>' rtXML.@br
  Internally used by simpleinternet.retrieve to determine how to actually @noAutoLink(retrieve) the data.
*)
function guessType(const data: string): TRetrieveType;


var defaultInternetConfiguration: TInternetConfig; //**< default configuration, used by all internet access classes
    defaultInternetAccessClass:TInternetAccessClass = nil; //**< default internet access. This controls which internet library the program will use.
    defaultTransferInflater: TTransferInflaterClass;

const ContentTypeUrlEncoded: string = 'application/x-www-form-urlencoded';
const ContentTypeMultipart: string = 'multipart/form-data'; //; boundary=
const ContentTypeTextPlain: string = 'text/plain'; //; boundary=


//**Make a HTTP GET request to a certain url.
function httpRequest(url: string): string; overload;
//**Make a HTTP POST request to a certain url, sending the data in rawpostdata unmodified to the server.
function httpRequest(url: string; rawpostdata: string): string; overload;
//**Make a HTTP POST request to a certain url, sending the data in postdata to the server, after url encoding all name=value pairs of it.
function httpRequest(url: string; postdata: TStringList): string; overload;
//**Make a HTTP request to a certain url, sending the data in rawdata unmodified to the server.
function httpRequest(const method, url, rawdata: string): string; overload;


//**This provides a thread-safe default internet
function defaultInternet: TInternetAccess;
//**If you use the procedural interface from different threads, you have to call freeThreadVars
//**before the thread terminates to prevent memory leaks @br
procedure freeThreadVars;
implementation
uses bbrandomnumbergenerator;
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
  result.username:=''; //if these values are not initialized here, they might get a random value
  result.password:='';
  result.path:='';
  result.params:='';
  result.linktarget:='';

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

function decodeURL(const protocol, host, url: string; normalize: boolean): TDecodedUrl;
var
  temp: String;
begin
  temp := '';
  if not strBeginsWith(url, '/') then temp := '/';
  result := decodeURL(protocol + '://' + host + temp + url, normalize);
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

  if trimmed[1] = '<' then
    exit(rtXML);

  if trimmed[1] in ['[', '{'] then
    exit(rtJSON);

  exit(rtFile);
end;


procedure saveableURL(var url:string);
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


function TMIMEMultipartSubData.getFormDataName: string;
var
  j: Integer;
begin
  for j := 0 to high(headers) do
     if striBeginsWith(headers[j], 'Content-Disposition:') then
       exit(striBetween(headers[j], 'name="', '"')); //todo: decoding
  result := '';
end;


{ TMIMEMultipartData }

procedure TMIMEMultipartData.addFormData(const name, sdata: string; headers: string);
begin
  headers := 'Content-Disposition: form-data; name="'+name+'"' + #13#10 + headers; //todo: name may encoded with [RFC2045]/rfc2047
  add(sdata, headers);
end;

{procedure TMIMEMultipartData.addFormData(const name, sdata: string; headers: TStringArray);
begin
  add(sdata, strJoin(insertMissingNameToHeaders(name, headers), HeaderSeparator));
end;}

procedure TMIMEMultipartData.addFormDataFile(const name, filename: string; headers: string);
begin
  headers := 'Content-Disposition: form-data; name="'+name+'"; filename="'+filename+'"' + #13#10 + headers; //todo: name may encoded with [RFC2045]/rfc2047; filename may be approximated or encoded with 2045
  add(strLoadFromFileUTF8(filename), headers);
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
var joinedHeaders: TStringArray = nil;
    encodedData: TStringArray = nil;
    i: Integer;
    ok: Boolean;
    rng: TRandomNumberGenerator;
    rnginitialized: boolean = false;
  function randomLetter: char;
  var seed: qword = qword($3456789012345678);
      temp: qword = 0;
  begin
    if not rnginitialized then begin
      if length(boundaryHint) >= sizeof(temp) then move(boundaryHint[length(boundaryHint) - sizeof(temp) + 1], temp, sizeof(temp));
      seed := seed xor temp;
      rng.randomize(seed);
      rnginitialized := true;
    end;
    result := ALLOWED_BOUNDARY_CHARS[ rng.next(length(ALLOWED_BOUNDARY_CHARS)) + 1 ];
  end;

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

  rng := default(TRandomNumberGenerator);

  boundary := boundaryHint;
  repeat
    repeat
      ok := true;
      for i := 0 to high(data) do begin
        ok := ok and not strContains(joinedHeaders[i], boundary) and not strContains(encodedData[i], boundary);
        if not ok then break;
      end;
      if not ok then
        boundary += randomLetter;
    until ok or (length(boundary) >= 70 {max length});
    if not ok then
      if length(boundaryHint) <= 16 then boundary := boundaryHint
      else boundary := copy(boundaryHint, 1, 8) + strCopyFrom(boundaryHint, length(boundaryHint) - 7); //if boundary hint has max length, we can only use a (random) part
  until ok;

  result := '';
  for i := 0 to high(data) do begin
    result += #13#10'--' +boundary + #13#10;
    result += joinedHeaders[i] + #13#10;
    result += #13#10; //separator
    result += encodedData[i];
  end;

  result += #13#10'--' + boundary + '--'#13#10;
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

class function TMIMEMultipartData.buildHeaders(const name, filename, contenttype, headers: string): TStringArray;
var
  splittedHeaders: TStringArray;
  disposition: String;
begin
  splittedHeaders := strSplit(headers, HeaderSeparator, false);

  if (contenttype <> '') and (indexOfHeader(splittedHeaders, 'Content-Type') < 0) then
    arrayInsert(splittedHeaders, 0, 'Content-Type: ' + contenttype);
  if indexOfHeader(splittedHeaders, 'Content-Disposition') < 0 then begin
    disposition := 'Content-Disposition: form-data; name="'+name+'"';
    if filename <> '' then disposition += '; filename="'+filename+'"'; //todo: name may encoded with [RFC2045]/rfc2047; filename may be approximated or encoded with 2045
    arrayInsert(splittedHeaders, 0, disposition);
  end;

  result := splittedHeaders;
end;

class function TMIMEMultipartData.insertMissingNameToHeaders(const name: string; headers: TStringArray): TStringArray;
begin
  result := headers;
  if indexOfHeader(result, 'Content-Disposition') < 0 then
    arrayInsert(result, 0, 'Content-Disposition: form-data; name="'+name+'"');
end;

class function TMIMEMultipartData.nameFromHeader(const header: string): string;
begin
  result := strBefore(header, ':');
end;

class function TMIMEMultipartData.indexOfHeader(const sl: TStringArray; name: string): sizeint;
var
  i: sizeint;
begin
  name := trim(name) + ':';
  for i:=0 to high(sl) do
    if striBeginsWith(sl[i], name) then
      exit(i);
  exit(-1);
end;

class function TMIMEMultipartData.HeaderForBoundary(const boundary: string): string;
begin
  result := 'Content-Type: ' + ContentTypeMultipart + '; boundary=' + boundary;
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
  if strIsAbsoluteURI(rel) then result := decodeURL(rel)
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

type TProxyOptionKind = (pkHTTPorHTTPS, pkHTTP, pkHTTPS, pkSocks);
procedure TInternetConfig.setProxy(proxy: string);
var
  url: TDecodedUrl;
  view, v, viewKind: TStringView;
  kind: TProxyOptionKind;
begin
  view := proxy.view;
  view.trim();
  if view.IsEmpty then begin
    useProxy:=false;
    exit;
  end;
  proxyUsername:='';
  proxyPassword:='';
  for v in view.splitFindToArray(' ') do begin
    kind := pkHTTPorHTTPS;
    if v.splitRightOfFind(viewKind, '=') then begin
      if viewKind = 'socks' then kind := pkSocks
      else if viewKind = 'http' then kind := pkHTTP
      else if viewKind = 'https' then kind := pkHTTPS;
    end;
    url := decodeURL(v.ToString);
    with url do begin
      if ( (username <> '') or (password <> '') ) and (proxyUsername = '') and (proxyPassword = '') then begin
        proxyUsername:=strUnescapeHex(username, '%');
        proxyPassword:=strUnescapeHex(password, '%');
      end;
      if kind in [pkHTTP, pkHTTPS, pkHTTPorHTTPS] then begin
        if port = '' then port := '8080';
        if kind in [pkHTTP, pkHTTPorHTTPS] then begin
          proxyHTTPName := host;
          proxyHTTPPort := port;
        end;
        if kind in [pkHTTPS, pkHTTPorHTTPS] then begin
          proxyHTTPSName := host;
          proxyHTTPSPort := port;
        end;
      end;
      if kind = pkSocks then begin
        proxySOCKSName := host;
        proxySOCKSPort := port;
      end;
    end;
  end;
  useProxy:=true;
{  writeln('h:' ,proxyHTTPName,' ',proxyHTTPPort);
  writeln('t:' ,proxyHTTPSName,' ',proxyHTTPSPort);
  writeln('s:' ,proxySocksName,' ',proxySocksPort);}
end;

procedure TInternetConfig.searchCertificates;
//see https://serverfault.com/questions/62496/ssl-certificate-location-on-unix-linux
const SystemCAFiles: array[1..2{$ifndef windows}+7{$endif}] of string = (
{$ifndef windows}
'/etc/ssl/certs/ca-certificates.crt',                // Debian/Ubuntu/Gentoo etc.
'/etc/pki/tls/certs/ca-bundle.crt',                  // Fedora/RHEL 6
'/etc/ssl/ca-bundle.pem',                            // OpenSUSE
'/etc/pki/tls/cacert.pem',                           // OpenELEC
'/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem', // CentOS/RHEL 7
'/etc/ssl/cert.pem',                                 // Alpine Linux
'/usr/local/ssl/cert.pem',                           // OpenSSL default
{$endif}
'cacert.pem',
'ca-bundle.crt'
);
{$ifndef windows}
 const SystemCAPaths: array[1..7] of string = (
'/etc/ssl/certs',               // SLES10/SLES11, https://golang.org/issue/12139
'/system/etc/security/cacerts', // Android
'/usr/local/share/certs',       // FreeBSD
'/etc/pki/tls/certs',           // Fedora/RHEL
'/etc/openssl/certs',           // NetBSD
'/var/ssl/certs',              // AIX
'/usr/local/ssl/certs'         // OpenSSL default
);


 {$endif}
 var
   i: Integer;
   temp: AnsiString;
{$ifdef windows}
   programPath: String;
{$endif}
begin
  temp := GetEnvironmentVariable('SSL_CERT_FILE');
  if (temp <> '') and (FileExists(temp)) then CAFile := temp;
  temp := GetEnvironmentVariable('SSL_CERT_DIR');
  if (temp <> '') and (DirectoryExists(temp)) then CAPath := temp;

  {$ifdef android}
  temp := GetEnvironmentVariable('PREFIX');
  if (CAFile = '') and FileExists(temp + '/etc/tls/cert.pem') then CAFile := temp + '/etc/tls/cert.pem';
  if (CAPath = '') and DirectoryExists(temp + '/etc/tls/certs') then CAPath := temp + '/etc/tls/certs';
  {$endif}

  {$ifdef windows}
  programPath:=IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
  for i := low(SystemCAFiles) to high(SystemCAFiles) do begin
    if CAFile <> '' then break;
    if FileExists(programPath + SystemCAFiles[i]) then CAFile := programPath + SystemCAFiles[i];
  end;
  {$endif}
  for i := low(SystemCAFiles) to high(SystemCAFiles) do begin
    if CAFile <> '' then break;
    if FileExists(SystemCAFiles[i]) then CAFile := SystemCAFiles[i];
  end;


  {$ifndef windows}
  for i := low(SystemCAPaths) to high(SystemCAPaths) do begin
    if CAPath <> '' then break;
    if DirectoryExists(SystemCAPaths[i]) then CAPath := SystemCAPaths[i];
  end;
  {$endif}
end;

function TInternetConfig.equalsUserAgent(const otherConfig: TInternetConfig): boolean;
begin
  result := userAgent = otherConfig.userAgent;
end;

function TInternetConfig.equalsProxy(const otherConfig: TInternetConfig): boolean;
begin
  if useProxy <> otherConfig.useProxy then exit(false);
  if not useProxy then exit(true);
  result := (proxyHTTPName = otherConfig.proxyHTTPName) and (proxyHTTPPort = otherConfig.proxyHTTPPort)
         and (proxyHTTPSName = otherConfig.proxyHTTPSName) and (proxyHTTPSPort = otherConfig.proxyHTTPSPort)
         and (proxySOCKSName = otherConfig.proxySOCKSName) and (proxySOCKSPort = otherConfig.proxySOCKSPort)
         and (proxyUsername = otherConfig.proxyUsername) and (proxyPassword = otherConfig.proxyPassword)
end;

procedure TTransfer.beginTransfer(onClear: TTransferClearEvent; onReceivedBlock: TTransferBlockWriteEvent;
     const amethod: string; const aurl: TDecodedUrl; const adata: TInternetAccessDataBlock);
begin
  onClear();

  method := amethod;
  decodedUrl := aurl;
  url := aurl.combinedExclude([dupUsername, dupPassword, dupLinkTarget]);
  data := adata;

  //during transfer
  writeBlockCallback := onReceivedBlock;
  FreeAndNil(inflater); //should be freed in endTransfer, but might have failed because of exceptions during transfer
  currentSize := 0;
  contentLength := -1; //only used for progress event

  //out
  HTTPResultCode := -1;
  HTTPErrorDetails := '';
  receivedHTTPHeaders.Clear;

  //do not call OnTransferStart, since that event is triggered once for each request, while this function is called for the each request and again for each redirection;
end;

procedure TTransfer.writeBlock(const Buffer; Count: Longint);
  procedure checkForContentEncoding;
  var encoding: string;
  begin
    encoding := receivedHTTPHeaders['content-encoding'];
    if encoding = '' then exit;
    defaultTransferInflater.injectDecoder(self, encoding);
  end;
begin
  if (currentSize = 0) and (count > 0) and (defaultTransferInflater <> nil) then
    checkForContentEncoding;
  currentSize := currentSize + Count;
  writeBlockCallback(buffer, count);
  if Assigned(ownerAccess.FOnProgress) then begin
    if contentLength < 0 then
      contentLength := StrToIntDef(receivedHTTPHeaders['content-length'], 0);
    ownerAccess.FOnProgress(ownerAccess, currentSize, contentLength);
  end;
end;

procedure TTransfer.endTransfer;
begin
  if assigned(inflater) then begin
    inflater.endTransfer;
    FreeAndNil(inflater);
  end;
  if Assigned(ownerAccess.FOnProgress) then begin
    if (currentSize < contentLength) then ownerAccess.FOnProgress(ownerAccess, currentSize, contentLength)
    else if contentLength = -1 then ownerAccess.FOnProgress(ownerAccess, 0, 0);
  end;
end;


function TInternetAccess.request(method, protocol, host, url, data: string): string;
begin
  result := request(method, decodeURL(protocol, host, url), data);
end;

function TInternetAccess.request(const method, fullUrl, data: string): string;
begin
  result := request(method, decodeURL(fullUrl), data);
end;


function TInternetAccess.request(method: string; url: TDecodedUrl; data: string): string;
var builder: TStrBuilder;
begin
  builder.init(@result);
  request(method, url, data.pcharView, TTransferClearEvent(@builder.clear), TTransferBlockWriteEvent(@builder.appendBuffer));
  builder.final;

  if fconfig.logToPath<>'' then
    writeString(fconfig.logToPath, url.combined+'<-DATA:'+data,result);
end;

procedure clearStream(self: TStream);
begin
  self.Position := 0;
  self.Size := 0;
end;
procedure writeStream(self: TStream; const buffer; size: NativeInt);
begin
  self.WriteBuffer(buffer, size);
end;

procedure TInternetAccess.request(const method: string; const url: TDecodedUrl; const uploadData: string; outStream: TStream);
begin
  request(method, url, uploadData.pcharView, TTransferClearEvent(makeMethod(@clearStream, outStream)), TTransferBlockWriteEvent(makeMethod(@writeStream, outStream)));
end;

procedure TInternetAccess.request(const method: string; const url: TDecodedUrl; const uploadData: TInternetAccessDataBlock;
  const onClear: TTransferClearEvent; const onReceivedBlock: TTransferBlockWriteEvent);
begin
  doTransferChecked(onClear, onReceivedBlock, method, url, uploadData, 10);
end;


function TInternetAccess.getLastHTTPHeaders: THTTPHeaderList;
begin
  result := lastTransfer.receivedHTTPHeaders;
end;

function TInternetAccess.getLastHTTPResultCode: longint;
begin
  result := lastTransfer.HTTPResultCode;
end;

function TInternetAccess.getLastUrl: string;
begin
  result := lastTransfer.url;
end;

procedure TInternetAccess.setLastUrl(AValue: string);
begin
  lastTransfer.url := AValue;
  lastTransfer.decodedUrl := decodeURL(avalue);
end;

procedure TInternetAccess.setConfig(internetConfig: PInternetConfig);
begin
  fconfig := internetConfig^;
end;

function TInternetAccess.getConfig: PInternetConfig;
begin
  result := @fconfig
end;

procedure TInternetAccess.doTransferChecked(onClear: TTransferClearEvent; onReceivedBlock: TTransferBlockWriteEvent;
   method: string; url: TDecodedUrl; data: TInternetAccessDataBlock; remainingRedirects: integer);

var
  reaction: TInternetAccessReaction;
  message: String;
  transfer: TTransfer;
begin
  if assigned(FOnTransferStart) then
     FOnTransferStart(self, method, url, data);

  url.prepareSelfForRequest(lastTransfer.decodedUrl);
  transfer := lastTransfer;

  method := UpperCase(method);
  reaction := iarReject;
  while reaction <> iarAccept do begin
    url.path := urlEncodeData(url.path, ueURLPath); //remove forbidden characters from url. mostly for Apache HTTPClient, it throws an exception if it they remain
    url.params := urlEncodeData(url.params, ueURLQuery);

    transfer.beginTransfer(onClear, onReceivedBlock, method, url, data );
    doTransferUnchecked(transfer);
    transfer.endTransfer;

    reaction := iarReject;
    case transfer.HTTPResultCode of
      200..299: reaction := iarAccept;
      301,302,303: if remainingRedirects > 0 then
        if striEqual(method, 'POST') then reaction := iarFollowRedirectGET
        else reaction := iarFollowRedirectKeepMethod;
      304..308: if remainingRedirects > 0 then reaction := iarFollowRedirectKeepMethod;
      else reaction := iarReject;
    end;

    if Assigned(OnTransferReact) then OnTransferReact(self, transfer, reaction);

    case reaction of
      iarAccept: ; //see above
      iarFollowRedirectGET, iarFollowRedirectKeepMethod: begin
        if reaction = iarFollowRedirectGET then begin
          method := 'GET';
          data := default(TInternetAccessDataBlock);
        end;
        cookies.parseHeadersForCookies(url, transfer.receivedHTTPHeaders); //parse for old url
        url := url.resolved(transfer.receivedHTTPHeaders.getHeaderValue(iahLocation));
        dec(remainingRedirects);
        continue;
      end;
      iarRetry: ; //do nothing
      else begin
        message := IntToStr(transfer.HTTPResultCode) + ' ' + transfer.HTTPErrorDetails;
        if transfer.HTTPResultCode <= 0 then message := 'Internet Error: ' + message
        else message := 'Internet/HTTP Error: ' + message;
        raise EInternetException.Create(message + LineEnding + 'when talking to: '+url.combined, transfer.HTTPResultCode);
      end;
    end;

    cookies.parseHeadersForCookies(url, transfer.receivedHTTPHeaders);
  end;

  lastTransfer := transfer;
  //url.username:=''; url.password:=''; url.linktarget:=''; //keep this secret
  //lastTransfer.decodedUrl := url;

  if assigned(FOnTransferEnd) then
    FOnTransferEnd(self, lastTransfer);
end;

function TInternetAccess.getLastErrorDetails(): string;
begin
  result := lastTransfer.HTTPErrorDetails;
end;

procedure TCookieManager.clear;
begin
  SetLength(cookies, 0);
end;

procedure TCookieManager.setCookie(domain, path: string; const name, value: string; flags: TCookieFlags);
var i:longint;
begin
  domain := lowercase(domain);
  for i:=0 to high(cookies) do //todo: use a map
    if strEqual(cookies[i].name, name) //case-sensitive according to RFC6265 (likely insensitive in RFC 2109, but that is obsolete)
       and strEqual(cookies[i].domain, domain) //case-insensitive, but domain is assumed to be normalized as it is not send to the server
       and strEqual(cookies[i].path, path) //case-sensitive
       then begin
      cookies[i].value:=value;
      cookies[i].flags:=flags;
      exit;
    end;
  setlength(cookies,length(cookies)+1);
  cookies[high(cookies)].domain:=domain;
  cookies[high(cookies)].path:=path;
  cookies[high(cookies)].name:=name;
  cookies[high(cookies)].value:=value;
  cookies[high(cookies)].flags:=flags;
end;

//Returns the highest private domain (e.g. www.example.com -> example.com )
//todo: do not count points, but use a list of tlds (so it will works with co.uk)
function isPublicDomain(const domain: string): boolean;
begin
  result := not strContains(domain, '.');
end;

function normalizeDomain(const s: string): string;
begin
  result := lowercase(s); //should this do xn-- punycoding?
end;

//str is a sub domain of domain
function domainMatches(const str, domain: string): boolean;
//RFC 6265: A string domain-matches a given domain string if at ...
begin
  if strEqual(str, domain) then exit(true);
  result := strEndsWith(str, domain) and (length(str) > length(domain)) and (str[length(str) - length(domain)] = '.') {and str is not an ip};
end;


//str is a subdirectory of path
function pathMatches(str: string; const path: string): boolean;
begin
  if strEqual(str, path) then exit(true);
  if not strBeginsWith(str, '/') then str := '/' + str;
  str := strUnescapeHex(str, '%');
  if strBeginsWith(str, path) then begin
    if strEndsWith(path, '/')
       or (length(str) = length(path))
       or ((length(str) > length(path)) and (str[length(path)+1] = '/' ) ) then
      exit(true);
  end;
  result := false;
end;

procedure TCookieManager.parseHeadersForCookies(const source: TDecodedUrl; headers: THTTPHeaderList; allowUnsecureXidelExtensions: boolean);
const WSP = [' ',#9];
var i:longint;
    header:string;
    ci, headerlen: Integer;

  function parseNameValuePair(out name, value: string; needEqualSign: boolean): boolean;
  var
      nameStart, nameEnd: integer;
      valueStart, valueEnd: integer;
  begin
    while (i <= headerlen) and (header[i] in WSP) do i+=1;
    nameStart := i;
    while (i <= headerlen) and not (header[i] in ['=', ';']) do i+=1;
    nameEnd := i - 1;
    while (nameEnd > nameStart) and (header[nameEnd] in WSP) do dec(nameEnd);
    name:=copy(header,nameStart,nameEnd -  nameStart + 1);

    if (i > headerlen) or (header[i] <> '=') then begin
      value := '';
      exit(not needEqualSign);
    end;

    inc(i);
    while (i <= headerlen) and (header[i] in WSP) do i+=1;
    valueStart := i;
    while (i <= headerlen) and (header[i] <> ';') do i+=1;
    valueEnd := i - 1;
    while (valueEnd > valueStart) and (header[valueEnd] in WSP) do dec(valueEnd);
    value:=copy(header,valueStart,valueEnd - valueStart + 1 );

    result := true;
  end;

var name, value, domain, path, tName, tValue: string;
    flags: TCookieFlags;
    lastSlash: LongInt;
begin
  for ci := 0 to headers.Count - 1 do
    case TInternetAccess.parseHeaderLineKind(headers.Strings[ci]) of
      iahSetCookie: begin
        header := TInternetAccess.parseHeaderLineValue(headers.Strings[ci]);
        headerlen := length(header);
        i := 1;
        if not parseNameValuePair(name, value, true) then continue;

        domain := '';
        path := '';
        flags := [];
        while i < headerlen do begin
          inc(i);
          if not parseNameValuePair(tname, tvalue, false) then break;
          case lowercase(tname) of
            //'expires': ;
            //'max-age': ;
            'domain': begin
              if strBeginsWith(tvalue, '.') then delete(tvalue, 1, 1);
              domain := normalizeDomain(tvalue);
            end;
            'path': if strBeginsWith(tvalue, '/') then path := tvalue;
            'secure': Include(flags, cfSecure);
            //'httponly': Include(flags, cfHttpOnly);
            'hostonly': if allowUnsecureXidelExtensions then include(flags, cfHostOnly);
          end;
        end;
        if domain <> '' then begin
          //exclude(flags, cfHostOnly);
          if not allowUnsecureXidelExtensions then
            if not domainMatches(normalizeDomain(source.host), domain) or isPublicDomain(domain) then continue; //only allow for super domains and private ones
        end else begin;
          include(flags, cfHostOnly);
          domain := normalizeDomain(source.host);
        end;
        if path = '' then begin
          path := source.path;
          if not strBeginsWith(path, '/') then path := '/'
          else begin
            lastSlash := strLastIndexOf(path, '/');
            if lastSlash = 1 then path := '/'
            else path := copy(path, 1, lastSlash - 1);
          end;
        end;
        setCookie(domain, strUnescapeHex(path, '%'), name, value, flags);
      end;
      else;
    end;
end;

function TCookieManager.makeCookieHeader(const target: TDecodedUrl): string;
begin
  result:='';
  if length(cookies)=0 then exit;
  result := TInternetAccess.makeHeaderLine(iahCookie, makeCookieHeaderValueOnly(target));
end;

function TCookieManager.makeCookieHeaderValueOnly(const target: TDecodedUrl): string;
var
  i: Integer;
  domain: String;
  builder: TStrBuilder;
begin
  result:='';
  if length(cookies)=0 then exit;
  domain := normalizeDomain(target.host);
  builder.init(@result);

  for i := 0 to high(cookies) do
    if (strEqual(cookies[i].domain, domain)
       or (not (cfHostOnly in cookies[i].flags) and domainMatches(domain, cookies[i].domain))) //also send of super domain
       and (pathMatches(target.path, cookies[i].path))
       and (not (cfSecure in cookies[i].flags) or ( striEqual(target.protocol, 'https') ) )
       then begin
      if builder.count <> 0 then builder.append('; ');
      builder.append(cookies[i].name);
      builder.append('=');
      builder.append(cookies[i].value);
    end;
  builder.final;
end;

function TCookieManager.serializeCookies: string;
var builder: TStrBuilder;
  i: Integer;
begin
  result:='';
  with builder do begin
    init(@result);
    for i := 0 to high(cookies) do with cookies[i] do begin
      append('Set-Cookie: ');
      append(name);
      append('=');
      append(value);
      append('; Domain=');
      append(domain);
      append('; Path=');
      append( TInternetAccess.urlEncodeData(path, ueURLPath));
      if cfHostOnly in flags then append('; HostOnly');
      if cfSecure in flags then append('; Secure');
      append(#13#10);
    end;
    final;
  end;
end;

procedure TCookieManager.loadFromFile(const fn: string);
var temp: THTTPHeaderList;
    tempurl: TDecodedUrl;
begin
  temp := THTTPHeaderList.Create;
  try
    temp.LoadFromFile(fn);
    tempurl := decodeURL('file:///'); //this is not actually used in parseHeadersForCookies
    parseHeadersForCookies(tempurl, temp, true);
  finally
    temp.free;
  end;
end;

procedure TCookieManager.saveToFile(const fn: string);
begin
  strSaveToFileUTF8(fn, serializeCookies);
end;

class function TInternetAccess.parseHeaderLineKind(const line: string): THeaderKind;
  function check(const s: string): boolean;
  var
    i: Integer;
  begin
    result := false;
    if striBeginsWith(line, s) then begin
      for i := length(s) + 1 to length(line) do
        case line[i] of
          ' ',#9,#10,#13: ;
          ':': exit(true);
          else exit(false);
        end;
    end;
  end;

begin
  result := iahUnknown;
  if line = '' then exit();
  case line[1] of
    'c', 'C': if check('content-type') then exit(iahContentType)
              else if check('cookie') then exit(iahCookie)
              else if check('content-disposition') then exit(iahContentDisposition);
    'a', 'A': if check('accept') then exit(iahAccept);
    'l', 'L': if check('location') then exit(iahLocation);
    'r', 'R': if check('referer') then exit(iahReferer);
    's', 'S': if check('set-cookie') then exit(iahSetCookie);
    'u', 'U': if check('user-agent') then exit(iahUserAgent);
  end;
end;

class function TInternetAccess.parseHeaderLineValue(const line: string): string;
begin
  result := trim(strCopyFrom(line, pos(':', line)+1))
end;

class function TInternetAccess.parseHeaderLineName(const line: string): string;
begin
  result := copy(line, 1, pos(':', line) - 1);
end;

class function TInternetAccess.makeHeaderLine(const name, value: string): string;
begin
  result := name + ': ' + value;
end;

class function TInternetAccess.makeHeaderLine(const kind: THeaderKind; const value: string): string;
begin
  result := makeHeaderName(kind) + ': '+value;
end;

class function TInternetAccess.makeHeaderName(const kind: THeaderKind): string;
begin
  case kind of
    iahContentType: result := 'Content-Type';
    iahContentDisposition: result := 'Content-Disposition';
    iahAccept: result := 'Accept';
    iahReferer: result := 'Referer';
    iahLocation: result := 'Location';
    iahSetCookie: result := 'Set-Cookie';
    iahCookie: result := 'Cookie';
    iahUserAgent: result := 'User-Agent';
    else raise EInternetException.create('Internal error: Unknown header line kind');
  end;
end;

procedure TInternetAccess.enumerateAdditionalHeaders(const atransfer: TTransfer; callback: THeaderEnumCallback; callbackData: pointer);
  procedure callKnownKind(kind: THeaderKind; value: string);
  begin
    callback(callbackData, kind, makeHeaderName(kind), value);
  end;

var
  hadHeader: array[THeaderKind] of Boolean = (false, false, false, false, false, false, false, false, false);
  i: Integer;
  kind: THeaderKind;
  temp: String;
begin
  for i := 0 to additionalHeaders.Count - 1 do begin
     kind := parseHeaderLineKind(additionalHeaders.Strings[i]);
     hadHeader[kind] := true;
     callback(callbackData, kind, parseHeaderLineName(additionalHeaders.Strings[i]), parseHeaderLineValue(additionalHeaders.Strings[i]));
   end;

  if (not hadHeader[iahReferer]) and (lastUrl <> '') then callKnownKind( iahReferer, lastUrl );
   if (not hadHeader[iahAccept]) then callKnownKind( iahAccept, 'text/html,application/xhtml+xml,application/xml,text/*,*/*' );
   if (not hadHeader[iahCookie]) and (length(cookies.cookies) > 0) then begin
     temp := cookies.makeCookieHeaderValueOnly(atransfer.decodedUrl);
     if temp <> '' then callKnownKind( iahCookie, temp);
   end;
   if (not hadHeader[iahContentType]) and (not atransfer.data.isEmpty) then callKnownKind(iahContentType, ContentTypeForData);
   //if (not hadHeader[iahUserAgent]) then callKnownKind( iahUserAgent, internetConfig^.userAgent ); no central handling of agent, it is too different between the APIs
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

constructor THTTPHeaderList.Create;
begin
  inherited;
  NameValueSeparator := ':';
end;

function THTTPHeaderList.getHeaderValue(kind: THeaderKind): string;
var
  i: Integer;
begin
  for i:= 0 to count - 1 do
    if TInternetAccess.parseHeaderLineKind(Strings[i]) = kind then
      exit(TInternetAccess.parseHeaderLineValue(Strings[i]));
  exit('');
end;

function THTTPHeaderList.getHeaderValue(header: string): string;
var
  i: Integer;
begin
  header := header + ':';
  for i:= 0 to count - 1 do
    if striBeginsWith(Strings[i], header) then
      exit(trim(strCopyFrom(Strings[i], length(header) + 1)));
  exit('');
end;

function THTTPHeaderList.getContentDispositionFileNameTry(out filename: string): Boolean;
begin
  result := parseContentDispositionFileNameTry(getHeaderValue(iahContentDisposition), filename);
end;

procedure THTTPHeaderList.add(const name, value: string);
begin
  add(name + ': '+value);
end;

class function THTTPHeaderList.parseCharacterSetEncodedHeaderRFC5987AsUtf8Try(value: TPCharView; out utf8: string): Boolean;
var
  enc, country: TPCharView;
  codepage: TSystemCodePage;
begin
  result := value.splitRightOfFind(enc, '''');
  result := result and value.splitRightOfFind(country, '''');
  if not result then exit;
  codepage := strEncodingFromName(enc.ToString);
  if codepage = CP_NONE then exit(false);
  utf8 := strConvert(strUnescapeHex(value.ToString, '%'), codepage, cp_acp);
end;

class function THTTPHeaderList.parseContentDispositionFileNameTry(const contentDisposition: string; out filename: string): Boolean;
var
  directive, temp: String;
  directives: sysutils.TStringArray;
  name, value: TPCharView;
begin
  result := false;
  filename := '';
  if not contentDisposition.StartsWith('attachment') then exit;
  directives := contentDisposition.Split(';');
  for directive in directives do begin
    if not directive.pcharView.splitAtFind(name, '=', value) then continue;
    name.trim();
    value.trim();
    if name = 'filename*' then begin
      result := true;
      if parseCharacterSetEncodedHeaderRFC5987AsUtf8Try(value, temp) then begin
        filename := temp;
        exit(true);
      end;
    end else if name = 'filename' then begin
      if value.beginsWith('"') and value.endsWith('"') then begin
        value.leftOfLast(1);
        value.rightOfFirst(1);
      end;
      filename := value.ToString;
      result := true;
    end;
  end;
end;

function TInternetAccess.getLastContentType: string;
begin
  result := lastHTTPHeaders.getHeaderValue(iahContentType);
end;

constructor TInternetAccess.create();
begin
  create(defaultInternetConfiguration);
end;

constructor TInternetAccess.create(const internetConfig: TInternetConfig);
begin
  if ClassType = TInternetAccess then
    raise eabstracterror.create('Abstract internet class created (TInternetAccess)');

  additionalHeaders := THTTPHeaderList.Create;
  lastTransfer.receivedHTTPHeaders := THTTPHeaderList.Create;
  lastTransfer.ownerAccess := self;

  ContentTypeForData := ContentTypeUrlEncoded;

  setConfig(@internetConfig);
  if fconfig.userAgent='' then begin
    fconfig.userAgent:='Mozilla/5.0 (compatible)';
    setConfig(@fconfig);
  end;
end;

destructor TInternetAccess.Destroy;
begin
  FreeAndNil(lastTransfer.receivedHTTPHeaders); //created by init
  additionalHeaders.Free;
  FreeAndNil(lastTransfer.inflater); //should be freed in endTransfer, but might have failed because of exceptions during transfer
  inherited Destroy;
end;

function TInternetAccess.post(const totalUrl, data: string): string;
begin
  result := request('POST', totalUrl, data);
end;

function TInternetAccess.post(const protocol, host, url: string; data: string): string;
begin
  result:=request('POST',protocol,host,url,data);
end;

procedure TInternetAccess.get(const totalUrl: string; stream: TStream);
begin
  request('GET', decodeURL(totalUrl), '', stream);
end;

function TInternetAccess.get(const totalUrl: string): string;
begin
  result:=request('GET', totalUrl, '');
end;

procedure TInternetAccess.get(const protocol, host, url: string; stream: TStream);
begin
  request('GET', decodeURL(protocol, host, url), '', stream);
end;

function TInternetAccess.get(const protocol, host, url: string): string;
begin
  result:=request('GET', protocol, host, url, '');
end;



function TInternetAccess.existsConnection(): boolean;
begin
  result:=false;
  try
    if fconfig.connectionCheckPage = '' then
      result:=get('https','www.google.de','/')<>''
     else
      result:=get('https',fconfig.connectionCheckPage,'/')<>'';
  except
  end;
end;

function TInternetAccess.needConnection(): boolean;
begin
  result:=existsConnection();
end;

procedure TInternetAccess.closeOpenedConnections();
begin

end;

class function TInternetAccess.urlEncodeData(const data: string; encodingModel: TUrlEncodingModel): string;
const allowedUnreserved =  ['0'..'9', 'A'..'Z', 'a'..'z',    '-', '_', '.', '!', '~', '*', '''', '(', ')', '%'];
      allowedPath = allowedUnreserved  + [':','@','&','=','+','$',',', ';','/'];
      allowedURI = allowedUnreserved + [';','/','?',':','@','&','=','+','$',',','[',']'];
      allChars = [#0..#255];
const ENCODE_TABLE: array[TUrlEncodingModel] of TCharSet = (
  allChars - [#$20, #$2A, #$2D, #$2E, #$30..#$39, #$41..#$5A, #$5F, #$61..#$7A],
  //Multipart name encoding:
  //this is written in the HTML5 standard.
  //this is nothing like the browser behaviour. Firefox converts " to \" and line break to space
  [#$A, #$D, #$22],
  allChars - allowedPath,
  allChars - allowedURI,
  allChars - ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '~'],
  allChars - [#32..#126],
  allChars - ([#$20..#$7E] - ['<','>','"',' ','{','}','|','\','^','`'])
);
var
  i: Integer;

begin
  result:=strEscapeToHex(data, ENCODE_TABLE[encodingModel], '%');
  if encodingModel = ueHTMLForm then begin
    for i := 1 to length(result) do
      if result[i] = ' ' then result[i] := '+';
  end;
end;

class function TInternetAccess.urlEncodeData(data: TStringList; encodingModel: TUrlEncodingModel): string;
var
 i: Integer;
begin
  Result:='';
  for i:=0 to data.Count-1 do begin
    if result <> '' then result+='&';
    result+=urlEncodeData(data.Names[i], encodingModel)+'='+urlEncodeData(data.ValueFromIndex[i], encodingModel);
  end;
end;

class function TInternetAccess.reactFromCodeString(const codes: string; actualCode: integer;var reaction: TInternetAccessReaction): string;
  function matches(filter: string; value: string): boolean;
  var
    i: Integer;
  begin
    if length(filter) <> length(value) then exit(false);
    for i := 1 to length(filter) do
      if (filter[i] <> 'x') and (filter[i] <> value[i]) then
        exit(false);
    result := true;
  end;

var
  errors: TStringArray;
  cur: TStringArray;
  i: Integer;
begin
  result := '';
  errors := strSplit(codes, ',');
  for i:=0 to high(errors) do begin
    cur := strSplit(errors[i], '=');
    if matches(trim(cur[0]), inttostr(actualCode)) then begin
      result := trim(cur[1]);
      case result of
        'accept', 'ignore', 'skip': reaction := iarAccept;
        'retry': reaction := iarRetry;
        'redirect': reaction := iarFollowRedirectGET;
        'redirect-data': reaction := iarFollowRedirectKeepMethod;
        'abort': reaction := iarReject
      end;
      exit;
    end;
  end;
end;


threadvar theDefaultInternet: TInternetAccess;

function httpRequest(url: string): string;
begin
  result:=defaultInternet.get(url);
end;

function httpRequest(url: string; rawpostdata: string): string;
begin
  result:=defaultInternet.post(url, rawpostdata);
end;

function httpRequest(url: string; postdata: TStringList): string;
begin
  result := httpRequest(url, TInternetAccess.urlEncodeData(postdata, ueHTMLForm));
end;

function httpRequest(const method, url, rawdata: string): string;
begin
  result := defaultInternet.request(method, url, rawdata);
end;

function defaultInternet: TInternetAccess;
begin
  if theDefaultInternet <> nil then exit(theDefaultInternet);
  if defaultInternetAccessClass = nil then
    raise Exception.Create('You need to set defaultInternetAccessClass to choose between synapse, wininet or android. Or you can add one of the units synapseinternetaccess, okhttpinternetaccecss or w32internetaccess to your uses clauses (if that unit actually will be compiled depends on the active defines).');
  theDefaultInternet := defaultInternetAccessClass.create;
  result := theDefaultInternet;
end;


procedure freeThreadVars;
begin
  FreeAndNil(theDefaultInternet);
end;
initialization
defaultInternetConfiguration.checkSSLCertificates := true;
defaultInternetConfiguration.tryDefaultConfig := true;

finalization
  freeThreadVars;
end.

