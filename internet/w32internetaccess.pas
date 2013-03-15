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
{**This unit contains the wrapper for wininet}
unit w32internetaccess;

{$mode objfpc}{$H+}
//{$define debug}//7Remove debug before publishing (write every opened page to the hard disk)
//{$define simulateInet} //read the files previously written on the hard disks
interface

//{$DEFINE DELPHI_WININET} //If you use the Delphi wininet unit. Search for   "unit wininet" inurl:wininet.pas   to find one. (and then add {$mode delphi} there)
{$IFDEF WIN32}
{$DEFINE COMPILE_W32_INTERNETACCESS}  //If this unit should be compiled
{$ENDIF}

{$IFDEF PASDOCRUN}
{$DEFINE COMPILE_W32_INTERNETACCESS}  //If this unit should be compiled
{$ENDIF}

{$IFDEF COMPILE_W32_INTERNETACCESS}
uses
  windows,Classes, SysUtils,
  wininet,
  internetaccess;


type

  { EW32InternetException }

  EW32InternetException=class(EInternetException)
    constructor create();
    constructor create(s:string;showError:boolean=false);
    constructor create(s:string;httpStatusCode: integer);
  end;

  { TInternetAccess }

  { TW32InternetAccess }

  //**@abstract(Internet access class using the wininet library)
  //**Set defaultInternetAccessClass to TW32InternetAccess if
  //**you want to use wininet to connect to internet@br
  //**You probably need an additional wininet header (should be contained somewhere
  //**in freepascal, if not you can use the Delphi unit)
  TW32InternetAccess=class(TInternetAccess)
  protected
    hSession,hLastConnection: hInternet;
    lastConnectedUrl, lastRefererUrl: TDecodedUrl;
    newConnectionOpened:boolean;
    FLastHTTPHeaders: TStringList;
    function GetLastHTTPHeaders: TStringList; override;
    function doTransferRec(method:string; decoded: TDecodedUrl; data:string;redirectionCount: integer): string;
    function doTransfer(method:string; const url: TDecodedUrl; data:string): string;override;
  public
    constructor create();override;
    destructor destroy;override;
    function needConnection():boolean;override;
    procedure closeOpenedConnections();override;

    function internalHandle: TObject; override;

  public
    checkSSLCertificates: boolean;
  end;
  TW32InternetAccessClass = class of TW32InternetAccess;

const TEMPORARY_DIRECTORY='T:\theInternet\';

{$ENDIF}
implementation

uses bbutils;

resourcestring
  rsNoInternetSessionCre = 'No internet session created';
  rsConnectingTo0SFailed = 'Connecting to %0:s failed';
  rsReceivingFrom0SFaile = 'Receiving from %0:s failed.';
  rsInternetRequestFaile = 'Internet request failed';
  rsHTTPErrorCode0SNWhen = 'HTTP Error code: %0:s\nWhen connecting to %1:s';
  rsFailedToConnectToThe = 'Failed to connect to the internet.';
  rsNoInternetHandlesAva = 'No internet handles available. Restart your computer.';
  rsConnectionTimedOut = 'Connection timed out';
  rsExtendedInternetConn = 'Extended internet connection failure:';
  rsInternalErrorInWinin = 'Internal error in wininet.dll';
  rsInvalidUrl = 'Invalid url';
  rsUnrecognizedUrlSchem = 'Unrecognized url scheme';
  rsDNSRequestFailedProb = 'DNS request failed. Probably missing an internet connection or invalid host name.';
  rsProtocolNotFound = 'Protocol not found';
  rsInvalidParameter = 'Invalid parameter';
  rsInvalidParameterLeng = 'Invalid parameter length';
  rsTheRequestOptionCanN = 'The request option can not be set, only queried.';
  rsInternetHasBeenShutd = 'Internet has been shutdown. Restart your computer.';
  rsInvalidInternetOpera = 'Invalid internet operation.';
  rsOperationCancelled = 'Operation cancelled.';
  rsInvalidHandle = 'Invalid handle (type).';
  rsIncorrectHandle = 'Incorrect handle (state).';
  rsInvalidProxyUsage = 'Invalid proxy usage';
  rsRegistryCorrupted = 'Registry corrupted / not found.';
  rsCorruptedRegistry = 'Corrupted registry.';
  rsDirectInternetAccess = 'Direct internet access currently not possible.';
  rsIncorrectFormatForHt = 'Incorrect format for http request.';
  rsInternetItemNotFound = 'Internet item not found.';
  rsFailedToCreateConnec = 'Failed to create connection.';
  rsInternetConnectionAb = 'Internet connection aborted';
  rsInternetConnectionRe = 'Internet connection reseted';
  rsNeedRetriedRequest = 'Need retried request';
  rsUnsecurePage = 'Unsecure page';
  rsInternetHandleAlread = 'Internet handle already exists.';
  rsServerAnsweredWithou = 'Server answered without header';
  rsHTTPHeaderNotFound = 'HTTP header not found';
  rsServerResponseIsInva = 'Server response is invalid.';
  rsInvalidHttpHeader = 'Invalid http header';
  rsInvalidHttpQueryRequ = 'Invalid http query request';
  rsHttpHeaderAlreadyExi = 'Http header already exists';
  rsInvalidInternetHandl = 'Invalid internet handle';
  rsTheSSLCertificateWas = 'The SSL certificate was not revoked.';
  rsInvalidSSLCertificat = 'Invalid SSL certificate (invalid site name)';
  rsInvalidSSLCertificat2 = 'Invalid SSL certificate (too old)';
  rsFailedToValidateSSLC = 'Failed to validate SSL certificate';
  rsUnknownInternetError = 'Unknown internet error: ';
  rsWindowsErrorMessage = 'Windows error message: ';

//uses bbdebugtools;

{$IFDEF COMPILE_W32_INTERNETACCESS}

constructor EW32InternetException.create();
var s: string;
    temp1,temp2: dword;
begin
{ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP
Client authorization is not set up on this computer.
}
{ERROR_INTERNET_INCORRECT_USER_NAME
The request to connect and log on to an FTP server could not be completed because the supplied user name is incorrect.
ERROR_INTERNET_INCORRECT_PASSWORD
The request to connect and log on to an FTP server could not be completed because the supplied password is incorrect.
ERROR_INTERNET_LOGIN_FAILURE
The request to connect to and log on to an FTP server failed.}
{    ERROR_INTERNET_NO_CONTEXT
An asynchronous request could not be made because a zero context value was supplied.
ERROR_INTERNET_NO_CALLBACK
An asynchronous request could not be made because a callback function has not been set.
ERROR_INTERNET_REQUEST_PENDING
The required operation could not be completed because one or more requests are pending.}
{ERROR_INTERNET_ZONE_CROSSING
Not used in this release.}
{ERROR_FTP_TRANSFER_IN_PROGRESS
The requested operation cannot be made on the FTP session handle because an operation is already in progress.
ERROR_FTP_DROPPED
The FTP operation was not completed because the session was aborted.
ERROR_GOPHER_PROTOCOL_ERROR
An error was detected while parsing data returned from the gopher server.
ERROR_GOPHER_NOT_FILE
The request must be made for a file locator.
ERROR_GOPHER_DATA_ERROR
An error was detected while receiving data from the gopher server.
ERROR_GOPHER_END_OF_DATA
The end of the data has been reached.
ERROR_GOPHER_INVALID_LOCATOR
The supplied locator is not valid.
ERROR_GOPHER_INCORRECT_LOCATOR_TYPE
The type of the locator is not correct for this operation.
ERROR_GOPHER_NOT_GOPHER_PLUS
The requested operation can only be made against a Gopher+ server, or with a locator that specifies a Gopher+ operation.
ERROR_GOPHER_ATTRIBUTE_NOT_FOUND
The requested attribute could not be located.
ERROR_GOPHER_UNKNOWN_LOCATOR
The locator type is unknown.}
  s:='';
  case GetLastError of
    ERROR_INTERNET_OUT_OF_HANDLES:
      s:=rsNoInternetHandlesAva;
    ERROR_INTERNET_TIMEOUT:
      s:=rsConnectionTimedOut;
    ERROR_INTERNET_EXTENDED_ERROR: begin
      setlength(s,4096);
      temp2:=length(s);
      InternetGetLastResponseInfo({$ifndef DELPHI_WININET}@{$endif}temp1,@s[1],temp2);
      setlength(s,temp2);
      s:=rsExtendedInternetConn+#13#10+s;
    end;
    ERROR_INTERNET_INTERNAL_ERROR:
      s:=rsInternalErrorInWinin;
    ERROR_INTERNET_INVALID_URL:
      s:=rsInvalidUrl;
    ERROR_INTERNET_UNRECOGNIZED_SCHEME:
      s:=rsUnrecognizedUrlSchem;
    ERROR_INTERNET_NAME_NOT_RESOLVED:
      s:=rsDNSRequestFailedProb;
    ERROR_INTERNET_PROTOCOL_NOT_FOUND:
      s:=rsProtocolNotFound;
    ERROR_INTERNET_INVALID_OPTION:
      s:=rsInvalidParameter;
    ERROR_INTERNET_BAD_OPTION_LENGTH:
      s:=rsInvalidParameterLeng;
    ERROR_INTERNET_OPTION_NOT_SETTABLE:
      s:=rsTheRequestOptionCanN;
    ERROR_INTERNET_SHUTDOWN:
      s:=rsInternetHasBeenShutd;
    ERROR_INTERNET_INVALID_OPERATION:
      s:=rsInvalidInternetOpera;
    ERROR_INTERNET_OPERATION_CANCELLED:
      s:=rsOperationCancelled;
    ERROR_INTERNET_INCORRECT_HANDLE_TYPE:
      s:=rsInvalidHandle;
    ERROR_INTERNET_INCORRECT_HANDLE_STATE:
      s:=rsIncorrectHandle;
    ERROR_INTERNET_NOT_PROXY_REQUEST        :
      s:=rsInvalidProxyUsage;
    ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND:
      s:=rsRegistryCorrupted;
    ERROR_INTERNET_BAD_REGISTRY_PARAMETER:
      s:=rsCorruptedRegistry;
    ERROR_INTERNET_NO_DIRECT_ACCESS:
      s:=rsDirectInternetAccess;
    ERROR_INTERNET_INCORRECT_FORMAT:
      s:=rsIncorrectFormatForHt;
    ERROR_INTERNET_ITEM_NOT_FOUND:
      s:=rsInternetItemNotFound;
    ERROR_INTERNET_CANNOT_CONNECT:
      s:=rsFailedToCreateConnec;
    ERROR_INTERNET_CONNECTION_ABORTED:
      s:=rsInternetConnectionAb;
    ERROR_INTERNET_CONNECTION_RESET:
      s:=rsInternetConnectionRe;
    ERROR_INTERNET_FORCE_RETRY:
      s:=rsNeedRetriedRequest;
    ERROR_INTERNET_MIXED_SECURITY:
      s:=rsUnsecurePage;
    {ERROR_INTERNET_SSL_CERT_CN_INVALID:
      s:='Sicherheitszertifikat des Servers ungültig';}
    ERROR_INTERNET_HANDLE_EXISTS :
      s:=rsInternetHandleAlread;
    ERROR_HTTP_HEADER_NOT_FOUND :
      s:=rsHTTPHeaderNotFound;
    ERROR_HTTP_DOWNLEVEL_SERVER:
      s:=rsServerAnsweredWithou;
    ERROR_HTTP_INVALID_SERVER_RESPONSE:
      s:=rsServerResponseIsInva;
    ERROR_HTTP_INVALID_HEADER:
      s:=rsInvalidHttpHeader;
    ERROR_HTTP_INVALID_QUERY_REQUEST:
      s:=rsInvalidHttpQueryRequ;
    ERROR_HTTP_HEADER_ALREADY_EXISTS:
      s:=rsHttpHeaderAlreadyExi;
    ERROR_INVALID_HANDLE:
      s:=rsInvalidInternetHandl;
    ERROR_INVALID_PARAMETER:
      s:=rsInvalidParameter;
    ERROR_INTERNET_SEC_CERT_NO_REV:
      s :=rsTheSSLCertificateWas;
    ERROR_INTERNET_SEC_CERT_CN_INVALID:
      s :=rsInvalidSSLCertificat;
    ERROR_INTERNET_SEC_CERT_DATE_INVALID :
      s := rsInvalidSSLCertificat2;
    ERROR_INTERNET_SEC_CERT_REV_FAILED:
      s := rsFailedToValidateSSLC;
    else
      s:=rsUnknownInternetError + IntToStr(GetLastError);
  end;
  inherited create(s);
end;

constructor EW32InternetException.create(s:string;showError:boolean=false);
begin
  create();
  details:=rsWindowsErrorMessage+Message;
  Message:=s;
end;

constructor EW32InternetException.create(s: string; httpStatusCode: integer);
begin
  create(s);
  errorCode:=httpStatusCode;
end;

function TW32InternetAccess.GetLastHTTPHeaders: TStringList;
begin
  result := FLastHTTPHeaders;
end;

function TW32InternetAccess.doTransferRec(method:string; decoded: TDecodedUrl; data:string;redirectionCount: integer): string;
const postHeader='Content-Type: application/x-www-form-urlencoded';
const defaultAccept: array[1..6] of ansistring = ('text/html', 'application/xhtml+xml', 'application/xml', 'text/*', '*/*', '');
var
  databuffer : array[0..4095] of char;
  hfile: hInternet;
  dwindex,dwcodelen,dwread,dwNumber,temp,dwContentLength: cardinal;
  tempPort: integer;
  dwcode : array[1..20] of char;
  res    : pchar;
  cookiestr:string;
  newurl: string;
  callResult: boolean;
  i: Integer;
  headerOut: string;
  overridenPostHeader: string;
begin
  if not assigned(hSession) Then
    raise EW32InternetException.create(rsNoInternetSessionCre);

  if (lastConnectedUrl.Protocol<>decoded.protocol) or (lastConnectedUrl.Host<>decoded.host) or (lastConnectedUrl.Port <> decoded.port)
     or (lastConnectedUrl.username <> decoded.username) or (lastConnectedUrl.password <> decoded.password) then begin
    if hLastConnection<>nil then
      InternetCloseHandle(hLastConnection);
    if decoded.protocol='http' then begin
      tempPort:=80;
      temp:=INTERNET_SERVICE_HTTP;
    end else if decoded.protocol='https' then begin
      tempPort:=443;
      temp:=INTERNET_SERVICE_HTTP;
    end;
    if decoded.port <> '' then
      tempPort := StrToIntDef(decoded.port, 80);
    //huh? wininet seems to remember the password, if it is set once and continues sending it with new requests, even if is unset. (tested with WINE and Windows 7)
    if (decoded.username = '') and (decoded.password = '') then hLastConnection:= InternetConnect(hSession,pchar(decoded.host),tempPort,nil,            nil,temp,0,0)
    else if decoded.password = '' then                  hLastConnection:= InternetConnect(hSession,pchar(decoded.host),tempPort,pchar(decoded.username),nil,temp,0,0)
    else                                        hLastConnection:= InternetConnect(hSession,pchar(decoded.host),tempPort,pchar(decoded.username),pchar(decoded.password),temp,0,0);
    if hLastConnection=nil then
      raise EW32InternetException.create(format(rsConnectingTo0SFailed, [decoded.host]));
    lastConnectedUrl := decoded;
  end;

  if decoded.protocol='https' then begin
    if checkSSLCertificates then
      hfile := HttpOpenRequest(hLastConnection, pchar(method), pchar(decoded.path), nil, pchar(lastRefererUrl.combined), ppchar(@defaultAccept[low(defaultAccept)]), INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_AUTO_REDIRECT or INTERNET_FLAG_SECURE , 0)
     else
      hfile := HttpOpenRequest(hLastConnection, pchar(method), pchar(decoded.path), nil, pchar(lastRefererUrl.combined), ppchar(@defaultAccept[low(defaultAccept)]), INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_AUTO_REDIRECT or INTERNET_FLAG_SECURE or INTERNET_FLAG_IGNORE_CERT_CN_INVALID  or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID, 0)
  end else
    hfile := HttpOpenRequest(hLastConnection, pchar(method), pchar(decoded.path), nil, pchar(lastRefererUrl.combined), ppchar(@defaultAccept[low(defaultAccept)]), INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_AUTO_REDIRECT, 0);

  if not assigned(hfile) then
    raise EW32InternetException.create(format(rsReceivingFrom0SFaile, [decoded.combined])); //'Can''t connect');


  cookiestr:=makeCookieHeader;
  if cookiestr<>'' then
    HttpAddRequestHeaders(hfile,@cookiestr[1],length(cookiestr),HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);
  overridenPostHeader := postHeader;
  for i:=0 to additionalHeaders.Count - 1 do
    if not striBeginsWith(additionalHeaders[i], 'Content-Type') then
      HttpAddRequestHeaders(hfile, pchar(additionalHeaders), length(additionalHeaders[i]), HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD)
     else
      overridenPostHeader := trim(strCopyFrom(additionalHeaders[i], pos(':', additionalHeaders[i])+1));



  for i := 1 to 2 do begin //repeat if ssl certificate is wrong
    if data='' then
      callResult:= httpSendRequest(hfile, nil,0,nil,0)
     else
      callResult:= httpSendRequest(hfile, pchar(overridenPostHeader), Length(overridenPostHeader), @data[1], Length(data));

    if callResult then break;

    if not checkSSLCertificates then begin
      //as suggested by http://msdn.microsoft.com/en-us/subscriptions/aa917690.aspx
      temp := getLastError;
      if (temp = ERROR_INTERNET_INVALID_CA) or (temp = ERROR_INTERNET_SEC_CERT_REV_FAILED) or (temp = ERROR_INTERNET_SEC_CERT_NO_REV) then begin
        dwContentLength := sizeof(dwNumber);
        InternetQueryOption (hfile, INTERNET_OPTION_SECURITY_FLAGS, @dwNumber, dwContentLength);
        dwNumber := dwNumber or SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_REVOCATION;
        InternetSetOption (hfile, INTERNET_OPTION_SECURITY_FLAGS, @dwNumber, sizeof (dwNumber) );
        continue;
      end;
    end;

    raise EW32InternetException.create();
  end;
      
  lastRefererUrl := decoded;
  lastRefererUrl.username:=''; lastRefererUrl.password:=''; lastRefererUrl.linktarget:=''; //keep this secret

  dwIndex  := 0;
  dwCodeLen := 10;
  if not HttpQueryInfo(hfile, HTTP_QUERY_STATUS_CODE, @dwcode, dwcodeLen, dwIndex) then
    raise EW32InternetException.create();
  res := pchar(@dwcode);

  lastHTTPResultCode := StrToIntDef(res, -1);

  if (lastHTTPResultCode = 200) or (lastHTTPResultCode = 301) or (lastHTTPResultCode = 302) or (lastHTTPResultCode = 303) or (lastHTTPResultCode = 307) then begin
    dwNumber := sizeof(databuffer)-1;
    if HttpQueryInfo(hfile,HTTP_QUERY_RAW_HEADERS_CRLF,@databuffer,dwNumber,dwindex) then
      parseHeaderForCookies(databuffer) //handle cookies ourself, our we could not have separate cookies for different connections
     else
      dwNumber := 0;
  end else dwNumber := 0;

  Result:='';
  if ((lastHTTPResultCode = 301) or (lastHTTPResultCode = 302) or (lastHTTPResultCode = 303) or (lastHTTPResultCode = 307)) and (redirectionCount > 0) then begin
    InternetCloseHandle(hfile);
    //handle redirection ourself, or we could not read cookies transmitted during redirections (for videlibri ubfu)
    if dwNumber = 0 then exit;
    newurl := parseHeaderForLocation(databuffer);
    if newurl = '' then exit('');
    result := doTransferRec('GET', decoded.resolved(trim(newurl)), '', redirectionCount - 1);
    exit;
  end else if (lastHTTPResultCode =200) or (lastHTTPResultCode = 302) then begin
    if assigned(OnProgress) then begin
      dwCodeLen := 15;
      HttpQueryInfo(hfile, HTTP_QUERY_CONTENT_LENGTH, @dwcode, dwcodelen, dwIndex);
      res := pchar(@dwcode);
      dwContentLength:=StrToIntDef(res,1*1024*1024);
      OnProgress(self,0,dwContentLength);
    end;
    dwRead:=0;
    dwNumber := sizeof(databuffer)-1;
    SetLastError(0);
    while (InternetReadfile(hfile,@databuffer,dwNumber,DwRead)) and (dwread>0) do begin
      temp:=length(result);
      setLength(result,temp+dwRead);
      move(dataBuffer[0],result[temp+1],dwRead);
      if assigned(OnProgress) then
        OnProgress(self,length(result),dwContentLength);
    end;
  end else if res='0' then
    raise EW32InternetException.create(rsInternetRequestFaile)
   else
    raise EW32InternetException.create(format(rsHTTPErrorCode0SNWhen, [res, decoded.combined]), StrToIntDef(res, 999));

  lastHTTPHeaders.Clear;
  if not HttpQueryInfo(hfile, HTTP_QUERY_RAW_HEADERS_CRLF, @databuffer, @i, nil) then
    if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (i > 0) then begin
      setlength(headerOut, i+1);
      HttpQueryInfo(hfile, HTTP_QUERY_RAW_HEADERS_CRLF, @headerOut[1], @i, nil);
      lastHTTPHeaders.Text:=headerOut;
    end;


  InternetCloseHandle(hfile);
end;

function TW32InternetAccess.doTransfer(method:string; const url: TDecodedUrl;data:string): string;
begin
  result := doTransferRec(method, url, data, 10);
end;


constructor TW32InternetAccess.create();
var proxyStr:string;
    timeout: longint;
begin
  FLastHTTPHeaders := TStringList.Create;
  additionalHeaders := TStringList.Create;
  internetConfig:=@defaultInternetConfiguration;
  if defaultInternetConfiguration.userAgent='' then
    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible)';
  if defaultInternetConfiguration.tryDefaultConfig then
    hSession:=InternetOpen(pchar(defaultInternetConfiguration.userAgent),
                            INTERNET_OPEN_TYPE_PRECONFIG,
                            nil,nil,0)
  else if defaultInternetConfiguration.useProxy then begin
    if defaultInternetConfiguration.proxyHTTPName='' then proxyStr:=''
    else begin
      if pos('//',defaultInternetConfiguration.proxyHTTPName)>0 then
        proxyStr:='http='+defaultInternetConfiguration.proxyHTTPName
       else
        proxyStr:='http=http://'+defaultInternetConfiguration.proxyHTTPName;
      if defaultInternetConfiguration.proxyHTTPPort<>'' then
        proxyStr:=proxyStr+':'+defaultInternetConfiguration.proxyHTTPPort;
    end;

    if defaultInternetConfiguration.proxyHTTPSName<>'' then begin
      if pos('//',defaultInternetConfiguration.proxyHTTPSName)>0 then
        proxyStr:=proxyStr+' https='+defaultInternetConfiguration.proxyHTTPSName
       else
        proxyStr:=proxyStr+' https=https://'+defaultInternetConfiguration.proxyHTTPSName;
      if defaultInternetConfiguration.proxyHTTPSPort<>'' then
        proxyStr:=proxyStr+':'+defaultInternetConfiguration.proxyHTTPSPort;
    end;

    hSession:=InternetOpen(pchar(defaultInternetConfiguration.userAgent),
                            INTERNET_OPEN_TYPE_PROXY,
                            pchar(proxyStr),nil,0)
  end else begin
    hSession:=InternetOpen(pchar(defaultInternetConfiguration.userAgent),
                            INTERNET_OPEN_TYPE_DIRECT,
                            nil,nil,0)
  end;
  if hSession=nil then
    raise EW32InternetException.create(rsFailedToConnectToThe, true);
  hLastConnection:=nil;
  newConnectionOpened:=false;
  timeout:=2*60*1000;
  InternetSetOption(hSession,INTERNET_OPTION_RECEIVE_TIMEOUT,@timeout,4);
  checkSSLCertificates := false;
end;


destructor TW32InternetAccess.destroy;
begin
  if hLastConnection<>nil then
    InternetCloseHandle(hLastConnection);
  InternetCloseHandle(hsession);
  FLastHTTPHeaders.Free;
  additionalHeaders.Free;
  inherited;
end;

function TW32InternetAccess.needConnection(): boolean;
begin
  if existsConnection() then exit(true);
//  InternetAutodial(0,0);
  result:=InternetAttemptConnect(0)=ERROR_SUCCESS;
  if result then begin
    result:=existsConnection();
    newConnectionOpened:=true;
  end;
end;

procedure TW32InternetAccess.closeOpenedConnections();
begin
  if newConnectionOpened then begin
    //todo: close inet con
  end;
end;

function TW32InternetAccess.internalHandle: TObject;
begin
  result:=tobject(hLastConnection);
end;
{$ENDIF}

end.

