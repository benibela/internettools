{Copyright (C) 2006-2017  Benito van der Zander

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
{$IFDEF WINDOWS}
{$DEFINE COMPILE_W32_INTERNETACCESS}  //If this unit should be compiled
{$DEFINE USE_WININET_WRAPPER}
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
  //**@abstract(Internet access class using the wininet library)
  //**Set defaultInternetAccessClass to TW32InternetAccess if
  //**you want to use wininet to connect to internet@br
  //**You probably need an additional wininet header (should be contained somewhere
  //**in freepascal, if not you can use the Delphi unit)
  TW32InternetAccess=class(TInternetAccess)
  protected
    hSession,hLastConnection: hInternet;
    newConnectionOpened:boolean;
    lastServer: TDecodedUrl;
    procedure doTransferUnchecked(method:string; const decoded: TDecodedUrl; const data: TInternetAccessDataBlock); override;
    function getLastErrorDetails: string; override;
  public
    constructor create();override;
    destructor destroy;override;
    function needConnection():boolean;override;
    procedure closeOpenedConnections();override;

    function internalHandle: TObject; override;
  end;
  TW32InternetAccessClass = class of TW32InternetAccess;

const TEMPORARY_DIRECTORY='T:\theInternet\';

{$ENDIF}
implementation

uses bbutils//, bbdebugtools
  ;

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
  rsWindowsCode = 'Windows error code: ';
  rsWindowsErrorMessage = 'Windows error message: ';



{$IFDEF COMPILE_W32_INTERNETACCESS}

type TAddHeaderData = record
  overridenPostHeader: string;
  hfile: hinternet;
end;
  PAddHeaderData = ^TAddHeaderData;

procedure addHeader(data: pointer; headerKind: TW32InternetAccess.THeaderKind; const name, value: string);
var
  headerLine: string;
begin
  with PAddHeaderData(data)^ do begin
    case headerKind of
      iahContentType: begin
        overridenPostHeader := TW32InternetAccess.makeHeaderLine(headerKind, value);
        exit;
      end;
      else headerLine := TW32InternetAccess.makeHeaderLine(name, value) + #13#10;
    end;
    HttpAddRequestHeadersA(hfile, @headerLine[1],length(headerLine),HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);
  end;
end;


procedure TW32InternetAccess.doTransferUnchecked(method: string; const decoded: TDecodedUrl; const data: TInternetAccessDataBlock);
const defaultAccept: array[1..6] of ansistring = ('text/html', 'application/xhtml+xml', 'application/xml', 'text/*', '*/*', ''); //just as default. it will be overriden
var
  databuffer : array[0..4095] of char;
  hfile: hInternet;
  dwindex,dwcodelen,dwRead,dwNumber,temp: cardinal;
  tempPort: integer;
  dwcode : array[1..20] of char;
  res    : pchar;
  callResult: boolean;
  i: Integer;
  headerOut: string;
  headerAdd: TAddHeaderData;
begin
  if not assigned(hSession) Then exit;

  if (lastServer.Protocol<>decoded.protocol) or (lastServer.Host<>decoded.host) or (lastServer.Port <> decoded.port)
     or (lastServer.username <> decoded.username) or (lastServer.password <> decoded.password) then begin
    if hLastConnection<>nil then
      InternetCloseHandle(hLastConnection);
    if striequal(decoded.protocol, 'https') then begin
      tempPort:=443;
      temp:=INTERNET_SERVICE_HTTP;
    end else {if striequal(decoded.protocol, 'http') then} begin //if there is no else branch fpc gives pointless warnings
      tempPort:=80;
      temp:=INTERNET_SERVICE_HTTP;
    end;
    if decoded.port <> '' then
      tempPort := StrToIntDef(decoded.port, 80);
    //huh? wininet seems to remember the password, if it is set once and continues sending it with new requests, even if is unset. (tested with WINE and Windows 7)
    if (decoded.username = '') and (decoded.password = '') then hLastConnection:= InternetConnectA(hSession,pchar(decoded.host),tempPort,nil,            nil,temp,0,0)
    else if decoded.password = '' then                  hLastConnection:= InternetConnectA(hSession,pchar(decoded.host),tempPort,pchar(strUnescapeHex(decoded.username, '%')),nil,temp,0,0)
    else                                        hLastConnection:= InternetConnectA(hSession,pchar(decoded.host),tempPort,pchar(strUnescapeHex(decoded.username, '%')),pchar(strUnescapeHex(decoded.password, '%')),temp,0,0);
    if hLastConnection=nil then begin
      lastHTTPResultCode := -2;
      lastErrorDetails:=rsConnectingTo0SFailed;
      exit;
    end;
    lastServer := decoded; //remember to which server the connection points. We cannot use lastURLDecoded, since the connection has already changed, but lastURLDecoded is only set after redirects
  end;

  if striequal(decoded.protocol, 'https') then begin
    if internetConfig^.checkSSLCertificates then
      hfile := HttpOpenRequestA(hLastConnection, pchar(method), pchar(decoded.path+decoded.params), nil, pchar(lastURLDecoded.combined), ppchar(@defaultAccept[low(defaultAccept)]), INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_AUTO_REDIRECT or INTERNET_FLAG_SECURE , 0)
     else
      hfile := HttpOpenRequestA(hLastConnection, pchar(method), pchar(decoded.path+decoded.params), nil, pchar(lastURLDecoded.combined), ppchar(@defaultAccept[low(defaultAccept)]), INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_AUTO_REDIRECT or INTERNET_FLAG_SECURE or INTERNET_FLAG_IGNORE_CERT_CN_INVALID  or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID, 0)
  end else
    hfile := HttpOpenRequestA(hLastConnection, pchar(method), pchar(decoded.path+decoded.params), nil, pchar(lastURLDecoded.combined), ppchar(@defaultAccept[low(defaultAccept)]), INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_AUTO_REDIRECT, 0);

  if not assigned(hfile) then begin
    lastErrorDetails := rsReceivingFrom0SFaile;
    exit;
  end;

  headerAdd.hfile := hfile;
  enumerateAdditionalHeaders(decoded, @addHeader, data.count > 0, @headerAdd);

  for i := 1 to 2 do begin //repeat if ssl certificate is wrong
    if data.count = 0 then
      callResult:= httpSendRequestA(hfile, nil,0,nil,0)
     else
      callResult:= httpSendRequestA(hfile, pchar(headerAdd.overridenPostHeader), Length(headerAdd.overridenPostHeader), data.data, data.count);

    if callResult then break;

    if not internetConfig^.checkSSLCertificates then begin
      //as suggested by http://msdn.microsoft.com/en-us/subscriptions/aa917690.aspx
      temp := getLastError;
      if (temp = ERROR_INTERNET_INVALID_CA) or (temp = ERROR_INTERNET_SEC_CERT_REV_FAILED) or (temp = ERROR_INTERNET_SEC_CERT_NO_REV) then begin
        temp := sizeof(dwNumber);
        InternetQueryOptionA(hfile, INTERNET_OPTION_SECURITY_FLAGS, @dwNumber, @temp);
        dwNumber := dwNumber or SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_REVOCATION;
        InternetSetOptionA(hfile, INTERNET_OPTION_SECURITY_FLAGS, @dwNumber, sizeof (dwNumber) );
        continue;
      end;
    end;

    lastHTTPResultCode := -3;
    exit;
  end;

  dwIndex  := 0;
  dwCodeLen := 10;
  if not HttpQueryInfoA(hfile, HTTP_QUERY_STATUS_CODE, @dwcode, @dwcodeLen, @dwIndex) then
    exit;
  res := pchar(@dwcode);

  lastHTTPResultCode := StrToIntDef(res, -4);
  lastHTTPHeaders.Clear;
  dwNumber := 0;
  if not HttpQueryInfoA(hfile, HTTP_QUERY_RAW_HEADERS_CRLF, @databuffer, @i, nil) then
    if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (i > 0) then begin
      setlength(headerOut, i+1);
      HttpQueryInfoA(hfile, HTTP_QUERY_RAW_HEADERS_CRLF, @headerOut[1], @i, nil);
      lastHTTPHeaders.Text:=headerOut;
    end;

  if method <> 'HEAD' then begin
    dwRead:=0;
    SetLastError(0);
    while true do begin
      if InternetReadfile(hfile,@databuffer,sizeof(databuffer)-1,@DwRead) then begin
        if dwRead = 0 then
          break; //this is end-of-file condition according to MSDN (InternetReadFile must return true)
        writeBlock(databuffer[0], dwRead);
      end else if InternetQueryDataAvailable(hfile, @dwRead, 0, 0) then begin
        if dwRead = 0 then //the above condition never occurs (at least on WINE). So explicitly check for more data. (this is supposed to prevent problems with chunked transfers)
          break;
      end else break;
    end;
  end;

  InternetCloseHandle(hfile);
end;

function TW32InternetAccess.getLastErrorDetails: string;
var
  temp1,templen: dword;
begin
  result:=lastErrorDetails;
  case GetLastError of
    ERROR_INTERNET_OUT_OF_HANDLES:
      result:=rsNoInternetHandlesAva;
    ERROR_INTERNET_TIMEOUT:
      result:=rsConnectionTimedOut;
    ERROR_INTERNET_EXTENDED_ERROR: begin
      setlength(result,4096);
      templen:=length(result);
      InternetGetLastResponseInfoA({$ifndef DELPHI_WININET}@{$endif}temp1,@result[1],@templen);
      setlength(result,templen);
      result:=rsExtendedInternetConn+#13#10+result;
    end;
    ERROR_INTERNET_INTERNAL_ERROR:
      result:=rsInternalErrorInWinin;
    ERROR_INTERNET_INVALID_URL:
      result:=rsInvalidUrl;
    ERROR_INTERNET_UNRECOGNIZED_SCHEME:
      result:=rsUnrecognizedUrlSchem;
    ERROR_INTERNET_NAME_NOT_RESOLVED:
      result:=rsDNSRequestFailedProb;
    ERROR_INTERNET_PROTOCOL_NOT_FOUND:
      result:=rsProtocolNotFound;
    ERROR_INTERNET_INVALID_OPTION:
      result:=rsInvalidParameter;
    ERROR_INTERNET_BAD_OPTION_LENGTH:
      result:=rsInvalidParameterLeng;
    ERROR_INTERNET_OPTION_NOT_SETTABLE:
      result:=rsTheRequestOptionCanN;
    ERROR_INTERNET_SHUTDOWN:
      result:=rsInternetHasBeenShutd;
    ERROR_INTERNET_INVALID_OPERATION:
      result:=rsInvalidInternetOpera;
    ERROR_INTERNET_OPERATION_CANCELLED:
      result:=rsOperationCancelled;
    ERROR_INTERNET_INCORRECT_HANDLE_TYPE:
      result:=rsInvalidHandle;
    ERROR_INTERNET_INCORRECT_HANDLE_STATE:
      result:=rsIncorrectHandle;
    ERROR_INTERNET_NOT_PROXY_REQUEST        :
      result:=rsInvalidProxyUsage;
    ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND:
      result:=rsRegistryCorrupted;
    ERROR_INTERNET_BAD_REGISTRY_PARAMETER:
      result:=rsCorruptedRegistry;
    ERROR_INTERNET_NO_DIRECT_ACCESS:
      result:=rsDirectInternetAccess;
    ERROR_INTERNET_INCORRECT_FORMAT:
      result:=rsIncorrectFormatForHt;
    ERROR_INTERNET_ITEM_NOT_FOUND:
      result:=rsInternetItemNotFound;
    ERROR_INTERNET_CANNOT_CONNECT:
      result:=rsFailedToCreateConnec;
    ERROR_INTERNET_CONNECTION_ABORTED:
      result:=rsInternetConnectionAb;
    ERROR_INTERNET_CONNECTION_RESET:
      result:=rsInternetConnectionRe;
    ERROR_INTERNET_FORCE_RETRY:
      result:=rsNeedRetriedRequest;
    ERROR_INTERNET_MIXED_SECURITY:
      result:=rsUnsecurePage;
    {ERROR_INTERNET_SSL_CERT_CN_INVALID:
      result:='Sicherheitszertifikat des Servers ungültig';}
    ERROR_INTERNET_HANDLE_EXISTS :
      result:=rsInternetHandleAlread;
    ERROR_HTTP_HEADER_NOT_FOUND :
      result:=rsHTTPHeaderNotFound;
    ERROR_HTTP_DOWNLEVEL_SERVER:
      result:=rsServerAnsweredWithou;
    ERROR_HTTP_INVALID_SERVER_RESPONSE:
      result:=rsServerResponseIsInva;
    ERROR_HTTP_INVALID_HEADER:
      result:=rsInvalidHttpHeader;
    ERROR_HTTP_INVALID_QUERY_REQUEST:
      result:=rsInvalidHttpQueryRequ;
    ERROR_HTTP_HEADER_ALREADY_EXISTS:
      result:=rsHttpHeaderAlreadyExi;
    ERROR_INVALID_HANDLE:
      result:=rsInvalidInternetHandl;
    ERROR_INVALID_PARAMETER:
      result:=rsInvalidParameter;
    ERROR_INTERNET_SEC_CERT_NO_REV:
      result:=rsTheSSLCertificateWas;
    ERROR_INTERNET_SEC_CERT_CN_INVALID:
      result:=rsInvalidSSLCertificat;
    ERROR_INTERNET_SEC_CERT_DATE_INVALID :
      result:= rsInvalidSSLCertificat2;
    ERROR_INTERNET_SEC_CERT_REV_FAILED:
      result:= rsFailedToValidateSSLC;
    else if GetLastError <> 0 then
      result:='('+ rsWindowsCode + IntToStr(GetLastError)+')';
  end;
end;

constructor TW32InternetAccess.create();
var proxyStr:string;
    timeout: longint;
begin
  init;
  if defaultInternetConfiguration.tryDefaultConfig then
    hSession:=InternetOpenA(pchar(defaultInternetConfiguration.userAgent),
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

    if defaultInternetConfiguration.proxySOCKSName<>'' then begin
      if pos('//',defaultInternetConfiguration.proxySOCKSName)>0 then
        proxyStr:=proxyStr+' socks='+defaultInternetConfiguration.proxySOCKSName
       else
        proxyStr:=proxyStr+' socks=socks://'+defaultInternetConfiguration.proxySOCKSName;
      if defaultInternetConfiguration.proxySOCKSPort<>'' then
        proxyStr:=proxyStr+':'+defaultInternetConfiguration.proxySOCKSPort;
    end;

    hSession:=InternetOpenA(pchar(defaultInternetConfiguration.userAgent),
                            INTERNET_OPEN_TYPE_PROXY,
                            pchar(proxyStr),nil,0)
  end else begin
    hSession:=InternetOpenA(pchar(defaultInternetConfiguration.userAgent),
                            INTERNET_OPEN_TYPE_DIRECT,
                            nil,nil,0)
  end;
  if hSession=nil then
    raise EInternetException.create(rsFailedToConnectToThe + getLastErrorDetails);
  hLastConnection:=nil;
  newConnectionOpened:=false;
  timeout:=2*60*1000;
  InternetSetOptionA(hSession,INTERNET_OPTION_RECEIVE_TIMEOUT,@timeout,4);
end;


destructor TW32InternetAccess.destroy;
begin
  if hLastConnection<>nil then
    InternetCloseHandle(hLastConnection);
  InternetCloseHandle(hsession);
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

initialization

{$IFDEF USE_WININET_WRAPPER}
defaultInternetAccessClass := TW32InternetAccess;
{$ENDIF}

end.


