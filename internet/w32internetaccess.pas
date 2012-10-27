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

{$IFDEF COMPILE_W32_INTERNETACCESS}
uses
  windows,Classes, SysUtils,dialogs,
  wininet,
  internetaccess;


type
  EW32InternetException=class(EInternetException)
    constructor create();
    constructor create(s:string;showError:boolean=false);
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
    lastProtocol,lastHost:string;
    lastCompleteUrl: string;
    newConnectionOpened:boolean;
    FLastHTTPHeaders: TStringList;
    function GetLastHTTPHeaders: TStringList; override;
    function doTransfer(method:THTTPConnectMethod; protocol,host,url: string;data:string): string;override;
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
      s:='Keine Internethandles mehr verfÃ¼gbar.'#13#10'Bitte starten sie den Computer neu und versuchen es erneut';
    ERROR_INTERNET_TIMEOUT:
      s:='Time out, meine Verbindungsanfrage wurde nicht beantwortet'#13#10'Verbindungsversuch fehlgeschlagen.';
    ERROR_INTERNET_EXTENDED_ERROR: begin
      setlength(s,4096);
      temp2:=length(s);
      InternetGetLastResponseInfo({$ifndef DELPHI_WININET}@{$endif}temp1,@s[1],temp2);
      setlength(s,temp2);
      s:='Erweiterter Internetfehler: '#13#10+s;
    end;
    ERROR_INTERNET_INTERNAL_ERROR:
      s:='Interner Fehler in WinInet.dll'#13#10'Fehler bitte melden';
    ERROR_INTERNET_INVALID_URL:
      s:='Falsche URL'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_UNRECOGNIZED_SCHEME:
      s:='Unbekanntes URL-Schema'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_NAME_NOT_RESOLVED:
      s:='DNS fehlgeschlagen'#13#10'Wahrscheinlich keine Verbindung zum Internet';
    ERROR_INTERNET_PROTOCOL_NOT_FOUND:
      s:='Protokoll nicht gefunden'#13#10'Entweder Programmierfehler oder Fehler in Windows handelt';
    ERROR_INTERNET_INVALID_OPTION:
      s:='Falscher Parameter'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_BAD_OPTION_LENGTH:
      s:='Falscher Parameterlänge'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_OPTION_NOT_SETTABLE:
      s:='The request option can not be set, only queried.'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_SHUTDOWN:
      s:='Windowsinternetfunktionen werden leider gerade deaktiviert.'#13#10'Bitte starten sie den Computer neu und versuchen es erneut';
    ERROR_INTERNET_INVALID_OPERATION:
      s:='Ungültige Operation'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_OPERATION_CANCELLED:
      s:='Verbindung abgebrochen'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen (wenn auch harmlosen) Programmierfehler handelt';
    ERROR_INTERNET_INCORRECT_HANDLE_TYPE:
      s:='Falscher Handle für Operation'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_INCORRECT_HANDLE_STATE:
      s:='Handle im falschen Modus'#13#10'Bitte nochmal versuchen';
    ERROR_INTERNET_NOT_PROXY_REQUEST        :
      s:='Verbindungsart kann nicht über einen Proxy übertragen werden'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND:
      s:='Wichtigen Registryeintrag nicht gefunden'#13#10'Ursache: Wahrscheinlich Fehler in Windows oder im Internet Explorer';
    ERROR_INTERNET_BAD_REGISTRY_PARAMETER:
      s:='Ein wichtiger Registryeintrag hat den falsche Wert'#13#10'Ursache: Wahrscheinlich Fehler in Windows oder im Internet Explorer';
    ERROR_INTERNET_NO_DIRECT_ACCESS:
      s:='Direkte Verbindung momentan nicht möglich'#13#10'Bitte nochmal versuchen';
    ERROR_INTERNET_INCORRECT_FORMAT:
      s:='Falsches Format für Anfrage'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_ITEM_NOT_FOUND:
      s:='Internet_Item_Not_Found'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INTERNET_CANNOT_CONNECT:
      s:='Internetverbindung konnte nicht hergestellt werden'#13#10'Bitte nochmal versuchen oder ignorieren';
    ERROR_INTERNET_CONNECTION_ABORTED:
      s:='Internetverbindung unterbrochen'#13#10'Bitte nochmal versuchen oder beenden';
    ERROR_INTERNET_CONNECTION_RESET:
      s:='Internetverbindung unterbrochen/reseted'#13#10'Bitte nochmal versuchen oder ignorieren';
    ERROR_INTERNET_FORCE_RETRY:
      s:='Windows fordert Wiederholung der Anforderung'#13#10'Kann wahrscheinlich wie die meisten Windowsfehler durch einen Neustart behoben werden';
    ERROR_INTERNET_MIXED_SECURITY:
      s:='Teilweise unsicherer Inhalt';
    {ERROR_INTERNET_SSL_CERT_CN_INVALID:
      s:='Sicherheitszertifikat des Servers ungültig';}
    ERROR_INTERNET_HANDLE_EXISTS :
      s:='Handle existiert bereits'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_HTTP_HEADER_NOT_FOUND :
      s:='Header nicht gefunden'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_HTTP_DOWNLEVEL_SERVER:
      s:='Server hat mit Header-losen Daten geantwortet'#13#10'Bitte nochmal versuchen';
    ERROR_HTTP_INVALID_SERVER_RESPONSE:
      s:='Serverantwort nicht verstanden'#13#10'Bitte nochmal versuchen';
    ERROR_HTTP_INVALID_HEADER:
      s:='Header ungültig'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_HTTP_INVALID_QUERY_REQUEST:
      s:='Falsche Parameter bei HttpQueryInfo'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_HTTP_HEADER_ALREADY_EXISTS:
      s:='Headervalue existiert bereits'#13#10'Fehler bitte melden, da es sich wahrscheinlich um einen Programmierfehler handelt';
    ERROR_INVALID_HANDLE:
      s:='Handle bereits geschlossen'#13#10'Bitte nochmal versuchen';
    ERROR_INVALID_PARAMETER:
      s:='UngÃ¼ltiger Parameter';
    else
      s:='Unbekannter Internetfehler: ' + IntToStr(GetLastError);
  end;
  inherited create(s);
end;

constructor EW32InternetException.create(s:string;showError:boolean=false);
begin
  create();
  details:='Windowsfehlermeldung: '+Message;
  Message:=s;
end;


{$ifdef debug}procedure saveAbleURL(var url:string);
var temp:integer;
begin
  for temp:=1 to length(url) do
    if url[temp] in ['/','?','&',':','\','*','"','<','>','|'] then
      url[temp]:='#';

end;
function readString(url: string):string;
var tempdebug:TFileStream;
begin
  saveAbleURL(url);
  tempdebug:=TFileStream.create('E:\temp\delete\'+url,fmOpenRead);
  setlength(result,tempdebug.size);
  tempdebug.readBuffer(result[1],tempdebug.size);
  tempdebug.free;
end;
procedure writeString(url,value: string);
var tempdebug:TFileStream;
begin
  saveAbleURL(url);
  url:=copy(url,1,200);
  url:=TEMPORARY_DIRECTORY+url;
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
{$endif}

function TW32InternetAccess.GetLastHTTPHeaders: TStringList;
begin
  result := FLastHTTPHeaders;
end;

function TW32InternetAccess.doTransfer(method:THTTPConnectMethod; protocol,host,url: string;data:string): string;
const postHeader='Content-Type: application/x-www-form-urlencoded';
var
  databuffer : array[0..4095] of char;
  hfile: hInternet;
  dwindex,dwcodelen,dwread,dwNumber,temp,dwContentLength: cardinal;
  tempPort: integer;
  dwcode : array[1..20] of char;
  res    : pchar;
  cookiestr:string;
  operation: string;
  callResult: boolean;
  htmlOpenTagRead: boolean; htmlClosingTagRead: boolean;
  i: Integer;
  headerOut: string;
  label getMore;
begin
  if method=hcmPost then operation:='POST'
  else operation:='GET';
//  {$ifdef debug}
//  writeString(host+'_'+url+'_pre',operation+#13#10+host+#13#10+url+#13#10+data);
//  {$endif}
  {$ifdef simulateInet}
  if data='' then
    result:=readString(url)
  else
    result:=readString(url+'##DATA##'+data+'##DATA-END##');
  exit;
  {$endif}

  if not assigned(hSession) Then
    raise EW32InternetException.create('No internet session created');

  if (lastProtocol<>protocol) or (lastHost<>host) then begin
    if hLastConnection<>nil then
      InternetCloseHandle(hLastConnection);
    lastProtocol:=protocol;
    lastHost:=host;
    if protocol='http' then begin
      tempPort:=80;
      temp:=INTERNET_SERVICE_HTTP;
    end else if protocol='https' then begin
      tempPort:=443;
      temp:=INTERNET_SERVICE_HTTP;
    end;
    if pos(':',host)>0 then begin
      tempPort:=StrToIntDef(copy(host,pos(':',host)+1,length(host)),-1);
      if tempPort=-1 then
        raise EInternetException.create('Invalid port in url: '+protocol+host+url);
      host:=copy(host,1,pos(':',host)-1);
    end;
    lastCompleteUrl:='';
    hLastConnection:=InternetConnect(hSession,pchar(host),tempPort,'',nil,temp,0,0);
    if hLastConnection=nil then
      raise EW32InternetException.create('Verbindungsaufbau zu ' + host + ' fehlgeschlagen');
  end;

  if protocol='https' then
    hfile := HttpOpenRequest(hLastConnection, pchar(operation), pchar(url), nil, pchar(lastCompleteUrl), nil, INTERNET_FLAG_NO_COOKIES or  INTERNET_FLAG_RELOAD or INTERNET_FLAG_SECURE, 0)
   else
    hfile := HttpOpenRequest(hLastConnection, pchar(operation), pchar(url), nil, pchar(lastCompleteUrl), nil, INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_RELOAD, 0);

  if not assigned(hfile) then
    raise EW32InternetException.create('Aufruf von '+ url + ' fehlgeschlagen');//'Can''t connect');

  cookiestr:=makeCookieHeader;
  if cookiestr<>'' then
    HttpAddRequestHeaders(hfile,@cookiestr[1],length(cookiestr),HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);
  for i:=0 to additionalHeaders.Count - 1 do
    HttpAddRequestHeaders(hfile, pchar(additionalHeaders), length(additionalHeaders[i]), HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);

  if data='' then
    callResult:= httpSendRequest(hfile, nil,0,nil,0)
   else
    callResult:= httpSendRequest(hfile, postHeader, Length(postHeader), @data[1], Length(data));
  if not callResult then
    raise EW32InternetException.create();
      
  lastCompleteUrl:=protocol+'://'+host+url;

  dwIndex  := 0;
  dwCodeLen := 10;
  if not HttpQueryInfo(hfile, HTTP_QUERY_STATUS_CODE, @dwcode, dwcodeLen, dwIndex) then
    raise EW32InternetException.create();
  res := pchar(@dwcode);

  lastHTTPResultCode := StrToIntDef(res, -1);

  Result:='';
  if (res ='200') or (res ='302') then begin
    dwNumber := sizeof(databuffer)-1;
    if HttpQueryInfo(hfile,HTTP_QUERY_RAW_HEADERS_CRLF,@databuffer,dwNumber,dwindex) then
      parseHeaderForCookies(databuffer);

    if assigned(OnProgress) then begin
      dwCodeLen := 15;
      HttpQueryInfo(hfile, HTTP_QUERY_CONTENT_LENGTH, @dwcode, dwcodelen, dwIndex);
      res := pchar(@dwcode);
      dwContentLength:=StrToIntDef(res,1*1024*1024);
      OnProgress(self,0,dwContentLength);
    end;
    dwRead:=0;
    dwNumber := sizeof(databuffer)-1;
    htmlOpenTagRead:=false;
    htmlClosingTagRead:=false;

   getMore:
   SetLastError(0);
    while (InternetReadfile(hfile,@databuffer,dwNumber,DwRead)) and (dwread>0) do begin
      temp:=length(result);
      setLength(result,temp+dwRead);
      move(dataBuffer[0],result[temp+1],dwRead);
      if temp=0 then
        htmlOpenTagRead:=pos('<html',lowercase(databuffer))>0; //check if it is html file
      if htmlOpenTagRead then
        htmlClosingTagRead:=pos('</html',lowercase(databuffer))>0;
      if assigned(OnProgress) then
        OnProgress(self,length(result),dwContentLength);
//      if length(result)<2*dwNumber;
    end;
    {$ifdef debug}writeString('res_'+host+'_'+url,inttostr(GetTickCount)+': '+ inttostr(getlasterror));{$endif}
    if htmlOpenTagRead and not htmlClosingTagRead then begin
      htmlOpenTagRead:=false;
      sleep(1500);
      goto getmore;
    end;
  end else if res='0' then
    raise EW32InternetException.create('Internetverbindung fehlgeschlagen')
   else
    raise EW32InternetException.create('HTTP Error code: '+res+#13#10+'Beim Aufruf von '+protocol+'://'+host+url);

  lastHTTPHeaders.Clear;
  if not HttpQueryInfo(hfile, HTTP_QUERY_RAW_HEADERS_CRLF, @databuffer, @i, nil) then
    if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (i > 0) then begin
      setlength(headerOut, i+1);
      HttpQueryInfo(hfile, HTTP_QUERY_RAW_HEADERS_CRLF, @headerOut[1], @i, nil);
      lastHTTPHeaders.Text:=headerOut;
    end;


  InternetCloseHandle(hfile);

  {$ifdef debug}
  if data='' then
    writeString(host+'_'+url,result)
   else
    writeString(host+'_'+url+'##DATA##'+data+'##DATA-END##',result);
  {$endif}
end;

constructor TW32InternetAccess.create();
var proxyStr:string;
    timeout: longint;
begin
  {$ifdef debug}randomize;{$endif}
  lastCompleteUrl:='';
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
    raise EW32InternetException.create('Leider konnte keine Internetverbindung aufgebaut werden.'#13#10'Bitte überprüfen Sie, ob Sie im Internet sind.',true);
  hLastConnection:=nil;
  lastHost:='';
  newConnectionOpened:=false;
  timeout:=2*60*1000;
  InternetSetOption(hSession,INTERNET_OPTION_RECEIVE_TIMEOUT,@timeout,4)
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

