{Copyright (C) 2009  Benito van der Zander

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
{**This unit contains the wrapper for synapse}
unit synapseinternetaccess;


{$mode objfpc}{$H+}

{$IFNDEF WIN32}
{$DEFINE COMPILE_SYNAPSE_INTERNETACCESS} //If this unit should be compiled. Not enabled on windows, since you can use w32internetaccess there
{$ENDIF}



interface

{$IFDEF COMPILE_SYNAPSE_INTERNETACCESS}

uses
  Classes, SysUtils, internetAccess,
  httpsend,  //this is the synapse http unit (from http://www.ararat.cz/synapse/doku.php/download)
  blcksock,
  ssl_openssl //needed for https
  ;

type

{ TSynapseInternetAccess }
//**@abstract(Internet access class using the synapse library)
//**Set defaultInternetAccessClass to TSynapseInternetAccess if
//**you want to use wininet to connect to internet@br
//**You also have to install the synapse package@br
//**In contrast to native synapse this will automatically load openssl
//**if it is called for the https protocal
TSynapseInternetAccess=class(TInternetAccess)
  procedure connectionStatus(Sender: TObject; Reason: THookSocketReason;
    const Value: String);
protected
  //synapse will automatically handle keep alive
  connection: THTTPSend;
  lastProgressLength,contentLength:longint;
  forwardProgressEvent: TProgressEvent;
  //lastCompleteUrl: string;
  //newConnectionOpened:boolean;
  function doTransferRec(method:string; url: TDecodedUrl; data:string; redirectionCount:longint): string;
  function doTransfer(method:string; const url: TDecodedUrl; data:string): string;override;
  function GetLastHTTPHeaders: TStringList; override;
public
  Referer: string;

  constructor create();override;
  destructor destroy;override;
  function needConnection():boolean;override;
  procedure closeOpenedConnections();override;

  function internalHandle: TObject; override;
end;
TSynapseInternetAccessClass = class of TSynapseInternetAccess;

{$ENDIF}

implementation

{$IFDEF COMPILE_SYNAPSE_INTERNETACCESS}

uses synautil,ssl_openssl_lib,bbutils{$ifndef win32},netdb{$endif};

{ TSynapseInternetAccess }

{$ifdef win32}
function checkEtcResolv(): boolean;
begin
  result := false;
end;
{$else}
var resolvConfFileAge: longint = 0;
    resolvConfCS: TRTLCriticalSection;
function checkEtcResolv(): boolean;
var resolvConf: string;
  newAge: LongInt;
begin
  {$if FPC_FULlVERSION >= 020600}
  resolvConf := netdb.EtcPath + netdb.SResolveFile;
  {$else}
  resolvConf:=netdb.SResolveFile;
  {$endif}
  newAge := FileAge(resolvConf);
  result := false;
  if newAge > resolvConfFileAge then begin
    EnterCriticalsection(resolvConfCS);
    try
      if newAge > resolvConfFileAge then begin
        SetLength(DNSServers, 0);
        result := GetDNSServers(resolvConf) > 0;
        resolvConfFileAge := newAge;
      end;
    finally
      LeaveCriticalsection(resolvConfCS);
    end;
  end;
end;
{$endif}

procedure TSynapseInternetAccess.connectionStatus(Sender: TObject;
  Reason: THookSocketReason; const Value: String);
var
  i: Integer;
begin
  if (FOnProgress=nil) or (connection=nil) then exit;
  if contentLength=-1 then begin
    for i:=0 to connection.Headers.Count-1 do
      if pos('content-length',lowercase(connection.Headers[i]))>0 then begin
        contentLength:=StrToIntDef(copy(connection.Headers[i],pos(':',connection.Headers[i])+1,length(connection.Headers[i])),-1);
        exit;
      end;
    if contentLength=-1 then exit;
  end;
  lastProgressLength:=connection.DownloadSize;
  FOnProgress(self, connection.DownloadSize, contentLength);
end;


function TSynapseInternetAccess.doTransferRec(method:string; url: TDecodedUrl; data: string; redirectionCount:longint): string;
  procedure initConnection;
  var
    i: Integer;
  begin
   connection.Clear;
   connection.AddPortNumberToHost:=false;
   if data <> '' then begin
     WriteStrToStream(connection.Document, data);
     connection.MimeType := 'application/x-www-form-urlencoded';
   end;
   if Referer <> '' then
     connection.Headers.Add('Referer: '+Referer);
   connection.Headers.add('Accept: text/html,application/xhtml+xml,application/xml,text/*,*/*');;
   connection.Protocol:='1.1';
   for i := 0 to additionalHeaders.Count - 1 do
     if not striBeginsWith(additionalHeaders[i], 'Content-Type') then
       connection.Headers.add(additionalHeaders[i])
     else
       connection.MimeType := trim(strCopyFrom(additionalHeaders[i], pos(':', additionalHeaders[i])+1));
  end;

var newurl: string;
  i: Integer;
  ok: Boolean;
begin
  result:='';
  contentLength:=-1;
  lastProgressLength:=-1;

  if url.path = '' then url.path:='/';

  if url.protocol = 'https' then
    if (not IsSSLloaded) then //check if ssl is actually loaded
       raise EInternetException.Create('Couldn''t load ssl libraries: libopenssl and libcrypto'#13#10'(Hint: install also the dev packages on Debian)');

  initConnection;
  ok := connection.HTTPMethod(method,url.combined);

  if (not ok) and (checkEtcResolv) then begin
    initConnection;
    ok := connection.HTTPMethod(method,url.combined);
  end;

  if ok then begin
    //for i:=0 to connection.Headers.Count-1 do
    //  writeln(connection.Headers[i]);
     if (connection.ResultCode = 200) {or ((connection.ResultCode = 302) and (connection.document.Size > 512))} then
       result:=ReadStrFromStream(connection.Document, connection.Document.Size)
      else begin
       if ((connection.ResultCode = 301) or (connection.ResultCode = 302) or (connection.ResultCode = 303) or (connection.ResultCode = 307)) and (redirectionCount > 0) then
         for i:=0 to connection.Headers.Count-1 do
           if stribeginswith(connection.Headers[i], 'Location:') then begin
             newurl := connection.Headers[i]; strSplitGet(':',newurl);
             exit(doTransferRec('GET', url.resolved(trim(newurl)), '', redirectionCount - 1));
           end;
       raise EInternetException.Create('Transfer failed: '+inttostr(connection.ResultCode)+': '+connection.ResultString+#13#10'when talking to: '+url.combined);
      end;
  end else
    raise EInternetException.Create('Connecting failed'#13#10'when talking to: '+url.combined);

  Referer:=url.combined;
  lastHTTPResultCode := connection.ResultCode;

  if (FOnProgress<>nil) and (lastProgressLength<connection.DownloadSize) then
    if contentLength=-1 then FOnProgress(self,connection.DownloadSize,connection.DownloadSize)
    else FOnProgress(self,connection.DownloadSize,contentLength);
end;

function TSynapseInternetAccess.doTransfer(method:string;const url: TDecodedUrl; data: string): string;
begin
  result:=doTransferRec(method, url, data, 10);
end;

function TSynapseInternetAccess.GetLastHTTPHeaders: TStringList;
begin
  result := connection.Headers;
end;

constructor TSynapseInternetAccess.create();
begin
  additionalHeaders := TStringList.Create;

  internetConfig:=@defaultInternetConfiguration;
  if defaultInternetConfiguration.userAgent='' then
    defaultInternetConfiguration.userAgent:='Mozilla/3.0 (compatible)';

  connection:=THTTPSend.Create;
  connection.Sock.OnStatus:=@connectionStatus;
 // connection.Sock.SSL.SSLType:=LT_SSLv3;

  connection.UserAgent:=defaultInternetConfiguration.userAgent;
  if defaultInternetConfiguration.useProxy then begin
    if defaultInternetConfiguration.proxyHTTPName<>'' then begin
      connection.ProxyHost:=defaultInternetConfiguration.proxyHTTPName;
      connection.ProxyPort:=defaultInternetConfiguration.proxyHTTPPort;
    end;
    //TODO: https proxy
  end;
end;

destructor TSynapseInternetAccess.destroy;
begin
  FreeAndNil(connection);
  additionalHeaders.free;
  inherited destroy;
end;

function TSynapseInternetAccess.needConnection(): boolean;
begin
  result:=existsConnection();
end;

procedure TSynapseInternetAccess.closeOpenedConnections();
begin
  //TODO
end;

function TSynapseInternetAccess.internalHandle: TObject;
begin
 result:=connection;
end;

{$ifndef win32}
initialization
InitCriticalSection(resolvConfCS);
finalization
DoneCriticalsection(resolvConfCS);
{$endif}

{$ENDIF}

end.

