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
unit synapseinternetaccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, internetAccess,Dialogs,
  httpsend,  //this is the synapse http unit
  blcksock,
  ssl_openssl //needed for https
  ;

type

{ TSynapseInternetAccess }

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
  function transfer(protocol,host,url: string;data:string;progressEvent:TProgressEvent): string;
public
  constructor create();override;
  destructor destroy;override;
  function post(protocol,host,url: string;data:string):string;override;
  function get(protocol,host,url: string;progressEvent:TProgressEvent=nil):string;override;overload;
  function needConnection():boolean;override;
  procedure closeOpenedConnections();override;
end;
TSynapseInternetAccessClass = class of TSynapseInternetAccess;

implementation

uses synautil,ssl_openssl_lib;

{ TSynapseInternetAccess }

procedure TSynapseInternetAccess.connectionStatus(Sender: TObject;
  Reason: THookSocketReason; const Value: String);
var
  i: Integer;
begin
  if (forwardProgressEvent=nil) or (connection=nil) then exit;
  if contentLength=-1 then begin
    for i:=0 to connection.Headers.Count-1 do
      if pos('content-length',lowercase(connection.Headers[i]))>0 then begin
        contentLength:=StrToIntDef(copy(connection.Headers[i],pos(':',connection.Headers[i])+1,length(connection.Headers[i])),-1);
        exit;
      end;
    if contentLength=-1 then exit;
  end;
  lastProgressLength:=connection.DownloadSize;
  forwardProgressEvent(self, connection.DownloadSize, contentLength);
end;


function TSynapseInternetAccess.transfer(protocol, host, url: string;
  data: string; progressEvent: TProgressEvent): string;
var operation: string;
begin
  result:='';
  connection.Clear;
  if data = '' then operation:='GET'
  else begin
    operation:='POST';
    WriteStrToStream(connection.Document, data);
    connection.MimeType := 'application/x-www-form-urlencoded';
  end;
  if (UpperCase(protocol)='HTTPS') then
    if (not IsSSLloaded) then //check if ssl is actually loaded
       raise EInternetException.Create('Couldn''t load ssl libraries: libopenssl and libcrypto'#13#10'(Hint: install also the dev packages on Debian)');

  contentLength:=-1;
  lastProgressLength:=-1;
  forwardProgressEvent:=progressEvent;

  if connection.HTTPMethod(operation,protocol+'://'+host+url) and
     ((connection.ResultCode = 200) or (connection.ResultCode = 302))then begin
     result:=ReadStrFromStream(connection.Document, connection.Document.Size)
   end else
    raise EInternetException.Create('Transfer failed: '+inttostr(connection.ResultCode)+': '+connection.ResultString+#13#10'when talking to: '+protocol+'://'+host+url);

  if (progressEvent<>nil) and (lastProgressLength<connection.DownloadSize) then
    if contentLength=-1 then progressEvent(self,connection.DownloadSize,connection.DownloadSize)
    else progressEvent(self,connection.DownloadSize,contentLength);
end;

constructor TSynapseInternetAccess.create();
begin
  internetConfig:=@defaultInternetConfiguration;
  if defaultInternetConfiguration.userAgent='' then
    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible)';

  connection:=THTTPSend.Create;
  connection.Sock.OnStatus:=@connectionStatus;
 //e connection.Sock.SSL.SSLType:=LT_SSLv3;
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
  inherited destroy;
end;

function TSynapseInternetAccess.post(protocol, host, url: string; data: string
  ): string;
begin
  result:=transfer(protocol,host,url,data,nil);
end;

function TSynapseInternetAccess.get(protocol, host, url: string;
  progressEvent: TProgressEvent): string;
begin
  result:=transfer(protocol,host,url,'',progressEvent);
end;

function TSynapseInternetAccess.needConnection(): boolean;
begin
  result:=existsConnection();
end;

procedure TSynapseInternetAccess.closeOpenedConnections();
begin
  //TODO
end;

end.

