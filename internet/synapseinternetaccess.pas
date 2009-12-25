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
  Classes, SysUtils, internetAccess,
  httpsend,
  ssl_openssl //needed for https
  ;

type

{ TSynapseInternetAccess }

TSynapseInternetAccess=class(TInternetAccess)
protected
  //synapse will automatically handle keep alive
  connection: THTTPSend;
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

  if connection.HTTPMethod(operation,protocol+'://'+host+url) then
    result:=ReadStrFromStream(connection.Document, connection.Document.Size)
   else
    raise EInternetException.Create('Transfer failed: '+inttostr(connection.ResultCode)+': '+connection.ResultString+#13#10'when talking to: '+protocol+'://'+host+url);
  //TODO: test
end;

constructor TSynapseInternetAccess.create();
begin
  internetConfig:=@defaultInternetConfiguration;
  if defaultInternetConfiguration.userAgent='' then
    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible)';

  connection:=THTTPSend.Create;
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

