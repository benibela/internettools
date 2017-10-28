{Copyright (C) 2009-2017  Benito van der Zander

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



{$IFNDEF WINDOWS} //If this unit should be compiled. Not enabled on windows, since you can use w32internetaccess there
 {$IFNDEF USE_WININET_WRAPPER}{$IFNDEF USE_ANDROID_WRAPPER}{$IFNDEF USE_NO_WRAPPER}
  {$DEFINE USE_SYNAPSE_WRAPPER}
 {$ENDIF}{$ENDIF}{$ENDIF}
{$ENDIF}
{$IFDEF USE_SYNAPSE_WRAPPER}
 {$DEFINE COMPILE_SYNAPSE_INTERNETACCESS}
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
TSynapseInternetAccess=class;
TSynapseSplitStream = class(TMemoryStream)
  internetAccess: TSynapseInternetAccess;
  function Write(const Buffer; Count: LongInt): LongInt; override;
end;

THTTPSendWithFakeStream = class(THTTPSend)
  constructor Create;
end;

{ TSynapseInternetAccess }
//**@abstract(Internet access class using the Synapse library)
//**Set defaultInternetAccessClass to TSynapseInternetAccess to use it.@br
//**You also have to install the Synapse package.@br
//**In contrast to native Synapse this will automatically load openssl, if it is called on HTTPS URLs.
TSynapseInternetAccess=class(TInternetAccess)
protected
  //synapse will automatically handle keep alive
  connection: THTTPSendWithFakeStream;
  lastHTTPSFallbackHost: string;
  headersSet: boolean;
  procedure checkHeaders;
  //lastCompleteUrl: string;
  //newConnectionOpened:boolean;
  procedure doTransferUnchecked(method: string; const url: TDecodedUrl; const data:TInternetAccessDataBlock);override;
public
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

uses synautil,ssl_openssl_lib,bbutils{$ifndef WINDOWS},netdb{$endif};

{ TSynapseInternetAccess }

{$if defined(WINDOWS) or defined(android)}
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

function TSynapseSplitStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  internetAccess.checkHeaders;
  internetAccess.writeBlock(buffer, count);
  result := count;
end;

constructor THTTPSendWithFakeStream.Create;
begin
  inherited Create;
  FDocument.Free;
  FDocument := TSynapseSplitStream.Create;
end;

procedure addHeader(data: pointer; headerKind: TSynapseInternetAccess.THeaderKind; const name, header: string);
var
  connection: THTTPSend;
begin
  connection := THTTPSend(data);
  case headerKind of
    iahContentType: connection.MimeType := header;
    else connection.Headers.Add(TSynapseInternetAccess.makeHeaderLine(name, header));
  end;
end;

procedure TSynapseInternetAccess.checkHeaders;
begin
  if headersSet then exit;
  LastHTTPHeaders.assign(connection.Headers);
  lastHTTPResultCode := connection.ResultCode;
  headersSet := true;
end;

procedure TSynapseInternetAccess.doTransferUnchecked(method: string; const url: TDecodedUrl; const data: TInternetAccessDataBlock);
  procedure initConnection;
  begin
   connection.Clear;
   connection.Cookies.Clear;
   //Some servers fail without port in host, some with. This behaviour mirrors Firefox:
   connection.AddPortNumberToHost:=(url.port <> '')
                                    and ( (striEqual(url.protocol, 'http') and (url.port <> '80'))
                                          or (striEqual(url.protocol, 'https') and (url.port <> '443'))
                                         );
   if data.count > 0 then begin
     connection.Document.Size := data.count;
     move(data.data^, connection.Document.Memory^, data.count);
     connection.MimeType := ContentTypeForData; //this pointless as addHeader overrides it. But it does not hurt either
   end;
   connection.Protocol:='1.1';
   //fallback to TLS 1 for servers where auto detection fails
   if striequal(url.protocol, 'https') then
     if lastHTTPsFallbackHost = url.host then connection.Sock.SSL.SSLType := LT_TLSv1
     else connection.Sock.SSL.SSLType := LT_all;

   enumerateAdditionalHeaders(url, @addHeader, data.count > 0, connection);
   headersSet := false;
  end;

var ok: Boolean;
begin
  if striequal(url.protocol, 'https') then
    if (not IsSSLloaded) then begin//check if ssl is actually loaded
      lastHTTPResultCode := -2;
      lastErrorDetails := 'Couldn''t load ssl libraries: libopenssl and libcrypto' + LineEnding +
                          'They must be installed separately.' + LineEnding +
                          '  On Debian/Ubuntu install libssl-dev.' + LineEnding +
                          '  On Fedora/CentOS install openssl-devel.' + LineEnding +
                          '  On Windows install OpenSSL from https://slproweb.com/products/Win32OpenSSL.html';
      exit;
    end;


  initConnection;
  if (url.username <> '') then begin
    connection.UserName := strUnescapeHex(url.username, '%');
    connection.Password := strUnescapeHex(url.password, '%');
  end;

  ok := connection.HTTPMethod(method,url.combinedExclude([dupUsername, dupPassword, dupLinkTarget]));

  if (not ok) and (checkEtcResolv) then begin
    initConnection;
    ok := connection.HTTPMethod(method,url.combinedExclude([dupUsername, dupPassword, dupLinkTarget]));
  end;

  if (not ok) and (lastHTTPSFallbackHost <> url.host) then begin
    lastHTTPSFallbackHost := url.host;
    initConnection;
    ok := connection.HTTPMethod(method,url.combinedExclude([dupUsername, dupPassword, dupLinkTarget]));
  end;

  if ok then begin
    checkHeaders;
  end else begin
    lastHTTPResultCode := -4;
  end;
end;

constructor TSynapseInternetAccess.create();
var
  temp: String;
begin
  init;

  connection:=THTTPSendWithFakeStream.Create;
  (connection.Document as TSynapseSplitStream).internetAccess := self;

  connection.UserAgent:=defaultInternetConfiguration.userAgent;
  if defaultInternetConfiguration.useProxy then begin
    if defaultInternetConfiguration.proxyHTTPName<>'' then begin
      connection.ProxyHost:=defaultInternetConfiguration.proxyHTTPName;
      connection.ProxyPort:=defaultInternetConfiguration.proxyHTTPPort;
    end;
    if defaultInternetConfiguration.proxySOCKSName <>'' then begin
      temp := defaultInternetConfiguration.proxySOCKSName;
      if strContains(temp, '@') then begin
        connection.Sock.SocksUsername:=strSplitGet('@', temp);
        if strContains(connection.Sock.SocksUsername, ':') then begin
          connection.Sock.SocksPassword:=strSplit(connection.Sock.SocksUsername, ':')[1];
          connection.Sock.SocksUsername:=strSplit(connection.Sock.SocksUsername, ':')[0];
        end;
      end;
      connection.Sock.SocksIP:=temp;
      connection.Sock.SocksPort:=defaultInternetConfiguration.proxySOCKSPort;
    end;
    //TODO: https proxy
  end;
end;

destructor TSynapseInternetAccess.destroy;
begin
  FreeAndNil(connection);
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

initialization

{$IFDEF USE_SYNAPSE_WRAPPER}
defaultInternetAccessClass := TSynapseInternetAccess;
{$ENDIF}


{$if not (defined(WINDOWS) or defined(android))}
InitCriticalSection(resolvConfCS);
{$endif}
finalization

freeThreadVars; //otherwise ssl_openssl_lib.finalization calls SSLCS.Free; causing the TSynapseInternetAccess.free to crash, if another unit calls freeThreadVars later;

{$if not (defined(WINDOWS) or defined(android))}
DoneCriticalsection(resolvConfCS);
{$endif}

{$ENDIF}

end.

