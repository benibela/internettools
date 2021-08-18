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





{$IFNDEF WINDOWS} //If this unit should be compiled. Not enabled on windows, since you can use w32internetaccess there
 {$IFNDEF USE_WININET_WRAPPER}{$IFNDEF USE_ANDROID_WRAPPER}{$IFNDEF USE_NO_WRAPPER}
  {$DEFINE USE_SYNAPSE_WRAPPER}
 {$ENDIF}{$ENDIF}{$ENDIF}
{$ENDIF}
{$IFDEF USE_SYNAPSE_WRAPPER}
 {$DEFINE COMPILE_SYNAPSE_INTERNETACCESS}
{$ENDIF}

{$I ../internettoolsconfig.inc}


interface

{$IFDEF COMPILE_SYNAPSE_INTERNETACCESS}

uses
  Classes, SysUtils, internetAccess,
  httpsend,  //this is the synapse http unit (from http://www.ararat.cz/synapse/doku.php/download)
  blcksock,
  synapse_ssl_openssl_override //needed for https
  ;

type
TSynapseInternetAccess=class;
PTransfer = ^TTransfer;
TSynapseSplitStream = class(TMemoryStream)
  internetAccess: TSynapseInternetAccess;
  function Write(const Buffer; Count: LongInt): LongInt; override;
end;

THTTPSendWithFakeStream = class(THTTPSend)
  constructor Create;
  procedure closeConnection;
end;

{ TSynapseInternetAccess }
//**@abstract(Internet access class using the Synapse library)
//**Set defaultInternetAccessClass to TSynapseInternetAccess to use it.@br
//**You also have to install the Synapse package.@br
//**In contrast to native Synapse this will automatically load openssl, if it is called on HTTPS URLs.
TSynapseInternetAccess=class(TInternetAccess)
const
  SSLFallbackMaxVersion = TSSLType(ord(LT_SSHv2) - 1);
  SSLFallbackMinVersion = LT_TLSv1;

protected
  //synapse will automatically handle keep alive
  connection: THTTPSendWithFakeStream;
  lastHTTPSFallbackHost: string;
  lastHTTPSFallbackType: TSSLType;
  headersSet: boolean;
  currentTransfer: PTransfer;
  procedure checkHeaders;
  //lastCompleteUrl: string;
  //newConnectionOpened:boolean;
  procedure doTransferUnchecked(var transfer: TTransfer);override;
  procedure setConfig(internetConfig: PInternetConfig); override;
public
  constructor create();override;
  constructor create(const internetConfig: TInternetConfig);override;
  destructor destroy;override;

  function internalHandle: TObject; override;
end;
TSynapseInternetAccessClass = class of TSynapseInternetAccess;

{$ENDIF}

implementation

{$IFDEF COMPILE_SYNAPSE_INTERNETACCESS}

uses synautil,ssl_openssl_lib,bbutils
     {$ifndef WINDOWS},netdb{$endif}
     {$if FPC_FULLVERSION < 30101},dynlibs{$endif}
     {$if not (defined(WINDOWS) or defined(android))},BaseUnix{$endif}
     ;

resourcestring rsConnectionFailed = 'Connection failed. Some possible causes: Failed DNS lookup, failed to load OpenSSL, failed proxy, server does not exists, has no open port or uses an unknown https certificate.';
  rsSSLErrorNoOpenSSL = 'Couldn''t load ssl libraries: libopenssl and libcrypto%sThey must be installed separately.%s'+
                        'On Debian/Ubuntu install libssl-dev.%s' +
                        'On Fedora/CentOS install openssl-devel.%s' +
                        'On Windows install OpenSSL from https://slproweb.com/products/Win32OpenSSL.html';




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

procedure disableSIGPIPECrash;
var sa, osa: sigactionrec;
begin
  sa := default(sigactionrec);
  osa := default(sigactionrec);
  FPSigaction(SIGPIPE, nil, @osa);
  if osa.sa_handler <> sigactionhandler(SIG_DFL) then
    exit;
  sa.sa_handler := sigactionhandler(SIG_IGN);
  FPSigaction(SIGPIPE, @sa, nil);
end;

{$endif}

function TSynapseSplitStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  internetAccess.checkHeaders;
  internetAccess.currentTransfer.writeBlock(buffer, count);
  result := count;
end;

constructor THTTPSendWithFakeStream.Create;
begin
  inherited Create;
  FDocument.Free;
  FDocument := TSynapseSplitStream.Create;
end;

procedure THTTPSendWithFakeStream.closeConnection;
begin
  FAliveHost := '';
  FAlivePort := '';
end;



procedure addHeader(data: pointer; headerKind: THeaderKind; const name, header: string);
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
  currentTransfer.receivedHTTPHeaders.assign(connection.Headers);
  currentTransfer.HTTPResultCode := connection.ResultCode;
  headersSet := true;
end;

procedure TSynapseInternetAccess.doTransferUnchecked(var transfer: TTransfer);
  procedure initConnection;
  begin
   connection.Clear;
   connection.Cookies.Clear;
   with transfer do begin
     //Some servers fail without port in host, some with. This behaviour mirrors Firefox:
     connection.AddPortNumberToHost:=(decodedUrl.port <> '')
                                      and ( (striEqual(decodedUrl.protocol, 'http') and (decodedUrl.port <> '80'))
                                            or (striEqual(decodedUrl.protocol, 'https') and (decodedUrl.port <> '443'))
                                           );
     if not data.isEmpty then begin
       connection.Document.Size := data.length;
       move(data.data^, connection.Document.Memory^, data.length);
       connection.MimeType := ContentTypeForData; //this pointless as addHeader overrides it. But it does not hurt either
     end;
     connection.Protocol:='1.1';
     //fallback to TLS for servers where auto detection fails
     if striequal(decodedUrl.protocol, 'https') then
       if lastHTTPsFallbackHost = decodedUrl.host then connection.Sock.SSL.SSLType := lastHTTPSFallbackType
       else connection.Sock.SSL.SSLType := LT_all;

   end;
   enumerateAdditionalHeaders(transfer, @addHeader, connection);
   headersSet := false;
  end;

  procedure errorFailedToLoadSSL;
  var
    tempsep: String;
  begin
   tempsep := LineEnding + ' ' + #9 ;
   transfer.HTTPResultCode := -2;
   transfer.HTTPErrorDetails := Format(rsSSLErrorNoOpenSSL, [tempsep,tempsep,tempsep,tempsep]);
  end;

var openSSLError: string = '';

  function InitAndHTTPMethod: boolean;
  begin
    initConnection;
    result := connection.HTTPMethod(transfer.method, transfer.url);
    if (not result) and (connection.Sock.SSL is TSSLOpenSSLOverride) then begin
      with connection.Sock.SSL as TSSLOpenSSLOverride do begin
        if openSSLError.contains(outErrorMessage) then exit;
        if openSSLError <> '' then openSSLError += LineEnding;
        openSSLError += outErrorMessage;
      end;
    end;
  end;

var ok: Boolean;
  tempSSLType: TSSLType;
begin
  if connection.Sock.SSL is TSSLOpenSSLOverride then begin
    if  not IsSSLloaded then TSSLOpenSSLOverride.LoadOpenSSL;
    with (connection.Sock.SSL as TSSLOpenSSLOverride) do begin
      outErrorMessage := '';
      outErrorCode := -5;
    end;
  end;
  if striequal(transfer.decodedUrl.protocol, 'https') then begin
    if (SSLImplementation = TSSLNone)
       or ((SSLImplementation = TSSLOpenSSLOverride) and not IsSSLloaded)
    then begin//check if ssl is actually loaded
      errorFailedToLoadSSL;
      exit;
    end;
  end;
  currentTransfer := @transfer;

  transfer.HTTPErrorDetails := '';
  transfer.HTTPResultCode := -4;

  connection.Sock.SSL.VerifyCert := config.checkSSLCertificates;
  if (transfer.decodedUrl.username <> '') then begin
    connection.UserName := strUnescapeHex(transfer.decodedUrl.username, '%');
    connection.Password := strUnescapeHex(transfer.decodedUrl.password, '%');
  end;

  ok := InitAndHTTPMethod;

  if (not ok) and (checkEtcResolv) then
    ok := InitAndHTTPMethod;


  if (not ok) and (lastHTTPSFallbackHost <> transfer.decodedUrl.host) then begin
    lastHTTPSFallbackHost := transfer.decodedUrl.host;
    for tempSSLType := SSLFallbackMaxVersion downto SSLFallbackMinVersion do begin
      lastHTTPSFallbackType := tempSSLType;
      ok := InitAndHTTPMethod;
      if ok then break;
    end;
  end;

  if ok then begin
    checkHeaders;
  end else if transfer.HTTPResultCode = -4 then
    transfer.HTTPErrorDetails := rsConnectionFailed + openSSLError;
end;

procedure TSynapseInternetAccess.setConfig(internetConfig: PInternetConfig);
var
  temp: String;
begin
  if not fconfig.equalsUserAgent(internetConfig^) or not fconfig.equalsProxy(internetConfig^) then begin
    connection.closeConnection;
    connection.UserAgent:=internetConfig.userAgent;
    connection.ProxyHost:='';
    connection.ProxyPort:='';
    connection.ProxyUser:='';
    connection.ProxyPass:='';
    connection.Sock.SocksIP:='';
    connection.Sock.SocksPort:='';
    connection.Sock.SocksUsername := '';
    connection.Sock.SocksPassword := '';
    if internetConfig.useProxy then begin
      if internetConfig.proxyHTTPName<>'' then begin
        connection.ProxyHost:=internetConfig.proxyHTTPName;
        connection.ProxyPort:=internetConfig.proxyHTTPPort;
      end;
      connection.ProxyUser:=internetConfig.proxyUsername;
      connection.ProxyPass:=internetConfig.proxyPassword;

      if internetConfig.proxySOCKSName <>'' then begin
        temp := internetConfig.proxySOCKSName;
        if strContains(temp, '@') then begin
          connection.Sock.SocksUsername:=strSplitGet('@', temp);
          if strContains(connection.Sock.SocksUsername, ':') then begin
            connection.Sock.SocksPassword:=strSplit(connection.Sock.SocksUsername, ':')[1];
            connection.Sock.SocksUsername:=strSplit(connection.Sock.SocksUsername, ':')[0];
          end;
        end;
        connection.Sock.SocksIP:=temp;
        connection.Sock.SocksPort:=internetConfig.proxySOCKSPort;
      end
     //TODO: https proxy
    end;
  end;
  if connection.Sock.SSL is TSSLOpenSSLOverride then begin
    (connection.Sock.SSL as TSSLOpenSSLOverride).CAFile := internetConfig.CAFile;
    (connection.Sock.SSL as TSSLOpenSSLOverride).CAPath := internetConfig.CAPath;
    (connection.Sock.SSL).CertCAFile := internetConfig.CAFile;
  end;
  inherited setConfig(internetConfig);
end;

constructor TSynapseInternetAccess.create();
begin
  inherited create();
end;

constructor TSynapseInternetAccess.create(const internetConfig: TInternetConfig);
begin
  connection:=THTTPSendWithFakeStream.Create;
  (connection.Document as TSynapseSplitStream).internetAccess := self;
  inherited;
end;

destructor TSynapseInternetAccess.destroy;
begin
  FreeAndNil(connection);
  inherited destroy;
end;

function TSynapseInternetAccess.internalHandle: TObject;
begin
 result:=connection;
end;

initialization

assert(TSynapseInternetAccess.SSLFallbackMaxVersion >= LT_TLSv1_1);


{$IFDEF USE_SYNAPSE_WRAPPER}
defaultInternetAccessClass := TSynapseInternetAccess;
{$ENDIF}

//{$IFDEF COMPILE_SYNAPSE_INTERNETACCESS}
defaultInternetConfiguration.searchCertificates;
//{$endif}

{$if not (defined(WINDOWS) or defined(android))}
InitCriticalSection(resolvConfCS{%H-});

disableSIGPIPECrash;

{$endif}
finalization

freeThreadVars; //otherwise ssl_openssl_lib.finalization calls SSLCS.Free; causing the TSynapseInternetAccess.free to crash, if another unit calls freeThreadVars later;

{$if not (defined(WINDOWS) or defined(android))}
DoneCriticalsection(resolvConfCS);
{$endif}

{$ENDIF}

end.

