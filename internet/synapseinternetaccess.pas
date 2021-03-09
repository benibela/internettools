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

TSSLOpenSSLOverride = class(TSSLOpenSSL)
protected
  FOldSSLType: TSSLType;
  FOldVerifyCert: boolean;
  internetAccess: TSynapseInternetAccess;
  function customCertificateHandling: boolean;
  function customQuickClientPrepare: boolean;
  procedure setCustomError(msg: string; id: integer = -3);
public
  function Connect: boolean; override;
  function LibVersion: String; override;
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
  procedure checkHeaders;
  //lastCompleteUrl: string;
  //newConnectionOpened:boolean;
  procedure doTransferUnchecked(method: string; const url: TDecodedUrl; const data:TInternetAccessDataBlock);override;
public
  constructor create();override;
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
  rsSSLErrorOpenSSLTooOld = 'OpenSSL version is too old for certificate checking. Required is OpenSSL 1.0.2+';
  rsSSLErrorCAFileLoadingFailed = 'Failed to load CA files.';
  rsSSLErrorSettingHostname = 'Failed to set hostname for certificate validation.';
  rsSSLErrorConnectionFailed = 'HTTPS connection failed after connecting to server. Some possible causes: handshake failure, mismatched HTTPS version/ciphers, invalid certificate';
  rsSSLErrorVerificationFailed = 'HTTPS certificate validation failed';




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
  internetAccess.writeBlock(buffer, count);
  result := count;
end;

constructor THTTPSendWithFakeStream.Create;
begin
  inherited Create;
  FDocument.Free;
  FDocument := TSynapseSplitStream.Create;
end;

type
  PX509_VERIFY_PARAM = pointer;
  TOpenSSL_version = function(t: integer): pchar; cdecl;
  TSSL_get0_param = function(ctx: PSSL_CTX): PX509_VERIFY_PARAM; cdecl;
  TX509_VERIFY_PARAM_set_hostflags = procedure(param: PX509_VERIFY_PARAM; flags: cardinal); cdecl;
  TX509_VERIFY_PARAM_set1_host = function(param: PX509_VERIFY_PARAM; name: pchar; nameLen: SizeUInt): integer; cdecl;

const X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS = 4;
var _SSL_get0_param: TSSL_get0_param = nil;
var _X509_VERIFY_PARAM_set_hostflags: TX509_VERIFY_PARAM_set_hostflags = nil;
var _X509_VERIFY_PARAM_set1_host: TX509_VERIFY_PARAM_set1_host = nil;
var _OpenSSL_version: TOpenSSL_version = nil;

function TSSLOpenSSLOverride.customCertificateHandling: boolean;
var
  param: PX509_VERIFY_PARAM;
label onError;
begin
  result := false;
  if VerifyCert then begin
    //see https://wiki.openssl.org/index.php/Hostname_validation
    if not assigned(_SSL_get0_param) or not assigned(_X509_VERIFY_PARAM_set_hostflags) or not assigned(_X509_VERIFY_PARAM_set1_host) then begin
      setCustomError(rsSSLErrorOpenSSLTooOld, -2);
      exit;
    end;
    param := _SSL_get0_param(Fssl);
    if param = nil then
      goto onError;
    _X509_VERIFY_PARAM_set_hostflags(param, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
    if _X509_VERIFY_PARAM_set1_host(param, pchar(SNIHost), length(SNIHost)) = 0 then
      goto onError;
  end;
  result := true;
  exit;

onError:
  setCustomError(rsSSLErrorSettingHostname, -2);
  result := false;
end;

function TSSLOpenSSLOverride.customQuickClientPrepare: boolean;
begin
  if not assigned(FSsl) or not assigned(Fctx) or (FOldSSLType <> FSSLType) or (VerifyCert <> FOldVerifyCert)  then begin
    result := Prepare(false);
    if result and VerifyCert and assigned(internetAccess) then
      if SslCtxLoadVerifyLocations(FCtx, internetAccess.internetConfig^.CAFile, internetAccess.internetConfig^.CAPath) <> 1 then begin
        SSLCheck;
        setCustomError(rsSSLErrorCAFileLoadingFailed);
        result := false;
      end;
  end else begin
    sslfree(Fssl);
    Fssl := SslNew(Fctx);
    result := FSsl <> nil;
    if not result then
      SSLCheck;
  end;
  if result then begin
    FOldSSLType := FSSLType;
    FOldVerifyCert := VerifyCert;
  end;
end;

procedure TSSLOpenSSLOverride.setCustomError(msg: string; id: integer);
var
  err: String;
begin
  if internetAccess = nil then
    exit;
  internetAccess.lastHTTPResultCode := id;
  err := msg;
  if LastErrorDesc <> '' then begin
    err := LineEnding + err;
    err += LineEnding+'OpenSSL-Error: '+LastErrorDesc;
    err += LineEnding+'OpenSSL information: CA file: '+internetAccess.internetConfig^.CAFile+' , CA dir: '+internetAccess.internetConfig^.CAPath+' , '+GetSSLVersion+', '+LibVersion;
  end;
  if internetAccess.lastErrorDetails.contains(err) then exit;
  if internetAccess.lastErrorDetails <> '' then internetAccess.lastErrorDetails += LineEnding;
  internetAccess.lastErrorDetails += err;

end;

function TSSLOpenSSLOverride.LibVersion: String;
begin
  Result:=inherited LibVersion;
  if assigned(_OpenSSL_version) then
    result += _OpenSSL_version(0);
end;

//copied from Synapse
function TSSLOpenSSLOverride.Connect: boolean;
type
  TSocket = longint;
const INVALID_SOCKET		= TSocket(NOT(0));
var
  x: integer;
  b: boolean;
  err: integer;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;

  if customQuickClientPrepare() then
  begin

    if not customCertificateHandling then
      exit;

{$IFDEF CIL}
    if sslsetfd(FSsl, FSocket.Socket.Handle.ToInt32) < 1 then
{$ELSE}
    if sslsetfd(FSsl, FSocket.Socket) < 1 then
{$ENDIF}
    begin
      SSLCheck;
      Exit;
    end;
    if SNIHost<>'' then
      SSLCtrl(Fssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, PAnsiChar(AnsiString(SNIHost)));
    //if (internetAccess <> nil) or (FSocket.ConnectionTimeout <= 0) then //do blocking call of SSL_Connect
    begin
      //this is the branch used by internet tools
      x := sslconnect(FSsl);
      if x < 1 then
      begin
        SSLcheck;
        setCustomError(rsSSLErrorConnectionFailed, -3);
        Exit;
      end;
    end;
    //this must be commented out, because ConnectionTimeout is missing in Synapse SVN r40
    {else //do non-blocking call of SSL_Connect
    begin
      b := Fsocket.NonBlockMode;
      Fsocket.NonBlockMode := true;
      repeat
        x := sslconnect(FSsl);
        err := SslGetError(FSsl, x);
        if err = SSL_ERROR_WANT_READ then
          if not FSocket.CanRead(FSocket.ConnectionTimeout) then
            break;
        if err = SSL_ERROR_WANT_WRITE then
          if not FSocket.CanWrite(FSocket.ConnectionTimeout) then
            break;
      until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
      Fsocket.NonBlockMode := b;
      if err <> SSL_ERROR_NONE then
      begin
        SSLcheck;
        Exit;
      end;
    end;}

    if FverifyCert then //seems like this is not needed, since sslconnect already fails on an invalid certificate
      if (GetVerifyCert <> 0) or (not DoVerifyCert) then begin
        setCustomError(rsSSLErrorVerificationFailed, -3);
        Exit;
      end;
    FSSLEnabled := True;
    Result := True;
  end;
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
   //fallback to TLS for servers where auto detection fails
   if striequal(url.protocol, 'https') then
     if lastHTTPsFallbackHost = url.host then connection.Sock.SSL.SSLType := lastHTTPSFallbackType
     else connection.Sock.SSL.SSLType := LT_all;

   enumerateAdditionalHeaders(url, @addHeader, data.count > 0, connection);
   headersSet := false;
  end;

  procedure errorFailedToLoadSSL;
  var
    tempsep: String;
  begin
   tempsep := LineEnding + ' ' + #9 ;
   lastHTTPResultCode := -2;
   lastErrorDetails := Format(rsSSLErrorNoOpenSSL, [tempsep,tempsep,tempsep,tempsep]);
  end;
var ok: Boolean;
  tempSSLType: TSSLType;
begin
  if striequal(url.protocol, 'https') then
    if (not IsSSLloaded) then begin//check if ssl is actually loaded
      errorFailedToLoadSSL;
      exit;
    end;

  lastHTTPResultCode := -4;

  connection.Sock.SSL.VerifyCert := internetConfig^.checkSSLCertificates;
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
    for tempSSLType := SSLFallbackMaxVersion downto SSLFallbackMinVersion do begin
      lastHTTPSFallbackType := tempSSLType;
      initConnection;
      ok := connection.HTTPMethod(method,url.combinedExclude([dupUsername, dupPassword, dupLinkTarget]));
      if ok then break;
    end;
  end;

  if ok then begin
    checkHeaders;
  end else if lastHTTPResultCode = -4 then
    lastErrorDetails := rsConnectionFailed;
end;

constructor TSynapseInternetAccess.create();
var
  temp: String;
begin
  init;

  connection:=THTTPSendWithFakeStream.Create;
  (connection.Document as TSynapseSplitStream).internetAccess := self;
  (connection.Sock.SSL as TSSLOpenSSLOverride).internetAccess := self;

  connection.UserAgent:=defaultInternetConfiguration.userAgent;
  if defaultInternetConfiguration.useProxy then begin
    if defaultInternetConfiguration.proxyHTTPName<>'' then begin
      connection.ProxyHost:=defaultInternetConfiguration.proxyHTTPName;
      connection.ProxyPort:=defaultInternetConfiguration.proxyHTTPPort;
    end;
    connection.ProxyUser:=defaultInternetConfiguration.proxyUsername;
    connection.ProxyPass:=defaultInternetConfiguration.proxyPassword;
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

function TSynapseInternetAccess.internalHandle: TObject;
begin
 result:=connection;
end;

initialization

assert(TSynapseInternetAccess.SSLFallbackMaxVersion >= LT_TLSv1_1);

if (SSLLibHandle <> 0) and (SSLUtilHandle <> 0) then begin
  _SSL_get0_param := TSSL_get0_param(GetProcedureAddress(SSLLibHandle, 'SSL_get0_param'));
  _X509_VERIFY_PARAM_set_hostflags := TX509_VERIFY_PARAM_set_hostflags(GetProcedureAddress(SSLUtilHandle, 'X509_VERIFY_PARAM_set_hostflags'));
  _X509_VERIFY_PARAM_set1_host := TX509_VERIFY_PARAM_set1_host(GetProcedureAddress(SSLUtilHandle, 'X509_VERIFY_PARAM_set1_host'));
  _OpenSSL_version := TOpenSSL_version(GetProcedureAddress(SSLLibHandle, 'OpenSSL_version'));
end;

{$IFDEF USE_SYNAPSE_WRAPPER}
defaultInternetAccessClass := TSynapseInternetAccess;
SSLImplementation := TSSLOpenSSLOverride;
defaultInternetConfiguration.searchCertificates;
{$ENDIF}


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

