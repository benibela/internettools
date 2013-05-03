{Copyright (C) 2013  Benito van der Zander

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
unit androidinternetaccess;


{$mode objfpc}{$H+}

{$IFDEF ANDROID}
{$DEFINE COMPILE_ANDROID_INTERNETACCESS} //If this unit should be compiled.
{$ENDIF}



interface

{$IFDEF COMPILE_ANDROID_INTERNETACCESS}

uses
  Classes, SysUtils, internetAccess,
  jni, LCLProc
  ;

type

{ TSynapseInternetAccess }
//**@abstract(Internet access class using the synapse library)
//**Set defaultInternetAccessClass to TSynapseInternetAccess if
//**you want to use wininet to connect to internet@br
//**You also have to install the synapse package@br
//**In contrast to native synapse this will automatically load openssl
//**if it is called for the https protocal

{ TAndroidInternetAccess }

TAndroidInternetAccess=class(TInternetAccess)
protected
  //synapse will automatically handle keep alive
  lastConnectedUrl: TDecodedUrl;
  jhttpclient: jobject;
  FLastHTTPHeaders: TStringList;
  function doTransfer2(method:string; url: TDecodedUrl; data:string): string;
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
TAndroidInternetAccessClass = class of TAndroidInternetAccess;

{$ENDIF}

implementation

{$IFDEF COMPILE_ANDROID_INTERNETACCESS}

uses bbutils, CustomDrawnInt, bbjniutils;

type THttpMethod = (hmDelete, hmGet, hmHead, hmOptions, hmPost, hmPut, hmTrace);
const methodCamelNames: array[THttpMethod] of string = ('Delete', 'Get', 'Head', 'Options', 'Post', 'Put', 'Trace');
threadvar javaEnvRef: PJNIEnv;
type TClassInformation = record
    jcDefaultHttpClient: jclass;
    jmDefaultHttpClientConstructor: jmethodID;
    jcMethods: array[THttpMethod] of jclass;
    jmMethodConstructors: array[THttpMethod] of jmethodID;
    jcByteArrayEntity: jclass;
    jmDefaultHttpClientExecute: jmethodID;
    jmHttpMessageAddHeader: jmethodID;
    jmHttpMessageHeaderIterator: jmethodID;
    jmHeaderIteratorHasNext: jmethodID;
    jmHeaderIteratorNextHeader: jmethodID;
    jmHeaderGetName: jmethodID;
    jmHeaderGetValue: jmethodID;
    jmByteArrayEntityConstructor: jmethodID;
    jcAbstractHttpEntity: jclass;
    jmAbstractHttpEntitySetContentType: jmethodID;
    jcHttpEntityEnclosingRequestBase: jclass;
    jmHttpEntityEnclosingRequestBaseSetEntity: jmethodID;
    jiHttpResponse: jclass;
    jmHttpResponseGetEntity: jmethodID;
    jmHttpResponseGetStatusLine: jmethodID;
    jiHttpEntity: jclass;
    jmHttpEntityGetContent: jmethodID;
    jmStatusLineGetReasonPhrase: jmethodID;
    jiStatusLine: jclass;
    jmStatusLineGetStatusCode: jmethodID;
    jmDefaultHttpClientGetParams: jmethodID;
    jmHttpParamsSetParameter: jmethodID;
    jcHttpHost: jclass;
    jmHttpHostConstructor: jmethodID;
end;

//threadvar initialized: boolean;
//          cache: TClassInformation;

//finds the necessary JNI classes
//was supposed to cache the classes and call findclass only once, but the jclass is garbage collected,
//and saving them in a global reference causes more trouble than the refinding them everytime
function initializeClasses: TClassInformation;

  function getc(n: pchar): jclass; inline;
  begin
    result := j.getclass(n);
  end;
  function getm(c: jclass; n, sig: pchar): jmethodID; inline;
  begin
    result := j.getmethod(c, n, sig);
  end;

var
  jiHttpMessage: jclass;
  jiHeaderIterator: jclass;
  jiHeader: jclass;
  m: THttpMethod;
  jiHttpParams: jclass;

begin
  //if initialized then exit(cache);
  with result do begin    //TODO: merge with androidutils of VideLibri

     jcDefaultHttpClient := getc('org/apache/http/impl/client/DefaultHttpClient');
     jmDefaultHttpClientConstructor := getm(jcDefaultHttpClient, '<init>', '()V');
     jmDefaultHttpClientExecute := getm(jcDefaultHttpClient, 'execute', '(Lorg/apache/http/client/methods/HttpUriRequest;)Lorg/apache/http/HttpResponse;');
     jmDefaultHttpClientGetParams := getm(jcDefaultHttpClient, 'getParams', '()Lorg/apache/http/params/HttpParams;');

     jiHttpParams := getc('org/apache/http/params/HttpParams');
     jmHttpParamsSetParameter := getm(jiHttpParams, 'setParameter', '(Ljava/lang/String;Ljava/lang/Object;)Lorg/apache/http/params/HttpParams;');

     jcHttpHost := getc('org/apache/http/HttpHost');
     jmHttpHostConstructor := getm(jcHttpHost, '<init>', '(Ljava/lang/String;I)V');

     jiHttpMessage := getc('org/apache/http/HttpMessage');
     jmHttpMessageAddHeader := getm(jiHttpMessage, 'addHeader', '(Ljava/lang/String;Ljava/lang/String;)V');
     jmHttpMessageHeaderIterator := getm(jiHttpMessage, 'headerIterator', '()Lorg/apache/http/HeaderIterator;');

     jiHeaderIterator := getc('org/apache/http/HeaderIterator');
     jmHeaderIteratorHasNext := getm(jiHeaderIterator, 'hasNext', '()Z');
     jmHeaderIteratorNextHeader := getm(jiHeaderIterator, 'nextHeader', '()Lorg/apache/http/Header;');

     jiHeader := getc('org/apache/http/Header');
     jmHeaderGetName := getm(jiHeader, 'getName', '()Ljava/lang/String;');
     jmHeaderGetValue := getm(jiHeader, 'getValue', '()Ljava/lang/String;');

     jcByteArrayEntity := getc('org/apache/http/entity/ByteArrayEntity');
     jmByteArrayEntityConstructor := getm(jcByteArrayEntity, '<init>', '([B)V');

     jcAbstractHttpEntity := getc('org/apache/http/entity/AbstractHttpEntity');
     jmAbstractHttpEntitySetContentType := getm(jcAbstractHttpEntity, 'setContentType', '(Ljava/lang/String;)V');

     jcHttpEntityEnclosingRequestBase := getc('org/apache/http/client/methods/HttpEntityEnclosingRequestBase');
     jmHttpEntityEnclosingRequestBaseSetEntity := getm(jcHttpEntityEnclosingRequestBase, 'setEntity', '(Lorg/apache/http/HttpEntity;)V');

     jiHttpResponse := getc('org/apache/http/HttpResponse');
     jmHttpResponseGetEntity := getm(jiHttpResponse, 'getEntity', '()Lorg/apache/http/HttpEntity;');
     jmHttpResponseGetStatusLine := getm(jiHttpResponse, 'getStatusLine', '()Lorg/apache/http/StatusLine;');

     jiHttpEntity := getc('org/apache/http/HttpEntity');
     jmHttpEntityGetContent := getm(jiHttpEntity, 'getContent', '()Ljava/io/InputStream;');

     jiStatusLine := getc('org/apache/http/StatusLine');
     jmStatusLineGetReasonPhrase := getm(jiStatusLine, 'getReasonPhrase', '()Ljava/lang/String;');
     jmStatusLineGetStatusCode := getm(jiStatusLine, 'getStatusCode', '()I');

     for m := low(THttpMethod) to high(THttpMethod) do begin
       jcMethods[m] := getc(pchar('org/apache/http/client/methods/Http'+methodCamelNames[m]));
       jmMethodConstructors[m] := getm(jcMethods[m], '<init>', '(Ljava/lang/String;)V');
     end;

   end;
 // initialized:=true;
  //cache:=result;
end;

function methodStringToMethod(s: string): THttpMethod;
begin
  case UpperCase(s) of
    'DELETE': result := hmDelete;
    'GET': result := hmGet;
    'HEAD': result := hmHead;
    'OPTIONS': result := hmOptions;
    'POST': result := hmPost;
    'PUT': result := hmPut;
    'TRACE': result := hmTrace;
    else raise EInternetException.create('Unknown http method: '+s);
  end;
end;


function TAndroidInternetAccess.doTransfer2(method: string; url: TDecodedUrl; data: string): string;
var jRequest: jvalue;
    classInfos: TClassInformation;

  procedure addHeader(const n,v: string);
  var args: array[0..1] of jvalue;
  begin
    //jRequest.addHeader(n, v)
    with classInfos do begin
      args[0].l := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar(n));
      args[1].l := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar(v));
      javaEnvRef^^.CallVoidMethodA(javaEnvRef, jRequest.l,  jmHttpMessageAddHeader, @args[0]);;
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, args[0].l);
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, args[1].l);
    end;
  end;

  procedure setRequestData;
  var jentity: jvalue;
      wrappedData: jbyteArray;
      temp: jvalue;
  begin
    with classInfos do begin
      if data = '' then exit;
      wrappedData := javaEnvRef^^.NewByteArray(javaEnvRef, length(data));
      if wrappedData = nil then raise EInternetException.create('Failed to allocate JNI array');
      javaEnvRef^^.SetByteArrayRegion(javaEnvRef, wrappedData, 0, length(data), @data[1]); //todo: faster way than copying?
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, wrappedData);


      //entity = new ByteArrayEntity(data)
      jentity.l := javaEnvRef^^.NewObjectA(javaEnvRef, jcByteArrayEntity, jmByteArrayEntityConstructor, @wrappedData);
      //entity.setContentType(..)
      if additionalHeaders.IndexOfName('Content-Type') < 0 then begin
        temp.l := javaEnvRef^^.NewStringUTF(javaEnvRef, 'application/x-www-form-urlencoded');
        javaEnvRef^^.CallVoidMethodA(javaEnvRef, jentity.l,  jmAbstractHttpEntitySetContentType, @temp);;
        javaEnvRef^^.DeleteLocalRef(javaEnvRef, temp.l);
      end;
      //httprequest.setEntity(entity)
      javaEnvRef^^.CallVoidMethodA(javaEnvRef, jRequest.l,  jmHttpEntityEnclosingRequestBaseSetEntity, @jentity);;

      javaEnvRef^^.DeleteLocalRef(javaEnvRef, wrappedData);
    end;
  end;

var
  i: Integer;

  jUrl: jvalue;
  m: THttpMethod;
  jResponse, jResult, jStatusLine, jHeaderIterator, jHeader: jobject;
  resultCode: integer;
  resultString: string;
begin
  result:='';
  if javaEnvRef^^.ExceptionCheck(javaEnvRef) <> JNI_FALSE then begin
    javaEnvRef^^.ExceptionDescribe(javaEnvRef);
    javaEnvRef^^.ExceptionClear(javaEnvRef);
    DebugLn('Warning: Ignoring exception');
  end;

  url.prepareSelfForRequest(lastConnectedUrl);

  m := methodStringToMethod(method);

  classInfos := initializeClasses; //todo: cache?

  with classInfos do begin;
    //HttpGet httpget = new HttpGet(url);
    jUrl.l := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar(url.combined));
    jRequest.l := javaEnvRef^^.NewObjectA(javaEnvRef, jcMethods[m], jmMethodConstructors[m], @jUrl);
    javaEnvRef^^.DeleteLocalRef(javaEnvRef, jUrl.l);

    if Referer <> '' then
      addHeader('Referer', Referer);
    for i := 0 to additionalHeaders.Count - 1 do
      addHeader(additionalHeaders.Names[i], additionalHeaders.ValueFromIndex[i]);
    if (data <> '') and (m in [hmPut, hmPost]) then
      setRequestData();

    //send
    jResponse := javaEnvRef^^.CallObjectMethodA(javaEnvRef, jhttpclient,  jmDefaultHttpClientExecute, @jRequest);;

    j.RethrowJavaExceptionIfThereIsOne(EInternetException); //if there is an exception during execute, do NOT delete the jRespone (having it in a finally block caused sigsegv, because it is an invalid not-nil value)

    try
      //process
      jResult := javaEnvRef^^.CallObjectMethod(javaEnvRef, jResponse,  jmHttpResponseGetEntity);

      jStatusLine := javaEnvRef^^.CallObjectMethod(javaEnvRef, jResponse, jmHttpResponseGetStatusLine);
      resultCode := javaEnvRef^^.CallIntMethod(javaEnvRef, jStatusLine, jmStatusLineGetStatusCode);
      resultString := j.jStringToStringAndDelete(javaEnvRef^^.CallObjectMethod(javaEnvRef, jStatusLine, jmStatusLineGetReasonPhrase));
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, jStatusLine);

      try
        if (resultCode >= 200) and (resultCode <= 250) then begin
          result := j.inputStreamToStringAndDelete(javaEnvRef^^.CallObjectMethod(javaEnvRef, jResult,  jmHttpEntityGetContent));

          lastHTTPHeaders.Clear;
          jHeaderIterator := javaEnvRef^^.CallObjectMethod(javaEnvRef, jResponse,  jmHttpMessageHeaderIterator);
          while javaEnvRef^^.CallBooleanMethod(javaEnvRef, jHeaderIterator,  jmHeaderIteratorHasNext) <> 0 do begin
            jHeader := javaEnvRef^^.CallObjectMethod(javaEnvRef, jHeaderIterator,  jmHeaderIteratorNextHeader);
            lastHTTPHeaders.Add(j.jStringToStringAndDelete(javaEnvRef^^.CallObjectMethod(javaEnvRef, jHeader, jmHeaderGetName)) + '='+
                                j.jStringToStringAndDelete(javaEnvRef^^.CallObjectMethod(javaEnvRef, jHeader, jmHeaderGetValue)));
            javaEnvRef^^.DeleteLocalRef(javaEnvRef, jHeader);
          end;
          javaEnvRef^^.DeleteLocalRef(javaEnvRef, jHeaderIterator);
        end else
          raise EInternetException.Create('Transfer failed: '+inttostr(ResultCode)+': '+resultString+#13#10'when talking to: '+url.combined, resultCode);
          //raise EInternetException.Create('Connecting failed'#13#10'when talking to: '+url.combined);
      finally
        javaEnvRef^^.DeleteLocalRef(javaEnvRef, jResult);
      end;

      if javaEnvRef^^.ExceptionCheck(javaEnvRef) <> JNI_FALSE then begin
        javaEnvRef^^.ExceptionDescribe(javaEnvRef);
        javaEnvRef^^.ExceptionClear(javaEnvRef);
        DebugLn('Warning: Ignoring exception');
      end;

      url.username:=''; url.password:=''; url.linktarget:=''; //keep it secret in referer
      Referer:=url.combined;
      lastConnectedUrl := url;
      lastHTTPResultCode := ResultCode;
    finally
      if  jResponse <> nil then
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, jResponse);
    end;
  end;
end;

function TAndroidInternetAccess.doTransfer(method: string; const url: TDecodedUrl; data: string): string;
begin
  result := doTransfer2(method, url, data);
end;


function TAndroidInternetAccess.GetLastHTTPHeaders: TStringList;
begin
  result := FLastHTTPHeaders;
end;



constructor TAndroidInternetAccess.create();
var args:array[0..1] of jvalue;
    temp: jobject;
    jparams: jobject;
    jlhttpclient: jobject;
begin
  additionalHeaders := TStringList.Create;
  FLastHTTPHeaders := TStringList.Create;

  if javaEnvRef = nil then
    if javaVMRef^^.GetEnv(javaVMRef,@javaEnvRef,JNI_VERSION_1_4) <> 0 then
      raise EInternetException.create('Failed to get VM environment');
  if javaEnvRef = nil then
    raise EInternetException.create('Failed to get VM environment');

  with initializeClasses do begin

    jlhttpclient := javaEnvRef^^.NewObject(javaEnvRef, jcDefaultHttpClient, jmDefaultHttpClientConstructor);
    jhttpclient := javaEnvRef^^.NewGlobalRef(javaEnvRef, jlhttpclient);
    if jhttpclient = nil then
      raise EInternetException.create('Failed to create DefaultHttpClient');
    javaEnvRef^^.DeleteLocalRef(javaEnvRef, jlhttpclient);


    internetConfig:=@defaultInternetConfiguration;
    if defaultInternetConfiguration.userAgent='' then
      defaultInternetConfiguration.userAgent:='Mozilla/3.0 (compatible)';

    jparams := javaEnvRef^^.CallObjectMethod(javaEnvRef, jhttpclient, jmDefaultHttpClientGetParams);

    args[0].l := javaEnvRef^^.NewStringUTF(javaEnvRef, 'http.useragent');
    args[1].l  := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar(defaultInternetConfiguration.userAgent));
    javaEnvRef^^.DeleteLocalRef(javaEnvRef, javaEnvRef^^.CallObjectMethodA(javaEnvRef, jparams, jmHttpParamsSetParameter, @args));
    javaEnvRef^^.DeleteLocalRef(javaEnvRef, args[0].l);
    javaEnvRef^^.DeleteLocalRef(javaEnvRef, args[1].l);

    if defaultInternetConfiguration.useProxy then begin
      args[0].l := javaEnvRef^^.NewStringUTF(javaEnvRef, pchar(internetConfig^.proxyHTTPName));
      args[1].i := StrToIntDef(internetConfig^.proxyHTTPPort, 8080);
      temp := javaEnvRef^^.NewObjectA(javaEnvRef, jcHttpHost, jmHttpHostConstructor, @args);
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, args[0].l);

      args[0].l := javaEnvRef^^.NewStringUTF(javaEnvRef, 'http.route.default-proxy');
      args[1].l := temp;
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, javaEnvRef^^.CallObjectMethodA(javaEnvRef, jparams, jmHttpParamsSetParameter, @args));
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, args[0].l);
      javaEnvRef^^.DeleteLocalRef(javaEnvRef, args[1].l);
    end;

    javaEnvRef^^.DeleteLocalRef(javaEnvRef, jparams);
  end;



  //see http://hc.apache.org/httpcomponents-client-ga/
  //http://developer.android.com/reference/org/apache/http/params/HttpParams.html
  //http://wiki.freepascal.org/Android_Programming
end;

destructor TAndroidInternetAccess.destroy;
begin
  additionalHeaders.free;
  FLastHTTPHeaders.Free;
  if jhttpclient <> nil then javaEnvRef^^.DeleteGlobalRef(javaEnvRef, jhttpclient)
  else DebugLn('Invalid jhttpclient reference (nil)');
  inherited destroy;
end;

function TAndroidInternetAccess.needConnection(): boolean;
begin
  result:=existsConnection();
end;

procedure TAndroidInternetAccess.closeOpenedConnections();
begin
  //TODO
end;

function TAndroidInternetAccess.internalHandle: TObject;
begin
 result:=tobject(jhttpclient);
end;

{$ENDIF}

end.

