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
{**This unit contains the wrapper for Android.}
unit androidinternetaccess;


{$mode objfpc}{$H+}
{$IFDEF ANDROID}
{$DEFINE COMPILE_ANDROID_INTERNETACCESS} //If this unit should be compiled.
{$DEFINE USE_ANDROID_WRAPPER}
{$ENDIF}


{$IFDEF PASDOCRUN} //for documentation purposes
{$DEFINE COMPILE_ANDROID_INTERNETACCESS}
{$ENDIF}


interface

{$IFDEF COMPILE_ANDROID_INTERNETACCESS}

uses
  Classes, SysUtils, internetAccess,
  jni, LCLProc //if these units are not found, you need to add lcl (or perhaps lclbase) to the project requirements
  ;

type
{ TAndroidInternetAccess }

//**@abstract(Internet access class using the Apache HttpComponents on Android.)
//**Set defaultInternetAccessClass to TAndroidInternetAccess if you want to use this class on Android.@br
//**Additionally bbjniutils.jvmref must be set to the Java VM reference.@br
//**This class handles cookies. Apaches's HttpClient also handles cookies. This might lead to duplicated cookie headers, if you do not disable Apache's handling with a custom http client class on the Java side.
TAndroidInternetAccess=class(TInternetAccess)
protected
  jhttpclient: jobject;
  procedure doTransferUnchecked(method:string; const url: TDecodedUrl; const data: TInternetAccessDataBlock);override;
  function ExceptionCheckAndClear: boolean;
public
  constructor create;override;
  destructor destroy;override;
  function needConnection():boolean;override;
  procedure closeOpenedConnections();override;

  function internalHandle: TObject; override;

  //**sets username and password for an host
  function setCredentials(const username, password: string; const host: string): boolean;
end;
TAndroidInternetAccessClass = class of TAndroidInternetAccess;

{$ENDIF}

var defaultHttpClientClass: jclass = nil;

implementation

{$IFDEF COMPILE_ANDROID_INTERNETACCESS}

uses bbutils,
     //bbdebugtools,
     bbjniutils; //if this unit is not found you need to add the ../system directory to the search paths

type THttpMethod = (hmDelete, hmGet, hmHead, hmOptions, hmPost, hmPut, hmTrace);
const methodCamelNames: array[THttpMethod] of string = ('Delete', 'Get', 'Head', 'Options', 'Post', 'Put', 'Trace');
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
    jmHttpParamsSetParameter, jmHttpParamsSetBooleanParameter: jmethodID;
    jcHttpHost: jclass;
    jmHttpHostConstructor, jmHttpHostToUri: jmethodID;

    jiHttpMessage: jclass;
    jiHeaderIterator: jclass;
    jiHeader: jclass;
    jiHttpParams: jclass;

    jcBasicHttpContext: jclass;
    jmBasicHttpContextInit, jmBasicHttpContextGetAttribute: jmethodID;
    jcHttpUriRequest: jclass;
    jmHttpUriRequestGetURI: jmethodID;
    jmObjectToString: jmethodID;
    jcUri: jclass;
    jmURIIsAbsolute: jmethodID;
end;

//threadvar initialized: boolean;
//          cache: TClassInformation;

//finds the necessary JNI classes
//was supposed to cache the classes and call findclass only once, but the jclass is garbage collected,
//and saving them in a global reference causes more trouble than the refinding them everytime
function initializeClasses(): TClassInformation;

  function getc(n: pchar): jclass; inline;
  begin
    result := j.getclass(n);
  end;
  function getm(c: jclass; n, sig: pchar): jmethodID; inline;
  begin
    result := j.getmethod(c, n, sig);
  end;

var
  m: THttpMethod;
  jcObject: jclass;

begin
  needJ;
  with result do begin    //TODO: merge with androidutils of VideLibri
     jcDefaultHttpClient := defaultHttpClientClass;
     if jcDefaultHttpClient = nil then jcDefaultHttpClient := getc('org/apache/http/impl/client/DefaultHttpClient');
     jmDefaultHttpClientConstructor := getm(jcDefaultHttpClient, '<init>', '()V');
     jmDefaultHttpClientExecute := getm(jcDefaultHttpClient, 'execute', '(Lorg/apache/http/client/methods/HttpUriRequest;Lorg/apache/http/protocol/HttpContext;)Lorg/apache/http/HttpResponse;');
     jmDefaultHttpClientGetParams := getm(jcDefaultHttpClient, 'getParams', '()Lorg/apache/http/params/HttpParams;');

     jiHttpParams := getc('org/apache/http/params/HttpParams');
     jmHttpParamsSetParameter := getm(jiHttpParams, 'setParameter', '(Ljava/lang/String;Ljava/lang/Object;)Lorg/apache/http/params/HttpParams;');
     jmHttpParamsSetBooleanParameter := getm(jiHttpParams, 'setBooleanParameter', '(Ljava/lang/String;Z)Lorg/apache/http/params/HttpParams;');

     jcHttpHost := getc('org/apache/http/HttpHost');
     jmHttpHostConstructor := getm(jcHttpHost, '<init>', '(Ljava/lang/String;I)V');
     jmHttpHostToUri := getm(jcHttpHost, 'toURI', '()Ljava/lang/String;');

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


     jcBasicHttpContext := getc('org/apache/http/protocol/BasicHttpContext');
     jmBasicHttpContextInit := getm(jcBasicHttpContext, '<init>', '()V');
     jmBasicHttpContextGetAttribute := getm(jcBasicHttpContext, 'getAttribute', '(Ljava/lang/String;)Ljava/lang/Object;');

     jcHttpUriRequest := getc('org/apache/http/client/methods/HttpUriRequest');
     jmHttpUriRequestGetURI := getm(jcHttpUriRequest, 'getURI', '()Ljava/net/URI;');

     jcUri := getc('java/net/URI');
     jmURIIsAbsolute := getm(jcUri, 'isAbsolute', '()Z');

     jcObject := getc('java/lang/Object');
     jmObjectToString := getm(jcObject, 'toString', '()Ljava/lang/String;');
     j.deleteLocalRef(jcObject);
   end;
 // initialized:=true;
  //cache:=result;
end;

procedure freeClasses(const info:TClassInformation);
var
  m: THttpMethod;
begin
  needJ;
  with info do begin
    if jcDefaultHttpClient <> defaultHttpClientClass then j.deleteLocalRef(jcDefaultHttpClient);

    j.deleteLocalRef(jcBasicHttpContext);
    j.deleteLocalRef(jcHttpUriRequest);
    j.deleteLocalRef(jcUri);
    j.deleteLocalRef(jiHttpParams);
    j.deleteLocalRef(jcHttpHost);
    j.deleteLocalRef(jiHttpMessage);
    j.deleteLocalRef(jiHeaderIterator);
    j.deleteLocalRef(jiHeader);
    j.deleteLocalRef(jcByteArrayEntity);
    j.deleteLocalRef(jcAbstractHttpEntity);
    j.deleteLocalRef(jcHttpEntityEnclosingRequestBase);
    j.deleteLocalRef(jiHttpResponse);
    j.deleteLocalRef(jiHttpEntity);
    j.deleteLocalRef(jiStatusLine);
    for m := low(THttpMethod) to high(THttpMethod) do
      j.deleteLocalRef(jcMethods[m]);
  end;
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

type TLocalStackRecord = record
    jRequest: jobject;
    classInfos: TClassInformation;
end;
     PLocalStackRecord = ^TLocalStackRecord;

procedure addHeader(data: pointer; headerKind: TAndroidInternetAccess.THeaderKind; const name, value: string);
var args: array[0..1] of jvalue;
begin
  //jRequest.addHeader(n, v)
  with PLocalStackRecord(data)^ do
    with classInfos do begin
      args[0].l := j.stringToJString(name);
      args[1].l := j.stringToJString(value);
      j.callVoidMethodChecked(jRequest,  jmHttpMessageAddHeader, @args[0]);;
      j.DeleteLocalRef(args[0].l);
      j.DeleteLocalRef(args[1].l);
    end;
end;

procedure TAndroidInternetAccess.doTransferUnchecked(method: string; const url: TDecodedUrl; const data: TInternetAccessDataBlock);
var stack: TLocalStackRecord;


  function setRequestData: boolean;
  var jentity: jvalue;
      wrappedData: jbyteArray;
      temp: jvalue;
  begin
    with stack do
      with classInfos do begin
        if data.isEmpty then exit;
        wrappedData := j.env^^.NewByteArray(j.env, data.count);
        if wrappedData = nil then begin
          lastErrorDetails := 'Failed to allocate JNI array';
          exit(false);
        end;
        j.env^^.SetByteArrayRegion(j.env, wrappedData, 0, data.count, data.data); //todo: is there a faster way than copying?

        //entity = new ByteArrayEntity(data)
        jentity.l := j.env^^.NewObjectA(j.env, jcByteArrayEntity, jmByteArrayEntityConstructor, @wrappedData);
        //entity.setContentType(..)
        if additionalHeaders.IndexOfName('Content-Type') < 0 then begin
          temp.l := j.NewStringUTF(ContentTypeForData);
          j.callVoidMethodChecked(jentity.l,  jmAbstractHttpEntitySetContentType, @temp);;
          j.DeleteLocalRef(temp.l);
        end;
        //httprequest.setEntity(entity)
        j.callVoidMethodChecked(jRequest,  jmHttpEntityEnclosingRequestBaseSetEntity, @jentity);;

        j.DeleteLocalRef(wrappedData);
        result := true;
      end;
  end;

var
  m: THttpMethod;
  jUrl, jResponse, jResult, jStatusLine, jHeaderIterator, jHeader, jContext: jobject;
  args: array[0..1] of jvalue;
  connectionResetRepeat: integer;
  connectionReset, needRequestData: Boolean;
begin
  needJ;
  if j.env^^.ExceptionCheck(j.env) <> JNI_FALSE then begin
    ExceptionCheckAndClear
    //log('Warning: Ignoring exception');
  end;

  m := methodStringToMethod(method);
  needRequestData := m in [hmPut, hmPost];
  with stack do begin
    classInfos := initializeClasses(); //todo: cache?

    connectionResetRepeat := 5;
    connectionReset := true;
    try
      while connectionReset do begin
        connectionReset := false;
        try
          with classInfos do begin;
            //HttpGet httpget = new HttpGet(url);
            jUrl := j.stringToJString(pchar(url.combinedExclude([dupUsername, dupPassword, dupLinkTarget])));
            jRequest := j.env^^.NewObjectA(j.env, jcMethods[m], jmMethodConstructors[m], @jUrl);
            if ExceptionCheckAndClear then exit;
            j.DeleteLocalRef(jUrl);

            enumerateAdditionalHeaders(url, @addHeader, not data.isEmpty, @stack);
            if (not data.isEmpty) and needRequestData then
              if not setRequestData() then begin
                lastErrorDetails:= 'Failed to set request data';
                exit;
              end;

            jContext := j.newObject(jcBasicHttpContext, jmBasicHttpContextInit);
            if ExceptionCheckAndClear then exit;

            if url.username <> '' then
              //problem: this is host specific, so it does not work for redirections
              if not setCredentials(strUnescapeHex(url.username, '%'), strUnescapeHex(url.password, '%'), url.host ) then begin
                lastErrorDetails:= 'Failed to set credentials';
                exit;
              end;


            //send
            args[0].l := jRequest;
            args[1].l := jContext;
            jResponse := j.CallObjectMethod(jhttpclient,  jmDefaultHttpClientExecute, @args);;

            if ExceptionCheckAndClear then exit;

            try
              //process
              jResult := j.CallObjectMethod(jResponse,  jmHttpResponseGetEntity);
              if ExceptionCheckAndClear then exit;

              jStatusLine := j.callObjectMethodChecked(jResponse, jmHttpResponseGetStatusLine);
              lastHTTPResultCode := j.callIntMethodChecked(jStatusLine, jmStatusLineGetStatusCode);
              lastErrorDetails := j.jStringToStringAndDelete(j.callObjectMethodChecked(jStatusLine, jmStatusLineGetReasonPhrase));
              j.DeleteLocalRef(jStatusLine);

              try
                lastHTTPHeaders.Clear;
                jHeaderIterator := j.callObjectMethodChecked(jResponse,  jmHttpMessageHeaderIterator);
                while j.CallBooleanMethod(jHeaderIterator,  jmHeaderIteratorHasNext) do begin
                  jHeader := j.callObjectMethodChecked(jHeaderIterator,  jmHeaderIteratorNextHeader);
                  lastHTTPHeaders.Add(j.jStringToStringAndDelete(j.callObjectMethodChecked(jHeader, jmHeaderGetName)) + ':'+
                                      j.jStringToStringAndDelete(j.callObjectMethodChecked(jHeader, jmHeaderGetValue)));
                  j.DeleteLocalRef(jHeader);
                end;
                j.DeleteLocalRef(jHeaderIterator);

                j.inputStreamReadAllAndDelete( j.callObjectMethodChecked(jResult,  jmHttpEntityGetContent), @writeBlock);
              finally
                j.DeleteLocalRef(jResult);
              end;

              if j.env^^.ExceptionCheck(j.env) <> JNI_FALSE then begin
                ExceptionCheckAndClear
                //log('Warning: Ignoring exception');
              end;

            finally
              if  jResponse <> nil then
                j.DeleteLocalRef(jResponse);
            end;

            j.deleteLocalRef(jRequest);
            j.deleteLocalRef(jContext);
          end;
        except
          on e: EAndroidInterfaceException do begin
            if (connectionResetRepeat > 0)
               and (
                ((strContains(e.Message, 'javax.net.ssl.SSLHandshakeException') and strContains(e.Message, 'I/O error during system call')))
                or (not needRequestData and
                    (
                    (strContains(e.Message, 'javax.net.ssl.SSLException') and strContains(e.Message, 'I/O error during system call'))
                    or (strContains(e.Message, 'java.net.SocketException') and strContains(e.Message, 'recvfrom failed: ETIMEDOUT'))
                    )
                   )
               )
            then begin
              dec(connectionResetRepeat);
              Sleep(2000);
              connectionReset := true;
            end else raise e;
          end;
        end;
      end;
    finally
      freeClasses(classInfos);
    end;
  end;
end;

function TAndroidInternetAccess.ExceptionCheckAndClear: boolean;
begin
  result := j.ExceptionCheck;
  if result then lastErrorDetails := j.ExceptionDescribeAndClear
end;


constructor TAndroidInternetAccess.create();
var args:array[0..1] of jvalue;
    temp: jobject;
    jparams: jobject;
    tempClasses: TClassInformation;

begin
  init;

  tempClasses := initializeClasses();
  try
    with needJ do
      with tempClasses do begin
        jhttpclient := newGlobalRefAndDelete(NewObject(jcDefaultHttpClient, jmDefaultHttpClientConstructor));
        if jhttpclient = nil then
          raise EInternetException.create('Failed to create DefaultHttpClient');


        jparams := CallObjectMethod(jhttpclient, jmDefaultHttpClientGetParams);

        args[0].l := NewStringUTF('http.useragent');
        args[1].l  := j.stringToJString(internetConfig^.userAgent);
        deleteLocalRef(callObjectMethod(jparams, jmHttpParamsSetParameter, @args));
        deleteLocalRef(args[0].l);
        deleteLocalRef(args[1].l);

        //disable 3xx handling, so we can handle it ourselves like on all other platforms
        args[0].l := NewStringUTF('http.protocol.handle-redirects');
        args[1].z  := JNI_FALSE;
        DeleteLocalRef(CallObjectMethod(jparams, jmHttpParamsSetBooleanParameter, @args));
        DeleteLocalRef(args[0].l);

        if internetConfig^.useProxy then begin
          args[0].l := stringToJString(internetConfig^.proxyHTTPName);
          args[1].i := StrToIntDef(internetConfig^.proxyHTTPPort, 8080);
          temp := NewObject(jcHttpHost, jmHttpHostConstructor, @args);
          DeleteLocalRef(args[0].l);

          args[0].l := NewStringUTF('http.route.default-proxy');
          args[1].l := temp;
          DeleteLocalRef(CallObjectMethod(jparams, jmHttpParamsSetParameter, @args));
          DeleteLocalRef(args[0].l);
          DeleteLocalRef(args[1].l);
        end;

        DeleteLocalRef(jparams);
      end;
  finally
    freeClasses(tempClasses);
  end;



  //see http://hc.apache.org/httpcomponents-client-ga/
  //http://developer.android.com/reference/org/apache/http/params/HttpParams.html
  //http://wiki.freepascal.org/Android_Programming
end;

destructor TAndroidInternetAccess.destroy;
begin
  if jhttpclient <> nil then needj.DeleteGlobalRef(jhttpclient);
  //else log('Invalid jhttpclient reference (nil)');
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

function TAndroidInternetAccess.setCredentials(const username, password: string; const host: string): boolean;
var jcCredentialsProvider, jcAbstractHttpClient, jcAuthScope, jcUsernamePasswordCredentials: jclass;
    jmAbstractHttpClientGetCredentialsProvider, jmCredentialsProviderSetCredentials, jmAuthScopeConstructor, jmUsernamePasswordCredentialsConstructor: jmethodID;
    jhost, jusername, jpassword, authScope, creds, credProvider: jclass;
    tempargs: array[0..1] of jvalue;
begin
  if jhttpclient = nil then begin
    lastErrorDetails := 'HttpClient not created';
    exit(false);
  end;
  //jhttpclient.getCredentialsProvider().setCredentials(new AuthScope(host, AuthScope.ANY_PORT), new UsernamePasswordCredentials(username, password));
  with needJ do begin
    jcAbstractHttpClient := getclass('org/apache/http/impl/client/AbstractHttpClient');
    jmAbstractHttpClientGetCredentialsProvider := getmethod(jcAbstractHttpClient, 'getCredentialsProvider', '()Lorg/apache/http/client/CredentialsProvider;');
    jcCredentialsProvider := getclass('org/apache/http/client/CredentialsProvider');
    jmCredentialsProviderSetCredentials := getmethod(jcCredentialsProvider, 'setCredentials', '(Lorg/apache/http/auth/AuthScope;Lorg/apache/http/auth/Credentials;)V');
    jcAuthScope := getclass('org/apache/http/auth/AuthScope');
    jmAuthScopeConstructor := getmethod(jcAuthScope, '<init>', '(Ljava/lang/String;I)V');
    jcUsernamePasswordCredentials := getclass('org/apache/http/auth/UsernamePasswordCredentials');
    jmUsernamePasswordCredentialsConstructor := getmethod(jcUsernamePasswordCredentials, '<init>', '(Ljava/lang/String;Ljava/lang/String;)V');

    try
      if host <> '' then jhost := stringToJString(host)
      else jhost := nil; //httpclient documentation says we can use null to set if for all hosts. But this this DOES NOT WORK and raises an illegalargumentexception.
      jusername := stringToJString(username);
      jpassword := stringToJString(password);

      try
        tempargs[0].l := jhost;
        tempargs[1].i := {AuthScope.ANY_PORT} -1;
        authScope := newObject(jcAuthScope, jmAuthScopeConstructor, @tempargs);
        tempargs[0].l := jusername;
        tempargs[1].l := jpassword;
        creds := newObject(jcUsernamePasswordCredentials, jmUsernamePasswordCredentialsConstructor, @tempargs);
        try
          credProvider := callObjectMethod(jhttpclient, jmAbstractHttpClientGetCredentialsProvider);
          tempargs[0].l := authScope;
          tempargs[1].l := creds;
          callVoidMethod(credProvider, jmCredentialsProviderSetCredentials, @tempargs);
          deleteLocalRef(credProvider);
        finally
          deleteLocalRef(authScope);
          deleteLocalRef(creds);
        end;
      finally
        deleteLocalRef(jpassword);
        deleteLocalRef(jusername);
        if jhost <> nil then deleteLocalRef(jhost);
      end;
    finally
      deleteLocalRef(jcCredentialsProvider);
      deleteLocalRef(jcAuthScope);
      deleteLocalRef(jcUsernamePasswordCredentials);
    end;
    result := true;
  end;
end;


{$ENDIF}


initialization
{$IFDEF USE_ANDROID_WRAPPER}
defaultInternetAccessClass := TAndroidInternetAccess;
{$ENDIF}

end.

