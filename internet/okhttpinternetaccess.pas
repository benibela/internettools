{Copyright (C) 2018  Benito van der Zander

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
{**This unit contains the wrapper TOKHTTPInternetAccess for OkHttp on Android.}
unit okhttpinternetaccess;


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
  Classes, SysUtils, internetAccess, jni;

type
//**@abstract(Internet access class using the Java OkHttp library.)
//**Set defaultInternetAccessClass to TOKHTTPInternetAccess if you want to use this class on Android.@br
//**Additionally bbjniutils.jvmref must be set to the Java VM reference.@br
//**Use the global onBuildCallback to customize the OkHttpClient.@br
//**Note: OkHttp is not installed on Android, you need to include it in your Java dependencies.
//**You cannot use it on a main thread in an app, because Android does not allow internet access from the main thread.
//**If you create a thread in Pascal, you likely cannot create the OkHttp client there, since the thread class loader cannot access dependencies only Android classes.
//**Hence you need to create it on the main thread, or keep a class loader reference in bbjniutils.jCustomClassLoader @br
//**Not yet supported: proxies, user/password authentication
TOKHTTPInternetAccess=class(TInternetAccess)
protected
  procedure doTransferUnchecked(method:string; const url: TDecodedUrl; const data: TInternetAccessDataBlock);override;
  function ExceptionCheckAndClear: boolean;
public
  constructor create;override;
  destructor destroy;override;

  function internalHandle: TObject; override;
end;
TOKHTTPInternetAccessClass = class of TOKHTTPInternetAccess;

TOKHTTPBuilderCallback = procedure (sender: TOKHTTPInternetAccess; builder: jobject) of object;

//** Called when the OkHttpClient is created using a OkHttpClient.Builder. This is only called once single the OkHttpClient is used as singleton
var onBuildCallback: TOKHTTPBuilderCallback;
{$ENDIF}


implementation

{$IFDEF COMPILE_ANDROID_INTERNETACCESS}

uses bbutils,
     //bbdebugtools,
     bbjniutils; //if this unit is not found you need to add the ../system directory to the search paths

var okHttp: record
      client: jclass; //global OkHttpClient
      clientCreationCriticalSection: TRTLCriticalSection;

      ClientMethods: record
        newCall: jmethodID;
      end;

      RequestBuilderClass: jclass;
      RequestBuilderMethods: record
        init: jmethodID;
        addHeader, build, method, url: jmethodID
      end;

      RequestBodyClass: jclass;
      RequestBodyMethods: record
        create: jmethodID;
      end;

      MediaTypeClass: jclass;
      MediaTypeMethods: record
        parse: jmethodID;
      end;

      CallClass: jclass;
      CallMethods: record
        execute: jmethodID;
      end;

      ResponseClass: jclass;
      ResponseMethods: record
        body, code, headers, message, close: jmethodID;
      end;

      ResponseBodyClass: jclass;
      ResponseBodyMethods: record
        bytestream: jmethodID;
      end;

      HeadersClass: jclass;
      HeadersMethods: record
        name, value, size: jmethodID;
      end;
  end;

procedure addHeader(jrequestbuilder: jobject; {%H-}headerKind: TOKHTTPInternetAccess.THeaderKind; const name, value: string);
var args: array[0..1] of jvalue;
begin
  //jRequest.addHeader(n, v)
  with j do begin
    args[0].l := stringToJString(name);
    args[1].l := stringToJString(value);
    deleteLocalRef(jrequestbuilder.callObjectMethodChecked(okHttp.RequestBuilderMethods.addHeader, @args[0]));
    DeleteLocalRef(args[0].l);
    DeleteLocalRef(args[1].l);
  end;
end;

procedure TOKHTTPInternetAccess.doTransferUnchecked(method: string; const url: TDecodedUrl; const data: TInternetAccessDataBlock);
var jRequestBuilder: jobject;
var jUrl, jrequestbody, jrequest, jmediatype, jcall, jresponse, jbody, jheaders: jobject;
  contenttype: String;
  wrappedData: jbyteArray;
  args: array[0..1] of jvalue;
  headerCount: jint;
  i: Integer;
begin
  with needJ, okHttp do begin
    ExceptionCheckAndClear;

    jRequestBuilder := RequestBuilderClass.NewObject(RequestBuilderMethods.init);
    if ExceptionCheckAndClear then exit;

    jUrl := stringToJString(url.combinedExclude([dupUsername, dupPassword, dupLinkTarget]));
    if ExceptionCheckAndClear then exit;
    jRequestBuilder.callObjectMethodChecked(RequestBuilderMethods.url, @jurl).deleteLocalRef();
    jurl.deleteLocalRef();

    //todo: user/password

    if not data.isEmpty then begin
      wrappedData := env^^.NewByteArray(env, data.count);
      if wrappedData = nil then begin
        lastErrorDetails := 'Failed to allocate JNI array';
        exit;
      end;
      env^^.SetByteArrayRegion(env, wrappedData, 0, data.count, data.data); //todo: is there a faster way than copying?

      //mediatype = MediaType.parse(contenttype)
      contenttype := trim(additionalHeaders.Values['Content-Type']);
      if contenttype = '' then contenttype := ContentTypeForData;
      args[0].l := stringToJString(contenttype);
      jmediatype :=  MediaTypeClass.callStaticObjectMethodChecked(MediaTypeMethods.parse, @args[0]);
      DeleteLocalRef(args[0].l);

      //body = RequestBody.create(mediatype, data)
      args[0].l := jmediatype;
      args[1].l := wrappedData;
      jrequestbody := RequestBodyClass.callStaticObjectMethodChecked(RequestBodyMethods.create, @args);
      j.DeleteLocalRef(wrappedData);
      j.DeleteLocalRef(jmediatype);
    end else jrequestbody := nil;

    //builder.method(method, body)
    args[0].l := stringToJString(method);
    args[1].l := jrequestbody;
    jRequestBuilder.callObjectMethodChecked(RequestBuilderMethods.method, @args).deleteLocalRef();
    deleteLocalRef(args[0].l);
    if args[1].l <> nil then deleteLocalRef(args[1].l);

    enumerateAdditionalHeaders(url, @addHeader, not data.isEmpty, jRequestBuilder);
    if additionalHeaders.IndexOfName('User-Agent') < 0 then
       addHeader(jRequestBuilder, iahUserAgent, 'User-Agent', internetConfig^.userAgent);

    //request = builder.build()
    jrequest := jRequestBuilder.callObjectMethodChecked(RequestBuilderMethods.build);
    jRequestBuilder.deleteLocalRef();

    //call = client.newCall(request)
    jcall := Client.callObjectMethodChecked(ClientMethods.newCall, @jrequest);
    jrequest.deleteLocalRef();

    //response = call.execute()
    jresponse := jcall.callObjectMethod(CallMethods.execute);
    if ExceptionCheckAndClear then
       exit; //here we do not want to raise an exception if the HTTP request fails. The caller will raise it if lastHTTPResultCode is not set
    jcall.deleteLocalRef();

    lastHTTPResultCode := jresponse.callIntMethodChecked(ResponseMethods.code);
    lastErrorDetails := jresponse.callStringMethodChecked(ResponseMethods.message);

    jbody := jresponse.callObjectMethodChecked(ResponseMethods.body);
    inputStreamReadAllAndDelete( jbody.callObjectMethodChecked(ResponseBodyMethods.bytestream), @writeBlock);
    deleteLocalRef(jbody);

    lastHTTPHeaders.Clear;
    jheaders := jresponse.callObjectMethodChecked(ResponseMethods.headers);
    headerCount := callIntMethodChecked(jheaders, HeadersMethods.size);
    for i := 0 to headerCount - 1 do begin
      args[0].i := i;;
      lastHTTPHeaders.add( callStringMethodChecked(jheaders, HeadersMethods.name, @args[0]) + ':' + callStringMethodChecked(jheaders, HeadersMethods.value, @args[0]));
    end;
    deleteLocalRef(jheaders);

    jresponse.callVoidMethodChecked(ResponseMethods.close);
    deleteLocalRef(jresponse);
  end;
end;

function TOKHTTPInternetAccess.ExceptionCheckAndClear: boolean;
begin
  result := j.ExceptionCheck;
  if result then lastErrorDetails := j.ExceptionDescribeAndClear
end;


constructor TOKHTTPInternetAccess.create();
const BUILDER = 'okhttp3/OkHttpClient$Builder';
      REQUEST_BUILDER = 'okhttp3/Request$Builder';
var
  jbuilder: jobject;
  jbuilderClass: jclass;
  tempargs: jvalue;
begin
  init;
  needJ;
  with okHttp do begin
    EnterCriticalsection(clientCreationCriticalSection);
    try
      if Client = nil then with j do begin
        jbuilderClass := getclass(BUILDER);
        jbuilder := jbuilderClass.NewObject();
        //tempargs.l := nil;
        //jbuilder := jbuilder.callObjectMethodChecked(jbuilderClass.getmethod('cookieJar', '(Lokhttp3/CookieJar;)L'+BUILDER+';'), @tempargs);
        tempargs.z := JNI_FALSE;
        jbuilder.callObjectMethodChecked(jbuilderClass.getmethod('followSslRedirects', '(Z)L'+BUILDER+';'), @tempargs).deleteLocalRef();
        jbuilder.callObjectMethodChecked(jbuilderClass.getmethod('followRedirects', '(Z)L'+BUILDER+';'), @tempargs).deleteLocalRef();

        //todo: proxy

        if Assigned(onBuildCallback) then onBuildCallback(self, jbuilder);

        Client := jbuilder.callObjectMethodChecked(jbuilderClass.getmethod('build', '()Lokhttp3/OkHttpClient;')).newGlobalRefAndDelete();
        jbuilder.deleteLocalRef();
        jbuilderClass.deleteLocalRef();

        ClientMethods.newCall := getclass('okhttp3/OkHttpClient').getmethod('newCall', '(Lokhttp3/Request;)Lokhttp3/Call;');

        RequestBuilderClass := getclass(REQUEST_BUILDER).newGlobalRefAndDelete();
        RequestBuilderMethods.init := RequestBuilderClass.getmethod('<init>', '()V');
        RequestBuilderMethods.addHeader := RequestBuilderClass.getmethod('addHeader', '(Ljava/lang/String;Ljava/lang/String;)L'+REQUEST_BUILDER+';');
        RequestBuilderMethods.build := RequestBuilderClass.getmethod('build', '()Lokhttp3/Request;');
        RequestBuilderMethods.method := RequestBuilderClass.getmethod('method', '(Ljava/lang/String;Lokhttp3/RequestBody;)L'+REQUEST_BUILDER+';');
        RequestBuilderMethods.url := RequestBuilderClass.getmethod('url', '(Ljava/lang/String;)L'+REQUEST_BUILDER+';');

        RequestBodyClass := getclass('okhttp3/RequestBody').newGlobalRefAndDelete();
        RequestBodyMethods.create := j.getstaticmethod(RequestBodyClass, 'create', '(Lokhttp3/MediaType;[B)Lokhttp3/RequestBody;');

        MediaTypeClass := getclass('okhttp3/MediaType').newGlobalRefAndDelete();
        MediaTypeMethods.parse := j.getstaticmethod(MediaTypeClass, 'parse', '(Ljava/lang/String;)Lokhttp3/MediaType;');

        CallClass := getclass('okhttp3/Call');
        CallMethods.execute := CallClass.getmethod('execute', '()Lokhttp3/Response;');

        ResponseClass := getclass('okhttp3/Response');
        ResponseMethods.body := ResponseClass.getmethod('body', '()Lokhttp3/ResponseBody;');
        ResponseMethods.code := ResponseClass.getmethod('code', '()I');
        ResponseMethods.headers := ResponseClass.getmethod('headers', '()Lokhttp3/Headers;');
        ResponseMethods.message := ResponseClass.getmethod('message', '()Ljava/lang/String;');
        ResponseMethods.close := ResponseClass.getmethod('close', '()V');

        ResponseBodyClass := getclass('okhttp3/ResponseBody');
        ResponseBodyMethods.bytestream := ResponseBodyClass.getmethod('byteStream', '()Ljava/io/InputStream;');

        HeadersClass := getclass('okhttp3/Headers');
        HeadersMethods.name := HeadersClass.getmethod('name', '(I)Ljava/lang/String;');
        HeadersMethods.value := HeadersClass.getmethod('value', '(I)Ljava/lang/String;');
        HeadersMethods.size := HeadersClass.getmethod('size', '()I');

      end;
    finally
      LeaveCriticalsection(ClientCreationCriticalSection);
    end;
  end;
end;

destructor TOKHTTPInternetAccess.destroy;
begin
  inherited destroy;
end;

function TOKHTTPInternetAccess.internalHandle: TObject;
begin
 result:=tobject(okHttp.Client);
end;


{$ENDIF}


initialization
  InitCriticalSection(okHttp.ClientCreationCriticalSection);
{$IFDEF USE_ANDROID_WRAPPER}
//defaultInternetAccessClass := TOKHTTPInternetAccess;
{$ENDIF}

finalization
  DoneCriticalsection(okHttp.ClientCreationCriticalSection);


end.

