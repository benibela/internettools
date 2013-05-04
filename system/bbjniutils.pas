unit bbjniutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni;

type EAndroidInterfaceException = class(Exception);

//{$modeswitch advancedrecords}

type

{ TJavaEnv }

 TJavaEnv = object
  env: PJNIEnv;

  function getclass(n: pchar): jclass;
  function getmethod(c: jclass; n, sig: pchar): jmethodID;
  function getmethod(classname: pchar; n, sig: pchar): jmethodID;

  procedure callVoidMethod(obj: jobject; methodID: jmethodID); inline;
  procedure callVoidMethod(obj: jobject; methodID: jmethodID; args: Pjvalue); inline;
  function callObjMethod(obj: jobject;  methodID: jmethodID): jobject; inline;
  function callObjMethod(obj: jobject;  methodID: jmethodID; args: Pjvalue): jobject; inline;

  procedure callVoidMethodChecked(obj: jobject; methodID: jmethodID); inline;
  procedure callVoidMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue); inline;
  function callObjMethodChecked(obj: jobject;  methodID: jmethodID): jobject; inline;
  function callObjMethodChecked(obj: jobject;  methodID: jmethodID; args: Pjvalue): jobject; inline;

  procedure deleteLocalRef(obj: jobject); inline;

  function stringToJString(s: string): jobject;
  function jStringToStringAndDelete(s: jobject): string;

  procedure RethrowJavaExceptionIfThereIsOne(aExceptionClass: ExceptClass);
  procedure RethrowJavaExceptionIfThereIsOne();

  function inputStreamToStringAndDelete(stream: jobject; jmInputStreamRead: jmethodID): string;
  function inputStreamToStringAndDelete(stream: jobject): string; //same as in androidinternetaccess

  {$ifdef android}
  function getAssets: jobject;
  function getAssetAsString(name: string): string;
  function getAssetAsString(name: string; jmAssetManagerOpen, jmInputStreamRead: jmethodID): string;
  function getAssetAsString(assets: jobject; name: string): string;
  function getAssetAsString(assets: jobject; name: string; jmAssetManagerOpen, jmInputStreamRead: jmethodID): string;

  {$endif}


  function commonMethods_InputStream_Read_B: jmethodID; //todo cache
  function commonMethods_AssetManager_Open_StringInputStream: jmethodID; //todo cache
end;

const javaEnvRef: integer = $deadbeef; //block access to CustomDrawnInt.javaEnvRef because it is not thread safe
threadvar j: TJavaEnv; //this is an object to reduce the overhead caused by being a threadvar (i.e. you can write with j ...)

var jvmref: PJavaVM;
    jActivityObject: jobject;

var onLoad: function: integer;

function needJ: TJavaEnv;


function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; cdecl;
procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;


implementation

uses bbutils {$IFDEF CD_Android}, customdrawnint{$endif};

function needJ: TJavaEnv;
begin
  {$IFDEF CD_Android}if jvmref = nil then jvmref:=javaVMRef;{$endif}
  if j.env = nil then
    if jvmref^^.GetEnv(jvmref,@j.env,JNI_VERSION_1_4) <> 0 then
      raise EAndroidInterfaceException.create('Failed to get VM environment');
  if j.env = nil then
    raise EAndroidInterfaceException.create('Failed to get VM environment');
  result := j;
end;

function JNI_OnLoad(vm: PJavaVM; reserved: pointer): jint; cdecl;
begin
  jvmref := vm;
  if assigned(onLoad) then result := onload()
  else result := JNI_VERSION_1_4;
end;

procedure JNI_OnUnload(vm: PJavaVM; reserved: pointer); cdecl;
begin

end;

function TJavaEnv.getclass(n: pchar): jclass;
begin
  result := env^^.FindClass(env, n);
  if (result = nil) or (env^^.ExceptionCheck(env)<>0) then
    raise EAndroidInterfaceException.Create('TAndroidInternetAccess: Failed to find class: '+string(n));

end;
function TJavaEnv.getmethod(c: jclass; n, sig: pchar): jmethodID;
begin
  result := env^^.GetMethodID(env, c, n, sig);
  if (result = nil) or (env^^.ExceptionCheck(env)<>0) then
    raise EAndroidInterfaceException.Create('TAndroidInternetAccess: Failed to find method: '+string(n)+' '+string(sig));
end;

function TJavaEnv.getmethod(classname: pchar; n, sig: pchar): jmethodID;
begin
  result := getmethod(getclass(classname), n, sig);
end;

procedure TJavaEnv.callVoidMethod(obj: jobject; methodID: jmethodID); inline;
begin
  env^^.CallVoidMethod(env, obj, methodID);
end;

procedure TJavaEnv.callVoidMethod(obj: jobject; methodID: jmethodID; args: Pjvalue);
begin
  env^^.CallVoidMethodA(env, obj, methodID, args);
end;

function TJavaEnv.callObjMethod(obj: jobject; methodID: jmethodID): jobject; inline;
begin
  result := env^^.CallObjectMethod(env, obj, methodID);
end;

function TJavaEnv.callObjMethod(obj: jobject; methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := env^^.CallObjectMethodA(env, obj, methodID, args);
end;

procedure TJavaEnv.callVoidMethodChecked(obj: jobject; methodID: jmethodID);
begin
  callVoidMethod(obj, methodID);
  RethrowJavaExceptionIfThereIsOne();
end;

procedure TJavaEnv.callVoidMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue);
begin
  callVoidMethod(obj, methodID, args);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callObjMethodChecked(obj: jobject; methodID: jmethodID): jobject;
begin
  result := callObjMethod(obj, methodID);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callObjMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := callObjMethod(obj, methodID, args);
  RethrowJavaExceptionIfThereIsOne();
end;

procedure TJavaEnv.deleteLocalRef(obj: jobject);
begin
  env^^.DeleteLocalRef(env, obj);
end;

function TJavaEnv.stringToJString(s: string): jobject;
begin
  result := env^^.NewStringUTF(env, pchar(s));
end;

function TJavaEnv.jStringToStringAndDelete(s: jobject): string;
var chars: pchar;
begin
  if s = nil then exit('< (null) >');
  //setlength(result, env^^.GetStringUTFLength(env, s));
  chars := env^^.GetStringUTFChars(env, s, nil);
  if chars = nil then result := ''
  else result := chars;
  env^^.ReleaseStringUTFChars(env, s, chars);
  env^^.DeleteLocalRef(env, s);
end;


procedure TJavaEnv.RethrowJavaExceptionIfThereIsOne(aExceptionClass: ExceptClass);
var je: jthrowable;
    temp: jobject;
    message: String;
begin
  if env^^.ExceptionCheck(env) <> JNI_FALSE then begin
    je := env^^.ExceptionOccurred(env);
    env^^.ExceptionDescribe(env);
    env^^.ExceptionClear(env);
    temp:= env^^.CallObjectMethod(env, je, getmethod('java/lang/Object', 'getClass', '()Ljava/lang/Class;'));
    message := 'Java Internet Exception '
                   + jStringToStringAndDelete(env^^.CallObjectMethod(env, temp, getmethod('java/lang/Class', 'getName', '()Ljava/lang/String;'))) + ': '
                   + jStringToStringAndDelete(env^^.CallObjectMethod(env, je, getmethod('java/lang/Throwable', 'getMessage', '()Ljava/lang/String;')));
    env^^.DeleteLocalRef(env, temp);
    env^^.DeleteLocalRef(env, je);
    raise aexceptionClass.create(message);
  end;
end;

procedure TJavaEnv.RethrowJavaExceptionIfThereIsOne;
begin
  RethrowJavaExceptionIfThereIsOne(EAndroidInterfaceException);
end;


function TJavaEnv.inputStreamToStringAndDelete(stream: jobject; jmInputStreamRead: jmethodID): string;
var wrappedBuffer: jvalue;
    len: integer;
    oldlen: Integer;
begin
  if stream = nil then raise EAndroidInterfaceException.create('No stream');

  result := '';
  wrappedBuffer.l := env^^.NewByteArray(env, 16384);
  len := env^^.CallIntMethodA(env, stream,  jmInputStreamRead, @wrappedBuffer);;
  RethrowJavaExceptionIfThereIsOne;
  while len >= 0 do begin
    if len > 0 then begin
      oldlen := length(result);
      setlength(result, oldlen + len);
      env^^.GetByteArrayRegion(env, wrappedBuffer.l, 0, len, @result[oldlen+1]); //todo: faster way than copying?
    end;
    len := env^^.CallIntMethodA(env, stream,  jmInputStreamRead, @wrappedBuffer);;
    RethrowJavaExceptionIfThereIsOne;
  end;

  env^^.DeleteLocalRef(env, wrappedBuffer.l);
  env^^.DeleteLocalRef(env, stream);
end;

function TJavaEnv.inputStreamToStringAndDelete(stream: jobject): string;
begin
  result := inputStreamToStringAndDelete(commonMethods_InputStream_Read_B);
end;

{$ifdef android}
function TJavaEnv.getAssets: jobject;
begin
  result := callObjMethodChecked(jActivityObject, getmethod('android/content/Context', 'getAssets', '()Landroid/content/res/AssetManager;'));
end;

function TJavaEnv.getAssetAsString(name: string): string;
begin
  result := getAssetAsString(getAssets, name);
end;

function TJavaEnv.getAssetAsString(name: string; jmAssetManagerOpen, jmInputStreamRead: jmethodID): string;
begin
  result := getAssetAsString(getAssets, name, jmAssetManagerOpen, jmInputStreamRead);
end;

function TJavaEnv.getAssetAsString(assets: jobject; name: string): string;
begin
  result := getAssetAsString(name,
                             commonMethods_AssetManager_Open_StringInputStream,
                             commonMethods_InputStream_Read_B);
end;

function TJavaEnv.getAssetAsString(assets: jobject; name: string; jmAssetManagerOpen, jmInputStreamRead: jmethodID): string;
var
  temp: jobject;
  stream: jobject;
  templ: TStringArray;
  i: Integer;
begin
  if pos('/../', name) > 0 then begin
    templ := strSplit(name, '/');
    i := high(templ);
    while i >= 0 do begin
      if templ[i] = '..'  then begin arrayDelete(templ, i); if (i > 0) then arrayDelete(templ, i-1);i-=2;end
      else i-=1;
    end;
    name := strJoin(templ, '/');
  end;

  temp := stringToJString(name);
  stream := callObjMethodChecked(assets, jmAssetManagerOpen, @temp);
  deleteLocalRef(temp);

  result := inputStreamToStringAndDelete(stream, jmInputStreamRead);
end;




function TJavaEnv.commonMethods_InputStream_Read_B: jmethodID;
begin
  result := getmethod('java/io/InputStream', 'read', '([B)I');
end;

function TJavaEnv.commonMethods_AssetManager_Open_StringInputStream: jmethodID;
begin
  result := getmethod('android/content/res/AssetManager', 'open', '(Ljava/lang/String;)Ljava/io/InputStream;');
end;


{$endif}

end.

