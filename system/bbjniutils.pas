unit bbjniutils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, jni;

type EAndroidInterfaceException = class(Exception);


type

{ TJavaEnv }

 TJavaEnv = record
  env: PJNIEnv;

  function getclass(n: pchar): jclass;
  function getmethod(c: jclass; n, sig: pchar): jmethodID;
  function getmethod(classname: pchar; n, sig: pchar): jmethodID;
  function getfield(c: jclass; n, sig: pchar): jfieldID;
  function getfield(classname: pchar; n, sig: pchar): jfieldID;
  function getstaticmethod(c: jclass; n, sig: pchar): jmethodID;
  function getstaticmethod(classname: pchar; n, sig: pchar): jmethodID;

  function getObjectField(obj: jobject; id: jfieldID): jobject;
  function getStringField(obj: jobject; id: jfieldID): string;
  function getIntField(obj: jobject; id: jfieldID): jint;
  function getLongField(obj: jobject; id: jfieldID): jlong;
  function getBooleanField(obj: jobject; id: jfieldID): boolean;

  procedure callVoidMethod(obj: jobject; methodID: jmethodID); inline;
  procedure callVoidMethod(obj: jobject; methodID: jmethodID; args: Pjvalue); inline;
  function callObjectMethod(obj: jobject;  methodID: jmethodID): jobject; inline;
  function callObjectMethod(obj: jobject;  methodID: jmethodID; args: Pjvalue): jobject; inline;
  function callBooleanMethod(obj: jobject;  methodID: jmethodID): boolean; inline;
  function callBooleanMethod(obj: jobject;  methodID: jmethodID; args: Pjvalue): boolean; inline;
  function callIntMethod(obj: jobject;  methodID: jmethodID): jint; inline;
  function callIntMethod(obj: jobject;  methodID: jmethodID; args: Pjvalue): jint; inline;
  function callStringMethod(obj: jobject;  methodID: jmethodID): string; inline;
  function callStringMethod(obj: jobject;  methodID: jmethodID; args: Pjvalue): string; inline;

  procedure callVoidMethodChecked(obj: jobject; methodID: jmethodID); inline;
  procedure callVoidMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue); inline;
  function callObjectMethodChecked(obj: jobject;  methodID: jmethodID): jobject; inline;
  function callObjectMethodChecked(obj: jobject;  methodID: jmethodID; args: Pjvalue): jobject; inline;
  function callBooleanMethodChecked(obj: jobject;  methodID: jmethodID): boolean; inline;
  function callBooleanMethodChecked(obj: jobject;  methodID: jmethodID; args: Pjvalue): boolean; inline;
  function callIntMethodChecked(obj: jobject;  methodID: jmethodID): jint; inline;
  function callIntMethodChecked(obj: jobject;  methodID: jmethodID; args: Pjvalue): jint; inline;
  function callStringMethodChecked(obj: jobject;  methodID: jmethodID): string; inline;
  function callStringMethodChecked(obj: jobject;  methodID: jmethodID; args: Pjvalue): string; inline;

  procedure callStaticVoidMethod(obj: jobject; methodID: jmethodID); inline;
  procedure callStaticVoidMethod(obj: jobject; methodID: jmethodID; args: Pjvalue); inline;
  function callStaticObjMethod(obj: jobject;  methodID: jmethodID): jobject; inline;
  function callStaticObjMethod(obj: jobject;  methodID: jmethodID; args: Pjvalue): jobject; inline;

  procedure SetObjectField(Obj:JObject;FieldID:JFieldID;Val:JObject); inline;
  procedure SetStringField(Obj:JObject;FieldID:JFieldID;Val:string); inline;
  procedure SetIntField(Obj:JObject;FieldID:JFieldID; i: jint); inline;
  procedure SetLongField(Obj:JObject;FieldID:JFieldID; i: jlong); inline;
  procedure SetBooleanField(Obj:JObject;FieldID:JFieldID; b: Boolean); inline;

  procedure setObjectArrayElement(a: jobject; index: integer; v: jobject); inline;
  function getObjectArrayElement(a: jobject; index: integer): jobject; inline;
  procedure setStringArrayElement(a: jobject; index: integer; v: string); inline;
  function getStringArrayElement(a: jobject; index: integer): string; inline;
  function getArrayLength(a: jobject): jint; inline;

  function newObject(c: jclass; m: jmethodID): jobject;
  function newObject(c: jclass; m: jmethodID; args: Pjvalue): jobject;
  function newObjectArray(len: integer; c: jclass; def: jobject): jobject;

  function newGlobalRefAndDelete(obj: jobject): jobject;
  procedure deleteLocalRef(obj: jobject); inline;
  procedure deleteGlobalRef(obj: jobject);

  function NewStringUTF8(s: string): jobject;
  function stringToJString(s: string): jobject; //deprecated
  function jStringToStringAndDelete(s: jobject): string;

  procedure RethrowJavaExceptionIfThereIsOne(aExceptionClass: ExceptClass);
  procedure RethrowJavaExceptionIfThereIsOne();

  procedure ThrowNew(c: jclass; error: string);
  procedure ThrowNew(c: pchar; error: string);

  function inputStreamToStringAndDelete(stream: jobject; jmInputStreamRead, jmInputStreamClose: jmethodID): string;
  function inputStreamToStringAndDelete(stream: jobject): string; //same as in androidinternetaccess

  function getMapProperty(map: jobject; value: jobject): jobject;

  {$ifdef android}
  function getAssets: jobject;
  function getAssetAsString(name: string): string;
  function getAssetAsString(name: string; jmAssetManagerOpen, jmInputStreamRead, jmInputStreamClose: jmethodID): string;
  function getAssetAsString(assets: jobject; name: string): string;
  function getAssetAsString(assets: jobject; name: string; jmAssetManagerOpen, jmInputStreamRead, jmInputStreamClose: jmethodID): string;

  {$endif}


  function commonClasses_String: jclass; //todo cache
  function commonClasses_InputStream: jclass; //todo cache
  function commonMethods_InputStream_Read_B(inputStream: jclass = nil): jmethodID; //todo cache
  function commonMethods_InputStream_Close(inputStream: jclass = nil): jmethodID; //todo cache
  function commonMethods_AssetManager_Open_StringInputStream: jmethodID; //todo cache
end;

const javaEnvRef: cardinal = $deadbeef; //block access to CustomDrawnInt.javaEnvRef because it is not thread safe
threadvar j: TJavaEnv; //this is an object to reduce the overhead caused by being a threadvar (i.e. you can write with j ...)

var jvmref: PJavaVM;
    jContextObject: jobject;

var onLoad: function: integer;

function needJ: TJavaEnv;


function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; cdecl;
procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;


implementation

uses bbutils {$IFDEF CD_Android}, customdrawnint{$endif};

function needJ: TJavaEnv;
var attachArgs: JavaVMAttachArgs;
begin
  {$IFDEF CD_Android}if jvmref = nil then jvmref:=javaVMRef;{$endif}
  //debugln(inttostr( ThreadID)+' needJ: '+strFromPtr(j.env));
  if j.env = nil then begin
    attachArgs.version:=JNI_VERSION_1_2;
    attachArgs.name:=nil;
    attachArgs.group:=nil;
    if jvmref^^.AttachCurrentThread(jvmref,@j.env,@attachArgs) <> 0 then
      raise EAndroidInterfaceException.create('Failed to get VM environment');
    //if jvmref^^.GetEnv(jvmref,@j.env,JNI_VERSION_1_4) <> 0 then
    //  raise EAndroidInterfaceException.create('Failed to get VM environment');
    if j.env = nil then
      raise EAndroidInterfaceException.create('Failed to get VM environment');

  end;
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

function TJavaEnv.getfield(c: jclass; n, sig: pchar): jfieldID;
begin
  result := j.env^^.GetFieldID(env, c, n, sig);
end;

function TJavaEnv.getfield(classname: pchar; n, sig: pchar): jfieldID;
begin
  result := j.env^^.GetFieldID(env, getclass(classname), n, sig);
end;

function TJavaEnv.getstaticmethod(c: jclass; n, sig: pchar): jmethodID;
begin
  result := env^^.GetStaticMethodID(env, c, n, sig);
  if (result = nil) or (env^^.ExceptionCheck(env)<>0) then
    raise EAndroidInterfaceException.Create('TAndroidInternetAccess: Failed to find method: '+string(n)+' '+string(sig));
end;

function TJavaEnv.getstaticmethod(classname: pchar; n, sig: pchar): jmethodID;
begin
  result := getstaticmethod(getclass(classname), n, sig);
end;

function TJavaEnv.getObjectField(obj: jobject; id: jfieldID): jobject;
begin
  result := env^^.GetObjectField(env, obj, id);
end;

function TJavaEnv.getStringField(obj: jobject; id: jfieldID): string;
begin
  result := jStringToStringAndDelete(getObjectField(obj, id));
end;

function TJavaEnv.getIntField(obj: jobject; id: jfieldID): jint;
begin
  result := env^^.GetIntField(env, obj, id);
end;

function TJavaEnv.getLongField(obj: jobject; id: jfieldID): jlong;
begin
  result := env^^.GetLongField(env, obj, id);
end;

function TJavaEnv.getBooleanField(obj: jobject; id: jfieldID): boolean;
begin
  result := env^^.GetBooleanField(env, obj, id) <> JNI_FALSE;
end;

procedure TJavaEnv.callVoidMethod(obj: jobject; methodID: jmethodID); inline;
begin
  env^^.CallVoidMethod(env, obj, methodID);
end;

procedure TJavaEnv.callVoidMethod(obj: jobject; methodID: jmethodID; args: Pjvalue);
begin
  env^^.CallVoidMethodA(env, obj, methodID, args);
end;

function TJavaEnv.callObjectMethod(obj: jobject; methodID: jmethodID): jobject; inline;
begin
  result := env^^.CallObjectMethod(env, obj, methodID);
end;

function TJavaEnv.callObjectMethod(obj: jobject; methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := env^^.CallObjectMethodA(env, obj, methodID, args);
end;

function TJavaEnv.callBooleanMethod(obj: jobject; methodID: jmethodID): boolean;
begin
  result := env^^.CallBooleanMethod(env, obj, methodID) <> JNI_FALSE;
end;

function TJavaEnv.callBooleanMethod(obj: jobject; methodID: jmethodID; args: Pjvalue): boolean;
begin
  result := env^^.CallBooleanMethodA(env, obj, methodID, args) <> JNI_FALSE;
end;

function TJavaEnv.callIntMethod(obj: jobject; methodID: jmethodID): jint;
begin
  result := env^^.CallIntMethod(env, obj, methodID);
end;

function TJavaEnv.callIntMethod(obj: jobject; methodID: jmethodID; args: Pjvalue): jint;
begin
  result := env^^.CallIntMethodA(env, obj, methodID, args);
end;

function TJavaEnv.callStringMethod(obj: jobject; methodID: jmethodID): string;
begin
  result := jStringToStringAndDelete(env^^.CallObjectMethod(env, obj, methodID));
end;

function TJavaEnv.callStringMethod(obj: jobject; methodID: jmethodID; args: Pjvalue): string;
begin
  result := jStringToStringAndDelete(env^^.CallObjectMethodA(env, obj, methodID, args));
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

function TJavaEnv.callObjectMethodChecked(obj: jobject; methodID: jmethodID): jobject;
begin
  result := callObjectMethod(obj, methodID);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callObjectMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := callObjectMethod(obj, methodID, args);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callBooleanMethodChecked(obj: jobject; methodID: jmethodID): boolean;
begin
  result := env^^.CallBooleanMethod(env, obj, methodID) <> JNI_FALSE;
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callBooleanMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue): boolean;
begin
  result := env^^.CallBooleanMethodA(env, obj, methodID, args) <> JNI_FALSE;
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callIntMethodChecked(obj: jobject; methodID: jmethodID): jint;
begin
  result := env^^.CallIntMethod(env, obj, methodID);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callIntMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue): jint;
begin
  result := env^^.CallIntMethodA(env, obj, methodID, args);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callStringMethodChecked(obj: jobject; methodID: jmethodID): string;
begin
  result := callStringMethod(obj, methodID);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callStringMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue): string;
begin
  result := callStringMethod(obj, methodID, args);
  RethrowJavaExceptionIfThereIsOne();
end;

procedure TJavaEnv.callStaticVoidMethod(obj: jobject; methodID: jmethodID);
begin
  env^^.CallStaticVoidMethod(env, obj, methodID);
end;

procedure TJavaEnv.callStaticVoidMethod(obj: jobject; methodID: jmethodID; args: Pjvalue);
begin
  env^^.CallStaticVoidMethodA(env, obj, methodID, args);
end;

function TJavaEnv.callStaticObjMethod(obj: jobject; methodID: jmethodID): jobject;
begin
  result := env^^.CallStaticObjectMethod(env, obj, methodID);
end;

function TJavaEnv.callStaticObjMethod(obj: jobject; methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := env^^.CallStaticObjectMethodA(env, obj, methodID, args);
end;

procedure TJavaEnv.SetObjectField(Obj: JObject; FieldID: JFieldID; Val: JObject);
begin
  j.env^^.SetObjectField(env, Obj, FieldID, val);
end;

procedure TJavaEnv.SetStringField(Obj: JObject; FieldID: JFieldID; Val: string);
var
  temp: jobject;
begin
  temp := stringToJString(val);
  SetObjectField(Obj, FieldID, temp);
  deleteLocalRef(temp);
end;

procedure TJavaEnv.SetIntField(Obj: JObject; FieldID: JFieldID; i: jint);
begin
  j.env^^.SetIntField(env, Obj, FieldID, i);
end;

procedure TJavaEnv.SetLongField(Obj: JObject; FieldID: JFieldID; i: jlong);
begin
  j.env^^.SetLongField(env, Obj, FieldID, i);
end;

procedure TJavaEnv.SetBooleanField(Obj: JObject; FieldID: JFieldID; b: Boolean);
begin
  if b then j.env^^.SetBooleanField(env, Obj, FieldID, JNI_TRUE)
  else j.env^^.SetBooleanField(env, Obj, FieldID, JNI_FALSE)
end;

procedure TJavaEnv.setObjectArrayElement(a: jobject; index: integer; v: jobject);
begin
  j.env^^.SetObjectArrayElement(env, a, index, v);
end;

function TJavaEnv.getObjectArrayElement(a: jobject; index: integer): jobject;
begin
  result := env^^.GetObjectArrayElement(env, a, index);
end;

procedure TJavaEnv.setStringArrayElement(a: jobject; index: integer; v: string);
var
  temp: jobject;
begin
  temp := stringToJString(v);
  j.env^^.SetObjectArrayElement(env, a, index, temp);
  deleteLocalRef(temp);
end;

function TJavaEnv.getStringArrayElement(a: jobject; index: integer): string;
begin
  result := jStringToStringAndDelete(getObjectArrayElement(a, index));
end;

function TJavaEnv.getArrayLength(a: jobject): jint;
begin
  result := env^^.GetArrayLength(env, a);
end;

function TJavaEnv.newObject(c: jclass; m: jmethodID): jobject;
begin
  result := env^^.NewObject(env, c, m);
end;

function TJavaEnv.newObject(c: jclass; m: jmethodID; args: Pjvalue): jobject;
begin
  result := env^^.NewObjectA(env, c, m, args);
end;

function TJavaEnv.newObjectArray(len: integer; c: jclass; def: jobject): jobject;
begin
  result := env^^.NewObjectArray(env,   len, c, def);
end;

function TJavaEnv.newGlobalRefAndDelete(obj: jobject): jobject;
begin
  result := env^^.NewGlobalRef(env, obj);
  deleteLocalRef(obj);
end;

procedure TJavaEnv.deleteLocalRef(obj: jobject);
begin
  env^^.DeleteLocalRef(env, obj);
end;

procedure TJavaEnv.deleteGlobalRef(obj: jobject);
begin
  env^^.DeleteGlobalRef(env, obj);
end;

function TJavaEnv.NewStringUTF8(s: string): jobject;
begin
  result := env^^.NewStringUTF(env, pchar(s));
end;

function TJavaEnv.stringToJString(s: string): jobject;
begin
  result := env^^.NewStringUTF(env, pchar(s));
end;

function TJavaEnv.jStringToStringAndDelete(s: jobject): string;
var chars: pchar;
begin
  if s = nil then exit('');
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

procedure TJavaEnv.ThrowNew(c: jclass; error: string);
begin
  env^^.ThrowNew(env, c, pchar(error));
end;

procedure TJavaEnv.ThrowNew(c: pchar; error: string);
begin
  ThrowNew(getclass(c), error);
end;


function TJavaEnv.inputStreamToStringAndDelete(stream: jobject; jmInputStreamRead, jmInputStreamClose: jmethodID): string;
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
    len := callIntMethodChecked(stream,  jmInputStreamRead, @wrappedBuffer);;
  end;
  callVoidMethodChecked(stream, jmInputStreamClose);

  DeleteLocalRef(wrappedBuffer.l);
  DeleteLocalRef(stream);
end;

function TJavaEnv.inputStreamToStringAndDelete(stream: jobject): string;
var
  streamClass: jclass;
begin
  streamClass := commonClasses_InputStream;
  result := inputStreamToStringAndDelete(stream, commonMethods_InputStream_Read_B(streamClass), commonMethods_InputStream_Close(streamClass));
  deleteLocalRef(streamClass);
end;

function TJavaEnv.getMapProperty(map: jobject; value: jobject): jobject;
begin
  result := callObjectMethod(map, getmethod('java/util/Map', 'get', '(Ljava/lang/Object;)Ljava/lang/Object;'), @value);
end;

{$ifdef android}
function TJavaEnv.getAssets: jobject;
begin
  result := callObjectMethodChecked(jContextObject, getmethod('android/content/Context', 'getAssets', '()Landroid/content/res/AssetManager;'));
end;

function TJavaEnv.getAssetAsString(name: string): string;
begin
  result := getAssetAsString(getAssets, name);
end;

function TJavaEnv.getAssetAsString(name: string; jmAssetManagerOpen, jmInputStreamRead, jmInputStreamClose: jmethodID): string;
begin
  result := getAssetAsString(getAssets, name, jmAssetManagerOpen, jmInputStreamRead, jmInputStreamClose);
end;

function TJavaEnv.getAssetAsString(assets: jobject; name: string): string;
var
  stream: jclass;
begin
  stream := commonClasses_InputStream;
  result := getAssetAsString(name,
                             commonMethods_AssetManager_Open_StringInputStream,
                             commonMethods_InputStream_Read_B(stream),
                             commonMethods_InputStream_Close(stream));
  deleteLocalRef(stream);
end;

function TJavaEnv.getAssetAsString(assets: jobject; name: string; jmAssetManagerOpen, jmInputStreamRead, jmInputStreamClose: jmethodID
  ): string;
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
  stream := callObjectMethodChecked(assets, jmAssetManagerOpen, @temp);
  deleteLocalRef(temp);

  result := inputStreamToStringAndDelete(stream, jmInputStreamRead, jmInputStreamClose);
end;

function TJavaEnv.commonClasses_String: jclass;
begin
  result := getclass('java/lang/String');
end;


function TJavaEnv.commonClasses_InputStream: jclass;
begin
  result := getclass('java/io/InputStream');
end;

function TJavaEnv.commonMethods_InputStream_Read_B(inputStream: jclass): jmethodID;
var
  localinputstream: jclass;
begin
  if inputStream = nil then localinputStream := commonClasses_InputStream
  else localinputstream := inputStream;
  result := getmethod(commonClasses_InputStream, 'read', '([B)I');
  if inputStream = nil then deleteLocalRef(localinputstream);
end;

function TJavaEnv.commonMethods_InputStream_Close(inputStream: jclass): jmethodID;
var
  localinputstream: jclass;
begin
  if inputStream = nil then localinputStream := commonClasses_InputStream
  else localinputstream := inputStream;
  result := getmethod(localinputstream, 'close', '()V');
  if inputStream = nil then deleteLocalRef(localinputstream);
end;

function TJavaEnv.commonMethods_AssetManager_Open_StringInputStream: jmethodID;
begin
  result := getmethod('android/content/res/AssetManager', 'open', '(Ljava/lang/String;)Ljava/io/InputStream;');
end;


{$endif}

end.

