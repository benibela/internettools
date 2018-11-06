unit bbjniutils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch typehelpers}
{$Macro on}

interface

uses
  Classes, SysUtils, jni, bbutils;

type EAndroidInterfaceException = class(Exception);


type

{ TJavaEnv }

 TStringConversionMode = (scmAssumeMUTF8, scmConvertValidUTF8ToMUTF8, scmConvertAndRepairUTF8ToMUTF8);
 TJavaEnv = record
  env: PJNIEnv;

  function getclass(n: pchar): jclass;
  function getmethod(c: jclass; n, sig: pchar): jmethodID;
  function getmethod(classname: pchar; n, sig: pchar): jmethodID;
  function getfield(c: jclass; n, sig: pchar): jfieldID;
  function getfield(classname: pchar; n, sig: pchar): jfieldID;
  function getstaticmethod(c: jclass; n, sig: pchar): jmethodID;
  function getstaticmethod(classname: pchar; n, sig: pchar): jmethodID;
  function getstaticfield(c: jclass; n, sig: pchar): jfieldID;
  function getstaticfield(classname: pchar; n, sig: pchar): jfieldID;

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
  function callStaticObjectMethod(obj: jobject;  methodID: jmethodID): jobject; inline;
  function callStaticObjectMethod(obj: jobject;  methodID: jmethodID; args: Pjvalue): jobject; inline;

  procedure callStaticVoidMethodChecked(obj: jobject; methodID: jmethodID); inline;
  procedure callStaticVoidMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue); inline;
  function callStaticObjectMethodChecked(obj: jobject;  methodID: jmethodID): jobject; inline;
  function callStaticObjectMethodChecked(obj: jobject;  methodID: jmethodID; args: Pjvalue): jobject; inline;

  procedure SetObjectField(Obj:JObject;FieldID:JFieldID;Val:JObject); inline;
  procedure SetObjectFieldAndDelete(Obj:JObject;FieldID:JFieldID;Val:JObject); inline;
  procedure SetStringField(Obj:JObject;FieldID:JFieldID;Val:string); inline;
  procedure SetIntField(Obj:JObject;FieldID:JFieldID; i: jint); inline;
  procedure SetLongField(Obj:JObject;FieldID:JFieldID; i: jlong); inline;
  procedure SetBooleanField(Obj:JObject;FieldID:JFieldID; b: Boolean); inline;

  procedure setObjectArrayElement(a: jobject; index: integer; v: jobject); inline;
  procedure setObjectArrayElementAndDelete(a: jobject; index: integer; v: jobject); inline;
  function getObjectArrayElement(a: jobject; index: integer): jobject; inline;
  procedure setStringArrayElement(a: jobject; index: integer; v: string); inline;
  function getStringArrayElement(a: jobject; index: integer): string; inline;
  function getIntArray(a: jobject): TLongintArray; inline;
  function getArrayLength(a: jobject): jint; inline;

  function newObject(c: jclass; m: jmethodID): jobject;
  function newObject(c: jclass; m: jmethodID; args: Pjvalue): jobject;
  function newObjectArray(len: integer; c: jclass; def: jobject = nil): jobject;
  function newStringArray(len: integer; def: jobject = nil): jobject;

  function newGlobalRefAndDelete(obj: jobject): jobject;
  procedure deleteLocalRef(obj: jobject); inline;
  procedure deleteGlobalRef(obj: jobject);

  function NewStringUTF(s: string): jobject; inline; //directly calls jni's NewStringUTF (which will fail/crash if the string contains #0 or characters > #$FFFF)
  function stringToJString(s: string; conversionMode: TStringConversionMode = scmConvertAndRepairUTF8ToMUTF8): jobject; //converts/repairs the string, so that it works with all (even invalid) UTF8 strings
  function jStringToString(s: jobject): string;
  function jStringToStringAndDelete(s: jobject): string;
  function booleanToJboolean(b: boolean): jboolean; inline;

  function arrayToJArray(a: array of string): jobject;

  procedure RethrowJavaExceptionIfThereIsOne(aExceptionClass: ExceptClass);
  procedure RethrowJavaExceptionIfThereIsOne();
  function ExceptionDescribeAndClear: string;
  function ExceptionCheck: boolean;

  procedure ThrowNew(c: jclass; error: string);
  procedure ThrowNew(c: pchar; error: string);

  function inputStreamToStringAndDelete(stream: jobject): string;
  private
  procedure inputStreamReadAllAndDelete(stream: jobject; readCallback: TStreamLikeWrite; jmInputStreamRead, jmInputStreamClose: jmethodID);
  public
  procedure inputStreamReadAllAndDelete(stream: jobject; readCallback: TStreamLikeWrite);

  function getMapProperty(map: jobject; value: jobject): jobject; deprecated 'I do not use this anymore? So I remove it?';

  {$ifdef android}
  function getAssets: jobject;
  function getAssetAsString(name: string): string;
  function getAssetAsString(assets: jobject; name: string): string;
  {$endif}
end;



TjobjectHelper = type helper for jobject
  procedure SetObjectField(FieldID:JFieldID;Val:JObject); inline;
  procedure SetStringField(FieldID:JFieldID;Val:string); inline;
  procedure SetIntField(FieldID:JFieldID; i: jint); inline;
  procedure SetLongField(FieldID:JFieldID; i: jlong); inline;
  procedure SetBooleanField(FieldID:JFieldID; b: Boolean); inline;

  procedure callVoidMethod(methodID: jmethodID); inline;
  procedure callVoidMethod(methodID: jmethodID; args: Pjvalue); inline;
  function callObjectMethod(methodID: jmethodID): jobject; inline;
  function callObjectMethod(methodID: jmethodID; args: Pjvalue): jobject; inline;
  function callBooleanMethod(methodID: jmethodID): boolean; inline;
  function callBooleanMethod(methodID: jmethodID; args: Pjvalue): boolean; inline;
  function callIntMethod(methodID: jmethodID): jint; inline;
  function callIntMethod(methodID: jmethodID; args: Pjvalue): jint; inline;
  function callStringMethod(methodID: jmethodID): string; inline;
  function callStringMethod(methodID: jmethodID; args: Pjvalue): string; inline;

  procedure callVoidMethodChecked(methodID: jmethodID); inline;
  procedure callVoidMethodChecked(methodID: jmethodID; args: Pjvalue); inline;
  function callObjectMethodChecked(methodID: jmethodID): jobject; inline;
  function callObjectMethodChecked(methodID: jmethodID; args: Pjvalue): jobject; inline;
  function callBooleanMethodChecked(methodID: jmethodID): boolean; inline;
  function callBooleanMethodChecked(methodID: jmethodID; args: Pjvalue): boolean; inline;
  function callIntMethodChecked(methodID: jmethodID): jint; inline;
  function callIntMethodChecked(methodID: jmethodID; args: Pjvalue): jint; inline;
  function callStringMethodChecked(methodID: jmethodID): string; inline;
  function callStringMethodChecked(methodID: jmethodID; args: Pjvalue): string; inline;

  procedure callStaticVoidMethodChecked(methodID: jmethodID); inline;
  procedure callStaticVoidMethodChecked(methodID: jmethodID; args: Pjvalue); inline;
  function callStaticObjectMethodChecked(methodID: jmethodID): jobject; inline;
  function callStaticObjectMethodChecked(methodID: jmethodID; args: Pjvalue): jobject; inline;

  procedure deleteLocalRef(); inline;
  procedure deleteGlobalRef(); inline;
  function newGlobalRefAndDelete(): jobject; inline;
{end;

TjclassHelper = type helper for jclass}
  function NewObject: jobject;
  function NewObject(m: jmethodID): jobject;
  function NewObject(m: jmethodID; args: Pjvalue): jobject;
  function getmethod(n, sig: pchar): jmethodID;
  function getstaticmethod(n, sig: pchar): jmethodID;
end;


const javaEnvRef: cardinal = $deadbeef; //block access to CustomDrawnInt.javaEnvRef because it is not thread safe
threadvar j: TJavaEnv; //this is an object to reduce the overhead caused by being a threadvar (i.e. you can write with j ...)

var jvmref: PJavaVM; //**< Java VM reference as passed to JNI_OnLoad.
    jContextObject: jobject;
    jCustomClassLoader: jobject; //**< alternative class loader to use rather than env.findclass
    jCustomClassLoaderFindClassMethod: jmethodID;

    jCommonClasses: record
      &String: record
        classRef: jclass;
      end;
      InputStream: record
        classRef: jclass;
        read_B, close: jmethodID;
      end;
      {$ifdef android}
      android: record
        Context: record
          classRef: jclass;
          getAssets: jmethodID;
        end;
        AssetManager: record
          classRef: jclass;
          open: jmethodID;
        end;
      end;
      {$endif}
    end;

var onLoad: function: integer;
var onUnload: procedure;

function needJ: TJavaEnv;


function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}


//do not use (they are here just for testing)
function isValidModifiedUTF8(const s: string; conversionMode: TStringConversionMode): integer;
function repairModifiedUTF8(const s: string): string; //valid utf is converted, invalid characters are removed

procedure setCustomClassLoaderFromLoadedClass(c: jclass);

implementation

uses math {$IFDEF CD_Android}, customdrawnint{$endif};

var commonClassesInitialized: boolean;
procedure initializeCommonClasses;
begin
  if commonClassesInitialized then exit;
  with jCommonClasses, j do begin
    &String.classRef := newGlobalRefAndDelete(getclass('java/lang/String'));
    with InputStream do begin
      classRef := newGlobalRefAndDelete(getclass('java/io/InputStream'));
      read_B := getmethod(classRef, 'read', '([B)I');
      close := getmethod(classRef, 'close', '()V');
    end;
    {$ifdef android}
    with android.Context do begin
      classRef := newGlobalRefAndDelete(getclass('android/content/Context'));
      getAssets := getmethod(classRef, 'getAssets', '()Landroid/content/res/AssetManager;');
    end;
    with android.AssetManager do begin
      classRef := newGlobalRefAndDelete(getclass('android/content/res/AssetManager'));
      open := getmethod(classRef, 'open', '(Ljava/lang/String;)Ljava/io/InputStream;');
    end;
    {$endif}
  end;
  commonClassesInitialized := true;
end;


function needJ: TJavaEnv;
var attachArgs: JavaVMAttachArgs;
begin
  if jvmref = nil then begin
    {$IFDEF CD_Android}jvmref:=javaVMRef;{$endif}
    if jvmref = nil then raise EAndroidInterfaceException.create('bbjniutils.jvmref is not set. It must be set to the value passed to JNI_OnLoad.');
  end;
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

    initializeCommonClasses; //this actually should not be here, since it should be globally called once, while needJ is called for every thread
  end;
  result := j;
end;

function JNI_OnLoad(vm: PJavaVM; reserved: pointer): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  jvmref := vm;
  if assigned(onLoad) then result := onload()
  else result := JNI_VERSION_1_4;
end;

procedure JNI_OnUnload(vm: PJavaVM; reserved: pointer); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  if assigned(onUnload) then onUnload();
end;


{$define TjclassHelper := TjobjectHelper}
function TjclassHelper.NewObject: jobject;
begin
  result := NewObject(j.getmethod(self, '<init>', '()V'));
end;

function TjclassHelper.NewObject(m: jmethodID): jobject;
begin
  result := j.newObject(self, m);
end;

function TjclassHelper.NewObject(m: jmethodID; args: Pjvalue): jobject;
begin
  result := j.newObject(self, m, args);
end;

function TjclassHelper.getmethod(n, sig: pchar): jmethodID;
begin
  result := j.getmethod(self, n, sig);
end;

function TjobjectHelper.getstaticmethod(n, sig: pchar): jmethodID;
begin
  result := j.getstaticmethod(self, n, sig);
end;


procedure TjobjectHelper.SetObjectField(FieldID: JFieldID; Val: JObject);
begin
  j.SetObjectField(self, FieldID, val);
end;

procedure TjobjectHelper.SetStringField(FieldID: JFieldID; Val: string);
begin
  j.SetStringField(self, FieldID, val);
end;

procedure TjobjectHelper.SetIntField(FieldID: JFieldID; i: jint);
begin
  j.SetIntField(self, FieldID, i);
end;

procedure TjobjectHelper.SetLongField(FieldID: JFieldID; i: jlong);
begin
  j.SetLongField(self, FieldID, i);
end;

procedure TjobjectHelper.SetBooleanField(FieldID: JFieldID; b: Boolean);
begin
  j.SetBooleanField(self, FieldID, b);
end;


procedure TjobjectHelper.callVoidMethod(methodID: jmethodID);
begin
  j.callVoidMethod(self, methodID);
end;

procedure TjobjectHelper.callVoidMethod(methodID: jmethodID; args: Pjvalue);
begin
  j.callVoidMethod(self, methodID, args);
end;

function TjobjectHelper.callObjectMethod(methodID: jmethodID): jobject;
begin
  result := j.callObjectMethod(self, methodID);
end;

function TjobjectHelper.callObjectMethod(methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := j.callObjectMethod(self, methodID, args);
end;

function TjobjectHelper.callBooleanMethod(methodID: jmethodID): boolean;
begin
  result := j.callBooleanMethod(self, methodID);
end;

function TjobjectHelper.callBooleanMethod(methodID: jmethodID; args: Pjvalue): boolean;
begin
  result := j.callBooleanMethod(self, methodID, args);
end;

function TjobjectHelper.callIntMethod(methodID: jmethodID): jint;
begin
  result := j.callIntMethod(self, methodID);
end;

function TjobjectHelper.callIntMethod(methodID: jmethodID; args: Pjvalue): jint;
begin
  result := j.callIntMethod(self, methodID, args);
end;

function TjobjectHelper.callStringMethod(methodID: jmethodID): string;
begin
  result := j.callStringMethod(self, methodID);
end;

function TjobjectHelper.callStringMethod(methodID: jmethodID; args: Pjvalue): string;
begin
  result := j.callStringMethod(self, methodID, args);
end;



procedure TjobjectHelper.callVoidMethodChecked(methodID: jmethodID);
begin
  j.callVoidMethodChecked(self, methodID);
end;

procedure TjobjectHelper.callVoidMethodChecked(methodID: jmethodID; args: Pjvalue);
begin
  j.callVoidMethodChecked(self, methodID, args);
end;

function TjobjectHelper.callObjectMethodChecked(methodID: jmethodID): jobject;
begin
  result := j.callObjectMethodChecked(self, methodID);
end;

function TjobjectHelper.callObjectMethodChecked(methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := j.callObjectMethodChecked(self, methodID, args);
end;

function TjobjectHelper.callBooleanMethodChecked(methodID: jmethodID): boolean;
begin
  result := j.callBooleanMethodChecked(self, methodID);
end;

function TjobjectHelper.callBooleanMethodChecked(methodID: jmethodID; args: Pjvalue): boolean;
begin
  result := j.callBooleanMethodChecked(self, methodID, args);
end;

function TjobjectHelper.callIntMethodChecked(methodID: jmethodID): jint;
begin
  result := j.callIntMethodChecked(self, methodID);
end;

function TjobjectHelper.callIntMethodChecked(methodID: jmethodID; args: Pjvalue): jint;
begin
  result := j.callIntMethodChecked(self, methodID, args);
end;

function TjobjectHelper.callStringMethodChecked(methodID: jmethodID): string;
begin
  result := j.callStringMethodChecked(self, methodID);
end;

function TjobjectHelper.callStringMethodChecked(methodID: jmethodID; args: Pjvalue): string;
begin
  result := j.callStringMethodChecked(self, methodID, args);
end;

procedure TjobjectHelper.callStaticVoidMethodChecked(methodID: jmethodID);
begin
  j.callStaticVoidMethodChecked(self, methodID);
end;

procedure TjobjectHelper.callStaticVoidMethodChecked(methodID: jmethodID; args: Pjvalue);
begin
  j.callStaticVoidMethodChecked(self, methodID, args);
end;

function TjobjectHelper.callStaticObjectMethodChecked(methodID: jmethodID): jobject;
begin
  result := j.callStaticObjectMethodChecked(self, methodID);
end;

function TjobjectHelper.callStaticObjectMethodChecked(methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := j.callStaticObjectMethodChecked(self, methodID, args);
end;

procedure TjobjectHelper.deleteLocalRef();
begin
  j.deleteLocalRef(self);
end;

procedure TjobjectHelper.deleteGlobalRef();
begin
  j.deleteGlobalRef(self);
end;

function TjobjectHelper.newGlobalRefAndDelete(): jobject;
begin
  result := j.newGlobalRefAndDelete(self);
end;



function TJavaEnv.getclass(n: pchar): jclass;
var
  jn: jobject;
begin
  if jCustomClassLoader <> nil then begin
    jn := stringToJString(n);
    result := callObjectMethod(jCustomClassLoader, jCustomClassLoaderFindClassMethod, @jn);
    if ExceptionCheck then begin
      env^^.ExceptionClear(env);
      result := nil;
    end;
    deleteLocalRef(jn);
    if result <> nil then exit;
  end;
  result := env^^.FindClass(env, n);
  RethrowJavaExceptionIfThereIsOne();
end;
function TJavaEnv.getmethod(c: jclass; n, sig: pchar): jmethodID;
begin
  result := env^^.GetMethodID(env, c, n, sig);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.getmethod(classname: pchar; n, sig: pchar): jmethodID;
var
  c: jclass;
begin
  c := getclass(classname);
  result := getmethod(c, n, sig);
  deleteLocalRef(c);
end;

function TJavaEnv.getfield(c: jclass; n, sig: pchar): jfieldID;
begin
  result := env^^.GetFieldID(env, c, n, sig);
end;

function TJavaEnv.getfield(classname: pchar; n, sig: pchar): jfieldID;
var
  c: jclass;
begin
  c := getclass(classname);
  result := env^^.GetFieldID(env, c, n, sig);
  deleteLocalRef(c);
end;

function TJavaEnv.getstaticmethod(c: jclass; n, sig: pchar): jmethodID;
begin
  result := env^^.GetStaticMethodID(env, c, n, sig);
  if (result = nil) or (env^^.ExceptionCheck(env)<>0) then
    raise EAndroidInterfaceException.Create('TAndroidInternetAccess: Failed to find method: '+string(n)+' '+string(sig));
end;

function TJavaEnv.getstaticmethod(classname: pchar; n, sig: pchar): jmethodID;
var
  c: jclass;
begin
  c := getclass(classname);
  result := getstaticmethod(c, n, sig);
  deleteLocalRef(c);
end;

function TJavaEnv.getstaticfield(c: jclass; n, sig: pchar): jfieldID;
begin
  result := env^^.GetStaticFieldID(env, c, n, sig);
end;

function TJavaEnv.getstaticfield(classname: pchar; n, sig: pchar): jfieldID;
var
  c: jclass;
begin
  c := getclass(classname);
  result := getstaticfield(c, n, sig);
  deleteLocalRef(c);
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

function TJavaEnv.callStaticObjectMethod(obj: jobject; methodID: jmethodID): jobject;
begin
  result := env^^.CallStaticObjectMethod(env, obj, methodID);
end;

function TJavaEnv.callStaticObjectMethod(obj: jobject; methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := env^^.CallStaticObjectMethodA(env, obj, methodID, args);
end;

procedure TJavaEnv.callStaticVoidMethodChecked(obj: jobject; methodID: jmethodID);
begin
  callStaticVoidMethod(obj, methodID);
  RethrowJavaExceptionIfThereIsOne();
end;

procedure TJavaEnv.callStaticVoidMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue);
begin
  callStaticVoidMethod(obj, methodID, args);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callStaticObjectMethodChecked(obj: jobject; methodID: jmethodID): jobject;
begin
  result := callStaticObjectMethod(obj, methodID);
  RethrowJavaExceptionIfThereIsOne();
end;

function TJavaEnv.callStaticObjectMethodChecked(obj: jobject; methodID: jmethodID; args: Pjvalue): jobject;
begin
  result := callStaticObjectMethod(obj, methodID, args);
  RethrowJavaExceptionIfThereIsOne();
end;

procedure TJavaEnv.SetObjectField(Obj: JObject; FieldID: JFieldID; Val: JObject);
begin
  env^^.SetObjectField(env, Obj, FieldID, val);
end;

procedure TJavaEnv.SetObjectFieldAndDelete(Obj: JObject; FieldID: JFieldID; Val: JObject);
begin
  SetObjectField(obj, FieldID, val);
  deleteLocalRef(val);
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
  env^^.SetIntField(env, Obj, FieldID, i);
end;

procedure TJavaEnv.SetLongField(Obj: JObject; FieldID: JFieldID; i: jlong);
begin
  env^^.SetLongField(env, Obj, FieldID, i);
end;

procedure TJavaEnv.SetBooleanField(Obj: JObject; FieldID: JFieldID; b: Boolean);
begin
  env^^.SetBooleanField(env, Obj, FieldID, booleanToJboolean(b))
end;

procedure TJavaEnv.setObjectArrayElement(a: jobject; index: integer; v: jobject);
begin
  env^^.SetObjectArrayElement(env, a, index, v);
end;

procedure TJavaEnv.setObjectArrayElementAndDelete(a: jobject; index: integer; v: jobject);
begin
  setObjectArrayElement(a, index, v);
  deleteLocalRef(v);
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
  env^^.SetObjectArrayElement(env, a, index, temp);
  deleteLocalRef(temp);
end;

function TJavaEnv.getStringArrayElement(a: jobject; index: integer): string;
begin
  result := jStringToStringAndDelete(getObjectArrayElement(a, index));
end;

function TJavaEnv.getIntArray(a: jobject): TLongintArray;
begin
  SetLength(result, getArrayLength(a));
  if length(result) = 0 then exit;
  env^^.GetIntArrayRegion(env, a, 0, length(result), @result[0]);;
end;

function TJavaEnv.getArrayLength(a: jobject): jint;
begin
  if a = nil then exit(0);
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

function TJavaEnv.newStringArray(len: integer; def: jobject): jobject;
begin
  result := env^^.NewObjectArray(env, len, jCommonClasses.&String.classRef, def);
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

function TJavaEnv.NewStringUTF(s: string): jobject; inline;
begin
  result := env^^.NewStringUTF(env, pchar(s));
end;

const INVALID_UTF8 = 1;
const INVALID_MUTF8 = 2;
function isContinuationByte(c: char): boolean; inline;
begin
  result := (ord(c) and $C0 = $80);
end;
function isValidModifiedUTF8(const s: string; conversionMode: TStringConversionMode): integer;
var i: integer;
begin
  //if conversionMode is scmConvertAndRepairUTF8ToMUTF8 then we check if it is valid modified utf-8 (and valid utf-8)
  //if conversionMode is scmConvertValidUTF8ToMUTF8 then we check if it is valid utf-8 (do not abort after discovering it is invalid modified utf-8)
  result := 0;
  i := 1;
  while i <= length(s) do begin
    case s[i] of
    #$00: begin
      result := INVALID_MUTF8;
      if conversionMode = scmConvertAndRepairUTF8ToMUTF8 then exit;
    end;
    #$01..#$7f: ; //ok
    //C0, C1 is invalid ascii reencoding
    #$C0: begin
      inc(i);
      if s[i] <> #$80 then exit(INVALID_UTF8); //but valid modified UTF-8
    end;
    #$C2..#$DF: begin
      inc(i);
      if (length(s) < i) or not isContinuationByte(s[i]) then exit(INVALID_UTF8);
    end;
    #$E0..#$EF: begin
      if length(s) < i + 2 then exit(INVALID_UTF8);
      case s[i] of
        #$E0: if not (s[i+1] in [#$A0..#$BF]) then exit(INVALID_UTF8);
        //#$ED: if not (s[i+1] in [#$80..#$9F]) then exit(INVALID_UTF8); UTF-16 surrogate pair. Invalid UTF8, but valid MUTF8
        else if not isContinuationByte(s[i+1]) then exit(INVALID_UTF8);
      end;
      inc(i, 2);
      if not isContinuationByte(s[i]) then exit(INVALID_UTF8);
    end;
    #$F0..#$F4: begin
      if length(s) < i + 3 then exit(INVALID_UTF8);
      case s[i] of
        #$F0: if not (s[i+1] in [#$90..#$BF]) then exit(INVALID_UTF8);
        #$F4: if not (s[i+1] in [#$80..#$8F]) then exit(INVALID_UTF8);
        else if not isContinuationByte(s[i+1]) then exit(INVALID_UTF8);
      end;
      if not isContinuationByte(s[i+2]) or not isContinuationByte(s[i+3]) then exit(INVALID_UTF8);
      inc(i, 3);
      result := INVALID_MUTF8; //no 4-byte sequences allowed
      if conversionMode = scmConvertAndRepairUTF8ToMUTF8 then exit;
    end else exit(INVALID_UTF8);
    end;
    inc(i);
  end;
end;
{1000 - 8
1100 - C
1110 - E
1111 - F }
function repairModifiedUTF8(const s: string): string; //valid utf is converted, invalid characters are removed
const ERRMARKER = '?';
var res: string;
    p: SizeInt;
  procedure needLength(d: integer); inline;
  begin
    if p + d - 1 > length(res) then
      setlength(res, max(p + d - 1, length(res) + length(res) div 4));
  end;
  procedure pushUnicodeChar(c: integer);
  var lead, trail: integer;
    len: Integer;
  begin
    case c of
      0: begin
        //special encoding of 0
        needLength(2);
        res[p] := #$C0;
        res[p+1] := #$80;
        inc(p, 2);
      end;
      $1..$FFFF: begin
        //normal utf-8
        len := strGetUnicodeCharacterUTFLength(c);
        needLength(len);
        strGetUnicodeCharacterUTF(c, @res[p]);
        inc(p, len);
      end;
      else begin
        //convert to utf-16
        c := c - $010000;
        lead := $D800 + ((c shr 10) and $3FF);   //probably do not need the ands here
        trail := $DC00 + (c and $3FF);
        //convert both utf-16 surrogate pairs to utf-8
        needLength(6);
        //lead in 0xD800..0xDBFF = 11011xxx yyzzzzzz => utf-8 11101101 101xxxyy 10zzzzzz
        res[p] := #$ED;
        res[p+1] := chr($A0 or ((lead shr 6) and $3F));
        res[p+2] := chr($80 or (lead and $3F));

        //trail in 0xDC00..0xDFFF = 110111xx yyzzzzzz
        res[p+3] := #$ED;
        res[p+4] := chr($B0 or ((trail shr $6) and $3F));
        res[p+5] := chr($80 or (trail and $3F));

        inc(p, 6);
      end;
    end;
  end;
  procedure pushError;
  begin
    needLength(1);
    res[p] := ERRMARKER;
    inc(p);
  end;

var i: SizeInt;
  procedure reencode; inline;
  begin
    pushUnicodeChar(strDecodeUTF8Character(s, i));
  end;


begin
  setlength(res, length(s));
  p := 1;
  i := 1;
  while i <= length(s) do
    case s[i] of
    #$00: begin
      pushUnicodeChar(0);
      inc(i);
    end;
    #$01..#$7f: begin
      needLength(1);
      res[p] := s[i];
      inc(p); inc(i);
    end;
    #$C0..#$C1: begin
      if (i + 1 <= length(s)) and isContinuationByte(s[i+1]) then
        pushUnicodeChar(((ord(s[i]) and not $C0) shl 6) or (ord(s[i+1]) and not $80))
      else
        pushError;
      inc(i,2);
    end;
    #$C2..#$DF:
      if (i + 1 <= length(s)) and isContinuationByte(s[i+1]) then begin
        needLength(2);
        res[p] := s[i]; res[p+1] := s[i+1];
        inc(p, 2);
        inc(i, 2);
      end else begin
        pushError;
        inc(i, 2);
      end;
    #$E0..#$EF: begin
      if (length(s) < i + 2) or not isContinuationByte(s[i+1]) or not isContinuationByte(s[i + 2]) then begin
        pushError;
        inc(i, 3);
      end else if (s[i] = #$E0) and not (s[i+1] in [#$A0..#$BF]) then
        reencode
      else begin
        needLength(3);
        res[p] := s[i]; res[p+1] := s[i+1]; res[p+2] := s[i+2];
        inc(p, 3);
        inc(i, 3);
      end;
    end;
    #$F0..#$F4: begin
      if (length(s) < i + 3)
         or not isContinuationByte(s[i + 1]) or not isContinuationByte(s[i + 2]) or not isContinuationByte(s[i + 3])
         or ((s[i] = #$F4) and not (s[i+1] in [#$80..#$8F])) then begin
        pushError;
        inc(i, 4);
      end //else if (s[i] = #$F0) and not (s[i+1] in [#$90..#$BF]) then reencode //not needed
      //else if (s[i] = #$F4) and not (s[i+1] in [#$80..#$8F]) then error //above
      else reencode;
    end;
    #$F5..#$F7: begin
      pushError;
      inc(i, 4);
    end;
    {#$F8..#$FB: begin
      pushError;
      inc(i, 5);
    end;
    #$FC..#$FD: begin
      pushError;
      inc(i, 6);
    end;}
    else begin
      pushError;
      inc(i);
    end;

  end;
  if length(res) <> p - 1 then
    setlength(res, p - 1);
  result := res;
end;

function TJavaEnv.stringToJString(s: string; conversionMode: TStringConversionMode = scmConvertAndRepairUTF8ToMUTF8): jobject;
var ok: integer;
begin
  if (conversionMode = scmAssumeMUTF8) then
    exit(NewStringUTF(s));
  ok := isValidModifiedUTF8(s, conversionMode);
  if ok = 0 then
    exit(NewStringUTF(s));
  if (ok = INVALID_UTF8) and (conversionMode = scmConvertValidUTF8ToMUTF8) then
    raise EAndroidInterfaceException.create('String is invalid utf-8: '+strFromPtr(pointer(s))+':'+inttostr(length(s)));
  exit(NewStringUTF(repairModifiedUTF8(s)));
  //nothing works to catch it RethrowJavaExceptionIfThereIsOne; if result = nil then raise EAndroidInterfaceException.create('Failed to create string from '+strFromPtr(pointer(s))+':'+inttostr(length(s)));
end;

function TJavaEnv.jStringToString(s: jobject): string;
var chars: pchar;
begin
  if s = nil then exit('');
  //setlength(result, env^^.GetStringUTFLength(env, s));
  chars := env^^.GetStringUTFChars(env, s, nil);
  if chars = nil then result := ''
  else result := chars;
  env^^.ReleaseStringUTFChars(env, s, chars);
end;

function TJavaEnv.jStringToStringAndDelete(s: jobject): string;
begin
  if s = nil then exit('');
  result := jStringToString(s);
  env^^.DeleteLocalRef(env, s);
end;

function TJavaEnv.booleanToJboolean(b: boolean): jboolean;
begin
  if b then result := JNI_TRUE
  else result := JNI_FALSE;
end;

function TJavaEnv.arrayToJArray(a: array of string): jobject;
var
  i: Integer;
begin
  result := newObjectArray(length(a), jCommonClasses.&String.classRef, nil);
  for i := 0 to high(a) do
    setStringArrayElement(result, i, a[i]);
end;


procedure TJavaEnv.RethrowJavaExceptionIfThereIsOne(aExceptionClass: ExceptClass);
begin
  if ExceptionCheck then
    raise aexceptionClass.create(ExceptionDescribeAndClear);

end;

procedure TJavaEnv.RethrowJavaExceptionIfThereIsOne();
begin
  RethrowJavaExceptionIfThereIsOne(EAndroidInterfaceException);
end;

function TJavaEnv.ExceptionDescribeAndClear: string;
var je: jthrowable;
    temp: jobject;
begin
  je := env^^.ExceptionOccurred(env);
  if je = nil then exit('');
  env^^.ExceptionDescribe(env);
  env^^.ExceptionClear(env); //carefully, we need to clear before calling anything else
  temp:= env^^.CallObjectMethod(env, je, getmethod('java/lang/Object', 'getClass', '()Ljava/lang/Class;'));
  result := jStringToStringAndDelete(env^^.CallObjectMethod(env, temp, getmethod('java/lang/Class', 'getName', '()Ljava/lang/String;'))) + ': '
          + jStringToStringAndDelete(env^^.CallObjectMethod(env, je, getmethod('java/lang/Throwable', 'getMessage', '()Ljava/lang/String;')));
  env^^.DeleteLocalRef(env, temp);
  env^^.DeleteLocalRef(env, je);
end;

function TJavaEnv.ExceptionCheck: boolean;
begin
  result := env^^.ExceptionCheck(env) <> JNI_FALSE
end;

procedure TJavaEnv.ThrowNew(c: jclass; error: string);
begin
  env^^.ThrowNew(env, c, pchar(error));
end;

procedure TJavaEnv.ThrowNew(c: pchar; error: string);
begin
  ThrowNew(getclass(c), error);
end;

function TJavaEnv.inputStreamToStringAndDelete(stream: jobject): string;
var builder: TStrBuilder;
begin
  builder.init(@result);
  self.inputStreamReadAllAndDelete(stream, @builder.appendBuffer, jCommonClasses.InputStream.read_B, jCommonClasses.InputStream.close);
  builder.final;
end;

procedure TJavaEnv.inputStreamReadAllAndDelete(stream: jobject; readCallback: TStreamLikeWrite; jmInputStreamRead,
  jmInputStreamClose: jmethodID);
const BUFFERLEN = 8192;
var wrappedBuffer: jvalue;
    temp: JBoolean;
    len: jint;
    buffer: pointer;
begin
  if stream = nil then raise EAndroidInterfaceException.create('No stream');
  temp := 0;

  wrappedBuffer.l := env^^.NewByteArray(env, BUFFERLEN);
  len := env^^.CallIntMethodA(env, stream,  jmInputStreamRead, @wrappedBuffer);;
  RethrowJavaExceptionIfThereIsOne;
  while len >= 0 do begin
    if len > 0 then begin
      buffer := env^^.GetPrimitiveArrayCritical(env, wrappedBuffer.l, temp);
      if buffer <> nil then
        readCallback(buffer^, len);
      env^^.ReleasePrimitiveArrayCritical(env, wrappedBuffer.l, buffer, JNI_ABORT);
    end;
    len := callIntMethodChecked(stream,  jmInputStreamRead, @wrappedBuffer);;
  end;
  callVoidMethodChecked(stream, jmInputStreamClose);

  DeleteLocalRef(wrappedBuffer.l);
  DeleteLocalRef(stream);
end;

procedure TJavaEnv.inputStreamReadAllAndDelete(stream: jobject; readCallback: TStreamLikeWrite);
begin
  inputStreamReadAllAndDelete(stream, readCallback, jCommonClasses.InputStream.read_B, jCommonClasses.InputStream.close);
end;

function TJavaEnv.getMapProperty(map: jobject; value: jobject): jobject;
begin
  result := callObjectMethod(map, getmethod('java/util/Map', 'get', '(Ljava/lang/Object;)Ljava/lang/Object;'), @value);
end;

{$ifdef android}
function TJavaEnv.getAssets: jobject;
begin
  result := callObjectMethodChecked(jContextObject, jCommonClasses.android.Context.getAssets);
end;

function TJavaEnv.getAssetAsString(name: string): string;
var assets: jobject;
begin
  assets := getAssets;
  result := getAssetAsString(assets, name);
  deleteLocalRef(assets);
end;

function TJavaEnv.getAssetAsString(assets: jobject; name: string): string;
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
  stream := callObjectMethodChecked(assets, jCommonClasses.android.AssetManager.open, @temp);
  deleteLocalRef(temp);

  result := inputStreamToStringAndDelete(stream);
end;


{$endif}


procedure setCustomClassLoaderFromLoadedClass(c: jclass);
var classClass, classLoaderClass: jclass;
  getClassLoaderMethod: jmethodID;
begin
  //see https://stackoverflow.com/questions/13263340/findclass-from-any-thread-in-android-jni
  with needJ do begin
    classClass := env^^.GetObjectClass(env, c);
    getClassLoaderMethod := getmethod(classClass, 'getClassLoader', '()Ljava/lang/ClassLoader;');
    jCustomClassLoader := newGlobalRefAndDelete(callObjectMethodChecked(c, getClassLoaderMethod));

    classLoaderClass := env^^.FindClass(env, 'java/lang/ClassLoader');
    jCustomClassLoaderFindClassMethod := getmethod(classLoaderClass, 'findClass', '(Ljava/lang/String;)Ljava/lang/Class;');
  end;
end;


end.

