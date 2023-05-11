{**
  Logging and benchmarking functions for debugging

  Not supposed to be used in production
}
unit bbdebugtools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 


  procedure log(const s: string);

  procedure startTiming(const title:string='global');
  procedure stopTiming(const title:string = '');

  procedure stoplogging();

type TStringNotifyEvent = procedure(message: string) of object;

var
  logging: boolean=false;
  logFileCreated: boolean=false;
  logFile: TextFile;
  logFileSection: TRTLCriticalSection;
  logFileName: string;
  
  timing: TStringList=nil; //wird zur Speicherplatzoptimierung nicht freigegeben
                           //(bzw. erst nach Programmende von Windows)

  OnLog: TStringNotifyEvent = nil;

{$ifdef android}
function __android_log_write(prio:longint;tag,text:pchar):longint; cdecl; external 'liblog.so' name '__android_log_write';
{$endif}

implementation
uses bbutils;

function dateTimeFormatNow(const mask: string): string;
var st: TSystemTime;

begin
  st := default(TSystemTime);
  GetLocalTime(st);
  result := dateTimeFormat(mask, st.Year, st.month, st.Day, st.Hour, st.Minute, st.Second, st.MilliSecond * 1000000);
end;

  procedure log(const s: string);
  var t:string;
    {$ifndef android}
    sl: TStringArray;
    i: Integer;
    {$endif}
  begin
    if logging then begin
      t:=dateTimeFormatNow('yyyy-mm-dd:hh:nn:ss:zzz')+' ('+inttostr(GetThreadID)+'):  '+s;
      if not logFileCreated then begin
        system.InitCriticalSection(logFileSection);
        system.EnterCriticalSection(logFileSection);
        logFileCreated:=true;
        {$ifndef android}
        logFileName := fileNameExpand(GetTempDir+'videLibriLogFile_'+dateTimeFormatNow('yyyymmdd_hhnnss'));
        AssignFile(logFile, logFileName);
        Rewrite(logFile);
        WriteLn(logFile,'Logging gestartet am '+dateTimeFormatNow('yyyy-mm-dd "um" hh:nn:ss'));
        {$endif}
      end else system.EnterCriticalSection(logFileSection);
      {$ifndef android}
      if length(t) <= 255 then WriteLn(logFile,t)
      else begin
        sl := strSplit(t, LineEnding);
        for i:=0 to high(sl) do writeln(logFile, sl[i]);
      end;
      Flush(logFile);
      {$IFNDEF WIN32}
      if length(t) <= 255 then WriteLn(stderr,t)
      else begin
        for i:=0 to high(sl) do writeln(stderr, sl[i]);
      end;
      {$ENDIF}
      {$else}
      __android_log_write({INFO}4, 'VideLibri', pchar(t));
      {$endif}
      system.LeaveCriticalSection(logFileSection);
      if assigned(OnLog) then OnLog(t);
    end;
  end;

  procedure startTiming(const title: string);
  var index:longint;
  begin
    logging := true;
{    if ThreadID<>MainThreadID then
      exit; //timing isn't thread save}
    if timing=nil then timing:=TStringList.Create;
    index:=timing.IndexOf(title);
    if index=-1 then begin
      index:=timing.count;
      timing.add(title);
    end;
    log('started timing of '+title);
    timing.Objects[index]:=tobject(pointer(trunc(frac(now)*MSecsPerDay)));
  end;

  procedure stopTiming(const title: string);
  var time,oldtime:cardinal;
    i: Integer;
  begin
{    if ThreadID<>MainThreadID then
      exit; //timing isn't thread save }
    time:=trunc(frac(now)*MSecsPerDay);
    if (timing.count = 1) and (title = '') then i := 0
    else i := timing.IndexOf(title);
    oldtime:=cardinal(pointer(timing.Objects[i]));
    if (timing.count > 1) or (title <> '') then
      log('stopped timing of '+title+' run-time: '+IntToStr(time-oldtime)+' ms')
     else
      log('run-time: '+IntToStr(time-oldtime)+' ms');
    timing.Delete(i);
  end;


  procedure stoplogging();
  begin
    if not logFileCreated then exit;
    {$ifdef android}exit;{$endif}
    system.EnterCriticalSection(logFileSection);
    CloseFile(logFile);
    logFileCreated:=false;
    system.LeaveCriticalsection(logFileSection);
    system.DoneCriticalsection(logFileSection);
  end;

end.

