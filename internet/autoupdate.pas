{Copyright (C) 2006  Benito van der Zander

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
{
How to use it:
  1. create a new object with this options:
       version:
         Integer based incrementing version number
         for example 1, next version, 2.
         You cann't use 1.6, or 1.56, and 156>16
       installDir:
         Directory to the program files to be updated
       versionURL:
         URL containing a file containing only one number, the
         currently newest version of the program. (don't even start a
         new line at the end)
       versionDetailsURL:
         URL containing an ini file with details to the newest version.
         In the current version of TAutoUpdater this file contains only
         a section [version-number] with a string package-url containing 
         the ZIP-archive to download. (there can be more, but it has no use)
  2. call existsUpdate to compare the current version number with the one 
     in the web
     YOU NEED ACCESS TO VERSIONURL (= INTERNET).
  3. call hasDirectoryWriteAccess to check, if you are allowed to change the
     files in the program directory.
     If not, you have to restart the whole process with changed rights (->admin)
  4. call downloadUpdate(tempDir) to download the file to tempDir.
     tempDir can be omitted.
  5. call installUpdate to extract the zip archive and copy it to the program path
     YOU NEED ACCESS TO THE PROGRAMPATH
  6. call needRestart to check if the program must be restarted.
     Changes to the executable itself will only be installed after restart. 
     (using a batch file)

  benito@benibela.de
}

unit autoupdate;

{$mode objfpc}{$H+}

interface
{$DEFINE showProgress}
uses
  Classes, SysUtils,internetaccess,bbutils,simplexmlparser,  {$IFDEF showProgress}progressDialog{$ENDIF}
  ;
type

{ TAutoUpdater }

TVersionNumber=longint;
TAutoUpdater=class
private
  finternet:TInternetAccess;
  //fUpdateStyle:(usInstaller);
  fcurrentVersion:TVersionNumber;
  fnewversionstr:string;
  finstallDir,fversionsURL,fchangelogURL: string;
  ftempDir:string;



  fnewversion:TVersionNumber;
  function loadNewVersionEnterTag(tagName: string; properties: TProperties):boolean;
  procedure loadNewVersion(const url: string);

  finstallerurl,fallChanges,fallFixes,fallAdds:string;
  fbuildinfolastbuild: longint;
  fbuildinfolasttag:string;
  fupdatebuildversion:longint;
  function needBuildInfoEnterTag(tagName: string; properties: TProperties):boolean;
  function needBuildInfoTextRead(text: string):boolean;
  procedure needBuildInfo();//newversion,build

  procedure needInternet;inline;
  (** can every old file be replaced
     Result:
       true: yes, call installUpdate
       false: no, call installUpdate and RESTART program (you must restart)
  *)
  function _needRestart:boolean;
public
  constructor create(currentVersion:TVersionNumber;installDir,versionsURL,changelogURL: string);
  function hasDirectoryWriteAccess:boolean;
  function existsUpdate:boolean;
  (** downloads the update to $tempDir\$newVersion
  *)
  function listChanges:string;
  procedure downloadUpdate(tempDir:string='');
  (** overwrites files and call additional rewrite process *)
  procedure installUpdate;
  
  destructor destroy;override;
  
  property newestVersion: TVersionNumber read fnewversion;
  property needRestart: boolean read _needRestart;
end;
const homepageAlternative='www.benibela.de';
implementation

{ TAutoUpdater }

function TAutoUpdater.hasDirectoryWriteAccess: boolean;
var f:THandle;
begin
  {$IFDEF WIN32}
  if Win32Platform=VER_PLATFORM_WIN32_WINDOWS then result:=true
  else if Win32Platform=VER_PLATFORM_WIN32_NT then begin
    f:=CreateFile(pchar(copy(finstallDir,1,length(finstallDir)-1) ),GENERIC_WRITE, FILE_SHARE_WRITE or FILE_SHARE_READ, nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS,0);
    if f=INVALID_HANDLE_VALUE then result:=false
    else begin
      result:=true;
      closehandle(f);
    end;
  end else result:=false;
  {$ELSE}
  result:=true;//TODO
  {$ENDIF}
end;


function TAutoUpdater.loadNewVersionEnterTag(tagName: string;
  properties: TProperties): boolean;
begin
  if striequal(tagName,'stabil') then begin
    fnewversionstr:=getProperty('value',properties);
    fnewversion:=StrToIntDef(fnewversionstr,0);
    exit(false);
  end;
  Result:=true;
end;

procedure TAutoUpdater.loadNewVersion(const url: string);
var versionXML:string;
begin
  needInternet;
  versionXML:=finternet.get(url);
  fnewversion:=0;
  fnewversionstr:='';
  fupdatebuildversion:=0;
  parseXML(versionXML,@loadNewVersionEnterTag,nil,nil,eUTF8);
end;

function TAutoUpdater.needBuildInfoEnterTag(tagName: string;
  properties: TProperties): boolean;
begin
  if striequal(tagname,'build') then begin
    fbuildinfolastbuild:=StrToIntDef(getProperty('version',properties),0);
    if (fbuildinfolastbuild<=fnewversion) and (fbuildinfolastbuild>fupdatebuildversion) then
        fupdatebuildversion:=fbuildinfolastbuild;
  end else if striequal(tagname,'installer') then
    if fbuildinfolastbuild=fnewversion then
      finstallerurl:=getProperty('url',properties);

  fbuildinfolasttag:=tagName;
  result:=true;
end;

function TAutoUpdater.needBuildInfoTextRead(text: string): boolean;
begin
  if (fbuildinfolastbuild<=fnewversion) and (fbuildinfolastbuild>fcurrentVersion) then begin
    if striequal(fbuildinfolasttag,'fix') then fallFixes+=text+LineEnding
    else if striequal(fbuildinfolasttag,'add') then fallAdds+=text+LineEnding
    else if striequal(fbuildinfolasttag,'change') then fallChanges+=text+LineEnding;
  end;
  fbuildinfolasttag:='';
  result:=true;
end;

procedure TAutoUpdater.needBuildInfo();
var changelogXML:string;
begin
  if fupdatebuildversion=fnewversion then exit;
  needInternet;
  changelogXML:=finternet.get(fchangelogURL);
  fallAdds:='';
  fallChanges:='';
  fallFixes:='';
  finstallerurl:='';
  fupdatebuildversion:=0;
  parseXML(changelogXML,@needBuildInfoEnterTag,nil,@needBuildInfoTextRead,eUTF8);
  if fupdatebuildversion<>fnewversion then
    raise Exception.Create('Die Informationen im ChangeLog sind anscheinend ungültig. Version '+fnewversionstr+' nicht gefunden, nur '+IntToStr(fupdatebuildversion)+'.');
end;



procedure TAutoUpdater.needInternet; inline;
begin
  if finternet=nil then
    finternet:=defaultInternetAccessClass.create; //TODO: generic internet
end;

constructor TAutoUpdater.create(currentVersion: TVersionNumber; installDir,
  versionsURL,changelogURL: string);
begin
  fcurrentVersion:=currentVersion;
  finstallDir:=installDir;
  if finstallDir[length(finstallDir)]<>DirectorySeparator then
    finstallDir:=ExtractFilePath(ParamStr(0));
  fversionsURL:=versionsURL;
  fchangelogURL:=changelogURL;
end;

function TAutoUpdater.existsUpdate: boolean;
begin
  result:=false;
  try
    loadNewVersion(fversionsURL);
    result:=fnewversion>fcurrentVersion;
  except
    on EInternetException do result:=false;
  end;
end;

function TAutoUpdater.listChanges: string;
begin
  needBuildInfo();
  Result:='';
  if fallChanges<>'' then result+=#13#10'==Änderungen=='#13#10+fallChanges;
  if fallAdds<>'' then result+=#13#10'==Hinzufügungen=='#13#10+fallAdds;
  if fallFixes<>'' then result+=#13#10'==Gelöste Fehler=='#13#10+fallFixes;
end;

procedure TAutoUpdater.downloadUpdate(tempDir: string);
var updateUrl:string;
    update:TFileStream;
    {$IFDEF showProgress}progress:TProgressBarDialog;{$ENDIF}
begin
  needBuildInfo();
  if finstallerurl='' then
    raise exception.Create('Die Internetadresse des Updates konnte leider nicht ermittelt werden.'#13#10+
                           'Bitte laden Sie es manuell von '+homepageAlternative+' herunter, oder versuchen es später nochmal.');

  updateUrl:=finstallerurl;

  ftempDir:=tempDir;
  if ftempDir='' then
    ftempDir:=GetTempDir();
  if ftempDir[length(ftempDir)]<>DirectorySeparator then
    ftempDir:=ftempDir+DirectorySeparator;
  try
    //RemoveDir(copy(ftempDir,1,length(ftempdir)-1));
    mkdir(copy(ftempDir,1,length(ftempdir)-1));
  except
    if not DirectoryExists(copy(ftempDir,1,length(ftempdir)-1)) then
      raise exception.create('Temporäres Verzeichnis '+ftempDir+' konnte nicht erstellt werden');
  end;

  {$IFDEF showProgress}
    progress:=TProgressBarDialog.create('Update','Bitte warten Sie während das Update auf Version '+fnewversionstr+' geladen wird:');
  {$ENDIF}
  update:=TFileStream.Create(ftempDir+'videlibriupdate.exe',fmCreate);
  try
    finternet.get(updateUrl,update,@progress.progressEvent);
    if update.Size=0 then Abort;
    update.free;
  except
    raise EXCEPTION.Create('Das Update konnte nicht heruntergeladen werden');
  end;
  {$IFDEF showProgress}
    progress.free;
  {$ENDIF}
end;

function TAutoUpdater._needRestart: boolean;
begin
  result:=true;
end;

procedure TAutoUpdater.installUpdate;
begin
  //need win32, deprecated
  {$IFDEF WIN32}
  WinExec(pchar('"'+ftempDir+'videlibriupdate.exe" /SP- /silent /noicons "/dir='+ExtractFilePath(ParamStr(0))+'"'),SW_SHOWNORMAL);
  {$ELSE}
  //TODO:
  {$ENDIF}
end;

destructor TAutoUpdater.destroy;
begin
  if finternet<>nil then
    finternet.free;
  inherited destroy;
end;

end.

