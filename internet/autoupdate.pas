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
  Classes, SysUtils,internetaccess,XMLRead,DOM,XMLWrite
  {$IFDEF showProgress},progressDialog{$ENDIF}
  ;
type

{ TAutoUpdater }

TVersionNumber=longint;
TAutoUpdater=class
private
  finternet:TInternetAccess;
  fUpdateStyle:(usInstaller);
  fupdateExisting{,fneedBAT}:boolean;
  fcurrentVersion,fnewversion:TVersionNumber;
  fnewversionstr:string;
  finstallDir,fversionsURL,fchangelogURL: string;
  ftempDir:string;
  fchangeLog:TDOMNode;
  function loadValues(const url: string):TXMLDocument;
  procedure needInternet;inline;
  procedure needChangeLog;
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
  
  property newVersion: TVersionNumber read fnewversion;
  property needRestart: boolean read _needRestart;
end;
const homepageAlternative='www.benibela.de';
implementation
uses w32internetaccess,inifiles,(*unzip,ziptypes,*)windows;

{ TAutoUpdater }

function xmlGetText(node: TDOMNode):string;
var i:longint;
begin
  if node.NodeType=TEXT_NODE then result:=node.NodeValue
  else begin
    result:='';
    for i:=0 to node.ChildNodes.Count-1 do
      result+=xmlGetText(node.ChildNodes.Item[i]);
  end;
end;

function xmlGetAttribute(node: TDOMNode;name: string='value'):string;
begin
  result:=node.Attributes.GetNamedItem(name).NodeValue;
end;

function xmlFindNode(node:TDOMNode;nodeName:string):TDOMNode;overload;
var i:longint;
begin
  Result:=nil;
  for i:=0 to node.ChildNodes.Count-1 do
    if node.ChildNodes.item[i].NodeName=nodeName then
      exit(node.ChildNodes.item[i]);
end;

function xmlFindNode(node:TDOMNode;nodeName,attribName,attribValue:string):TDOMNode;overload;
var i:longint;
begin
  Result:=nil;
  for i:=0 to node.ChildNodes.Count-1 do
    if node.ChildNodes.item[i].NodeName=nodeName then
      if xmlGetAttribute(node.ChildNodes.item[i],attribName)=attribValue then
        exit(node.ChildNodes.item[i]);
end;


function TAutoUpdater.hasDirectoryWriteAccess: boolean;
var f:THandle;
begin
  if Win32Platform=VER_PLATFORM_WIN32_WINDOWS then result:=true
  else if Win32Platform=VER_PLATFORM_WIN32_NT then begin
    f:=CreateFile(pchar(copy(finstallDir,1,length(finstallDir)-1) ),GENERIC_WRITE, FILE_SHARE_WRITE or FILE_SHARE_READ, nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS,0);
    if f=INVALID_HANDLE_VALUE then result:=false
    else begin
      result:=true;
      closehandle(f);
    end;
  end else result:=false;
end;

function TAutoUpdater.loadValues(const url: string): TXMLDocument;
var stream:TMemoryStream;
    s:string;
begin
  needInternet;
  stream:=TMemoryStream.Create;
  finternet.get(url,stream);
  stream.Position:=0;
  ReadXMLFile(Result,stream);
 // MessageBox(0,pchar(result.NodeName),'',0);
//  WriteXMLFile(result,'T:\test.xml');
end;

procedure TAutoUpdater.needInternet; inline;
begin
  if finternet=nil then
    finternet:=TW32InternetAccess.create; //TODO: generic internet
end;

procedure TAutoUpdater.needChangeLog;
begin
  needInternet;
  if fchangeLog<>nil  then fchangeLog.free;
  fchangeLog:=loadValues(fchangelogURL);
  if fchangeLog=nil then
    raise exception.Create('ChangeLog konnte nicht geladen werden, das Update kann nicht automatisch heruntergeladen werden.'#13#10+
                           'Bitte laden Sie es manuell von '+homepageAlternative+' herunter, oder versuchen es später nochmal.');

end;

constructor TAutoUpdater.create(currentVersion: TVersionNumber; installDir,
  versionsURL,changelogURL: string);
begin
  fcurrentVersion:=currentVersion;
  finstallDir:=installDir;
  if finstallDir[length(finstallDir)]<>'\' then
    finstallDir:=ExtractFilePath(ParamStr(0));
  fversionsURL:=versionsURL;
  fchangelogURL:=changelogURL;
end;

function TAutoUpdater.existsUpdate: boolean;
var versions: TXMLDocument;
begin
  result:=true;
  try
    versions:=loadValues(fversionsURL);
    fnewversionstr:=xmlGetAttribute(versions.ChildNodes.Item[0].FindNode('stabil'));
    fnewversion:=StrToIntDef(fnewversionstr,fcurrentVersion);//.GetNamedItem(');
    versions.free;
    //fnewversion:=StrToInt(deleteNewLine(finternet.get(fversionURL)));
    
    result:=fnewversion>fcurrentVersion;
  except
    on EInternetException do result:=false;
  end;
end;

function TAutoUpdater.listChanges: string;
var i,j:longint;
    build:TDOMNode;
    fixes,changes,adds:string;
begin
  needChangeLog;
  fixes:='';
  changes:='';
  adds:='';
  for i:=0 to fchangeLog.ChildNodes.Item[0].ChildNodes.Count-1 do
    if fchangeLog.ChildNodes.Item[0].ChildNodes.Item[i].NodeName='build' then begin
      build:=fchangeLog.ChildNodes.Item[0].ChildNodes.Item[i];
      if (StrToIntDef(xmlGetAttribute(build,'version'),fcurrentVersion)
         > fcurrentVersion) and (StrToIntDef(xmlGetAttribute(build,'version'),fcurrentVersion)
         <= fnewVersion) then
        for j:=0 to build.ChildNodes.Count-1 do
          if build.ChildNodes.Item[j].NodeName='fix' then
           fixes+=xmlGetText(build.ChildNodes.Item[j])+#13#10
          else if build.ChildNodes.Item[j].NodeName='add' then
           adds+=xmlGetText(build.ChildNodes.Item[j])+#13#10
          else if build.ChildNodes.Item[j].NodeName='change' then
           changes+=xmlGetText(build.ChildNodes.Item[j])+#13#10
    end;;
  Result:='';
  if changes<>'' then result+='==Änderungen=='#13#10+changes;
  if adds<>'' then result+='==Hinzufügungen=='#13#10+adds;
  if fixes<>'' then result+='==Gelöste Fehler=='#13#10+fixes;
end;

{function sayYes( Rec : pReportRec ) : Boolean;
begin
  result:=true;
end;}

procedure TAutoUpdater.downloadUpdate(tempDir: string);
//var ini:TIniFile;
    //sl:TStringList;
    //str:TMemoryStream;
    //pack:string;
  //  f:file;//TFileStream;
var build:TDOMNode;
    installerNode,changeLogNode:TDOMNode;
    updateUrl:string;
    update:TFileStream;
    {$IFDEF showProgress}progress:TProgressBarDialog;{$ENDIF}
begin
  needChangeLog;

  changeLogNode:=xmlFindNode(fchangeLog.ChildNodes.Item[0],'changeLog');
  if changeLogNode = nil then
    raise Exception.Create('Die ChangeLog xml-Datei ist ungültig.'#13#10+
                           'Das Update kann momentan nicht automatisch heruntergeladen werden.'#13#10+
                           'Bitte laden Sie es manuell von '+homepageAlternative+' herunter, oder versuchen es später nochmal.');

  build:=xmlFindNode(changeLogNode,'build','version',fnewversionstr);
  if build = nil then
    raise Exception.Create('Build mit Versionnummer '+fnewversionstr+' konnte im ChangeLog nicht gefunden werden.'#13#10+
                           'Das Update kann momentan nicht automatisch heruntergeladen werden.'#13#10+
                           'Bitte laden Sie es manuell von '+homepageAlternative+' herunter, oder versuchen es später nochmal.');
  installerNode:=xmlFindNode(build,'installer');
  if installerNode=nil then
    raise Exception.Create('Installer für Build mit Versionnummer '+fnewversionstr+' konnte im ChangeLog nicht gefunden werden.'#13#10+
                           'Das Update kann momentan nicht automatisch heruntergeladen werden.'#13#10+
                           'Bitte laden Sie es manuell von '+homepageAlternative+' herunter, oder versuchen es später nochmal.');

  updateUrl:=xmlGetAttribute(installerNode,'url');
  if updateUrl='' then
    raise exception.Create('Die Internetadresse des Updates konnte leider nicht ermittelt werden.'#13#10+
                           'Bitte laden Sie es manuell von '+homepageAlternative+' herunter, oder versuchen es später nochmal.');

  ftempDir:=tempDir;
  if ftempDir='' then
    ftempDir:=GetTempDir();
  if ftempDir[length(ftempDir)]<>'\' then
    ftempDir:=ftempDir+'\';
  //ftempDir:=ftempDir+IntToStr(fnewversion)+'\';
  
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

            //http://www.w3.org/TR/xpath#location-paths
  {
  //str:=TMemoryStream.create;
  sl:=TStringList.create;
  sl.text:=finternet.get(fversionDetailsURL);
  sl.SaveToFile(ftempDir+'_#_UPDATE_DETAILS_#_.ini');
  ini:=TIniFile.Create(ftempDir+'_#_UPDATE_DETAILS_#_.ini');
//  ini.SetStrings(sl);
  pack:=ini.ReadString(IntToStr(fnewversion),'package-url','');
  if pack='' then raise Exception.Create('Detaillierte Updateinformationen sind nicht verfügbar');
  ini.free;
//  str.free;
  sl.free;
  DeleteFile(pchar(ftempDir+'_#_UPDATE_DETAILS_#_.ini'));


  pack:=finternet.get(pack);
//  f:=TFileStream.Create(ftempDir+'_#_UPDATE_#_UPDATE_#_.zip',fmCreate);
  //f.Write(pack,length(pack));
//  f.Free;
  assign(f,ftempDir+'_#_UPDATE_#_UPDATE_#_.zip');
  rewrite(f,1);
  BlockWrite(f,pack[1],length(pack));
  close(f);

  if FileUnzip(pchar(ftempDir+'_#_UPDATE_#_UPDATE_#_.zip'),pchar(ftempDir),'*.*',nil,@sayYes)=0 then
    raise Exception.Create('Update konnte nicht entpackt werden');

  DeleteFile(pchar(ftempDir+'_#_UPDATE_#_UPDATE_#_.zip'));

  fupdateExisting:=true;
  //result:=fupdateExisting;
  fneedBAT:=false;}
end;

function TAutoUpdater._needRestart: boolean;
begin
  {result:=not FileExists(ftempDir+ExtractFileName(ParamStr(0)));
  fneedBAT:=result;}
  result:=true;
end;

procedure TAutoUpdater.installUpdate;
{var frec:TSearchRec;
    myName:string;
    sl:TStringList;
    deltree:string;}
begin
  //need win32, deprecated
  WinExec(pchar('"'+ftempDir+'videlibriupdate.exe" /SP- /silent /noicons "/dir='+ExtractFilePath(ParamStr(0))+'"'),SW_SHOWNORMAL);

  (*myName:=lowercase(ExtractFileName(ParamStr(0)));
  FindFirst(ftempDir+'*.*',faAnyFile,frec);
  repeat
    if (frec.Name='') or (frec.Name[1]='.')  then continue;
    if lowercase(frec.Name)=myName then begin
      fneedbat:=true;
      continue;
    end;
    //copyFile(ftempDir+frec.Name,finstallDir+frec.Name);
    //DeleteFile(ftempDir+frec.Name)
    RenameFile(ftempDir+frec.Name,finstallDir+frec.Name);//should move it
    DeleteFile(pchar(ftempDir+frec.Name));
  until FindNext(frec)<>0;
  sysutils.FindClose(frec);
  
  if Win32Platform=VER_PLATFORM_WIN32_NT then deltree:='rd /S /Q'
  else deltree:='deltree /Y';

  if fneedBAT then begin
    sl:=TStringList.Create;
    sl.text:='@echo off'#13#10+
             'echo Update wird installiert...'+#13#10+
             ':Marke'#13#10+
             'Del "'+finstallDir+myName+'"'#13#10+
             'If EXIST "'+finstallDir+myName+'" Goto Marke'#13#10+
             'copy "'+ftempDir+myName+'" "'+finstallDir+myName+'"'#13#10+
//             'del "'+ftempDir+myName+'"'#13#10+
             '"'+finstallDir+myName+'" /updated-to '+inttostr(fnewversion)+#13#10+
             deltree+' "'+copy(ftempDir,1,length(ftempdir)-1)+'"'+#13#10
             ;
//             'del "'+ftempDir+'replaceExe.bat"'#13#10+
    sl.SaveToFile(ftempDir+'replaceExe.bat');
    sl.free;
    winexec(pchar('"'+ftempDir+'replaceExe.bat"'),sw_hide);
  end else RemoveDirectory(pchar(copy(ftempDir,1,length(ftempdir)-1)));*)
end;

destructor TAutoUpdater.destroy;
begin
  if finternet<>nil then
    finternet.free;
  if fchangeLog<>nil then
    fchangeLog.free;
  inherited destroy;
end;

end.

