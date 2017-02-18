(***
 @abstract(This unit implements the file module of http://expath.org/spec/file )

 Call registerModuleFile to register it.
 Afterwards you can use e.g. @code(query('Q{http://expath.org/ns/file}exists("/tmp/")')) to test for the existence of a file.
 If you add the namespace to the namespaces in the static context, you can write it simpler as @code(query('f:exists("/tmp/")')).





 not much tested
*)

unit xquery_module_file;


{$mode objfpc}{$H+}

interface

uses
  simplehtmltreeparser;


//**Registers the module to the XQuery engine
procedure registerModuleFile;

const XMLNamespaceURL_Expath_File = 'http://expath.org/ns/file';
var XMLNamespace_Expath_File: INamespace;
implementation

uses Classes, SysUtils, xquery, bbutils, strutils, bigdecimalmath, base64, math, xquery__regex
  , internetaccess //it does not need internet access itself, just the URI encoding function there
  {$ifdef unix},BaseUnix{$endif}
  {$ifdef windows},windows{$endif}
    ;
function strFileName({normalized}path: string): string; forward;
function myList(const path: IXQValue; relative, recurse: boolean; mask: string = '*'): IXQValue; forward;

//////////////////////////////
//copied from the LCL to reduce dependancies
type
  TCopyFileFlag = (
    cffOverwriteFile,
    cffCreateDestDirectory,
    cffPreserveTime
    );
  TCopyFileFlags = set of TCopyFileFlag;
function CopyFile(const SrcFilename, DestFilename: String;
                  Flags: TCopyFileFlags=[cffOverwriteFile]; ExceptionOnError: Boolean=False): Boolean;
var
  SrcHandle: THandle;
  DestHandle: THandle;
  Buffer: array[1..4096] of byte;
  ReadCount, WriteCount, TryCount: LongInt;
begin
  Result := False;
  // check overwrite
  if (not (cffOverwriteFile in Flags)) and FileExists(DestFileName) then
    exit;
  // check directory
  if (cffCreateDestDirectory in Flags)
  and (not DirectoryExists(ExtractFilePath(DestFileName)))
  and (not ForceDirectories(ExtractFilePath(DestFileName))) then
    exit;
  TryCount := 0;
  While TryCount <> 3 Do Begin
    SrcHandle := FileOpen(SrcFilename, fmOpenRead or fmShareDenyWrite);
    if (THandle(SrcHandle)=feInvalidHandle) then Begin
      Inc(TryCount);
      Sleep(10);
    End
    Else Begin
      TryCount := 0;
      Break;
    End;
  End;
  If TryCount > 0 Then
  begin
    if ExceptionOnError then
      raise EFOpenError.CreateFmt({SFOpenError}'Unable to open file "%s"', [SrcFilename])
    else
      exit;
  end;
  try
    DestHandle := FileCreate(DestFileName);
    if (THandle(DestHandle)=feInvalidHandle) then
    begin
      if ExceptionOnError then
        raise EFCreateError.CreateFmt({SFCreateError}'Unable to create file "%s"',[DestFileName])
      else
        Exit;
    end;
    try
      repeat
        ReadCount:=FileRead(SrcHandle,Buffer[1],High(Buffer));
        if ReadCount<=0 then break;
        WriteCount:=FileWrite(DestHandle,Buffer[1],ReadCount);
        if WriteCount<ReadCount then
        begin
          if ExceptionOnError then
            raise EWriteError.CreateFmt({SFCreateError}'Unable to write to file "%s"',[DestFileName])
          else
            Exit;
        end;
      until false;
    finally
      FileClose(DestHandle);
    end;
    if (cffPreserveTime in Flags) then
      FileSetDate(DestFilename, FileGetDate(SrcHandle));
    Result := True;
  finally
    FileClose(SrcHandle);
  end;
end;

function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: Boolean; ExceptionOnError: Boolean): boolean;
// Flags parameter can be used for the same thing.
var
  Flags: TCopyFileFlags;
begin
  if PreserveTime then
    Flags:=[cffPreserveTime, cffOverwriteFile]
  else
    Flags:=[cffOverwriteFile];
  Result := CopyFile(SrcFilename, DestFilename, Flags, ExceptionOnError);
end;



function ResolveDots(const AFilename: string): string;
//trim double path delims and expand special dirs like .. and .
//on Windows change also '/' to '\' except for filenames starting with '\\?\'
var SrcPos, DestPos, l, DirStart: integer;
  c: char;
  MacroPos: LongInt;
begin
  Result:=AFilename;
  {$ifdef windows}
  //Special case: everything is literal after this, even dots (this does not apply to '//?/')
  if (Pos('\\?\', AFilename) = 1) then Exit;
  {$endif}

  l:=length(AFilename);
  SrcPos:=1;
  DestPos:=1;


  // trim double path delimiters and special dirs . and ..
  while (SrcPos<=l) do begin
    c:=AFilename[SrcPos];
    {$ifdef windows}
    //change / to \. The WinApi accepts both, but it leads to strange effects in other places
    if (c in AllowDirectorySeparators) then c := PathDelim;
    {$endif}
    // check for double path delims
    if (c=PathDelim) then begin
      inc(SrcPos);
      {$IFDEF Windows}
      if (DestPos>2)
      {$ELSE}
      if (DestPos>1)
      {$ENDIF}
      and (Result[DestPos-1]=PathDelim) then begin
        // skip second PathDelim
        continue;
      end;
      Result[DestPos]:=c;
      inc(DestPos);
      continue;
    end;
    // check for special dirs . and ..
    if (c='.') then begin
      if (SrcPos<l) then begin
        if (AFilename[SrcPos+1]=PathDelim)
        and ((DestPos=1) or (AFilename[SrcPos-1]=PathDelim)) then begin
          // special dir ./
          // -> skip
          inc(SrcPos,2);
          continue;
        end else if (AFilename[SrcPos+1]='.')
        and (SrcPos+1=l) or (AFilename[SrcPos+2]=PathDelim) then
        begin
          // special dir ..
          //  1. ..      -> copy
          //  2. /..     -> skip .., keep /
          //  3. C:..    -> copy
          //  4. C:\..   -> skip .., keep C:\
          //  5. \\..    -> skip .., keep \\
          //  6. xxx../..   -> copy
          //  7. xxxdir/..  -> trim dir and skip ..
          //  8. xxxdir/..  -> trim dir and skip ..
          if DestPos=1 then begin
            //  1. ..      -> copy
          end else if (DestPos=2) and (Result[1]=PathDelim) then begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF Windows}
          end else if (DestPos=3) and (Result[2]=':')
          and (Result[1] in ['a'..'z','A'..'Z']) then begin
            //  3. C:..    -> copy
          end else if (DestPos=4) and (Result[2]=':') and (Result[3]=PathDelim)
          and (Result[1] in ['a'..'z','A'..'Z']) then begin
            //  4. C:\..   -> skip .., keep C:\
            inc(SrcPos,2);
            continue;
          end else if (DestPos=3) and (Result[1]=PathDelim)
          and (Result[2]=PathDelim) then begin
            //  5. \\..    -> skip .., keep \\
            inc(SrcPos,2);
            continue;
          {$ENDIF}
          end else if (DestPos>1) and (Result[DestPos-1]=PathDelim) then begin
            if (DestPos>3)
            and (Result[DestPos-2]='.') and (Result[DestPos-3]='.')
            and ((DestPos=4) or (Result[DestPos-4]=PathDelim)) then begin
              //  6. ../..   -> copy
            end else begin
              //  7. xxxdir/..  -> trim dir and skip ..
              DirStart:=DestPos-2;
              while (DirStart>1) and (Result[DirStart-1]<>PathDelim) do
                dec(DirStart);
              MacroPos:=DirStart;
              while MacroPos<DestPos do begin
                if (Result[MacroPos]='$')
                and (Result[MacroPos+1] in ['(','a'..'z','A'..'Z']) then begin
                  // 8. directory contains a macro -> keep
                  break;
                end;
                inc(MacroPos);
              end;
              if MacroPos=DestPos then begin
                DestPos:=DirStart;
                inc(SrcPos,2);
                continue;
              end;
            end;
          end;
        end;
      end else begin
        // special dir . at end of filename
        if DestPos=1 then begin
          Result:='.';
          exit;
        end else begin
          // skip
          break;
        end;
      end;
    end;
    // copy directory
    repeat
      Result[DestPos]:=c;
      inc(DestPos);
      inc(SrcPos);
      if (SrcPos>l) then break;
      c:=AFilename[SrcPos];
      {$ifdef windows}
      //change / to \. The WinApi accepts both, but it leads to strange effects in other places
      if (c in AllowDirectorySeparators) then c := PathDelim;
      {$endif}
      if c=PathDelim then break;
    until false;
  end;
  // trim result
  if DestPos<=length(AFilename) then
    SetLength(Result,DestPos-1);
end;

//////////////////////////////

function strAddPathSeparator(path: string): string;
begin
  if path = '' then path := '.' + DirectorySeparator;
  if not (path[length(path)] in AllowDirectorySeparators) then path += DirectorySeparator;
  result := path;
end;

type TFileLister = class
  recurse: boolean;
  constructor create;
  procedure foundSomething(const dir, current: String; const search: TRawByteSearchRec); virtual;
  procedure startSearch(path: string; prefixForOutput: string = ''); virtual;
end;

constructor TFileLister.create;
begin
  recurse := true;
end;

procedure TFileLister.foundSomething(const dir, current: String; const search: TRawByteSearchRec);
  function isSymLink: boolean;
  {$ifdef windows}
  const
    IO_REPARSE_TAG_MOUNT_POINT = $A0000003;
    IO_REPARSE_TAG_SYMLINK     = $A000000C;
  var
    temp: LongInt;
  {$endif}
  begin
    {$ifdef windows}
    {$ifdef wince}
    result := false;
    {$else}
    Result := (search.FindData.dwReserved0 = IO_REPARSE_TAG_SYMLINK) or (search.FindData.dwReserved0 = IO_REPARSE_TAG_MOUNT_POINT);
    if result then begin
      temp := FileGetAttr(current);
      result := (temp <> -1) and (temp and FILE_ATTRIBUTE_REPARSE_POINT <> 0);
    end;
    {$endif}
    {$else}
    result := FpReadLink(current) <> '';
    {$endif}
  end;
begin
  if ((faDirectory and search.Attr) <> 0) and not isSymLink and recurse then
    startSearch(dir + search.Name, current );
end;

procedure TFileLister.startSearch(path: string; prefixForOutput: string = '');
var
  search: TRawByteSearchRec;
  current: string;


begin
  path := strAddPathSeparator(path);
  if prefixForOutput <> '' then prefixForOutput := strAddPathSeparator(prefixForOutput);
  if SysUtils.FindFirst(path + '*', faAnyFile, search) = 0 then begin
    try
      repeat
        case search.Name of
          '.', '..', '': continue;
        end;
        current := prefixForOutput + search.Name;
        foundSomething(path, current, search);
      until SysUtils.FindNext(search) <> 0;
    finally
      SysUtils.FindClose(search);
    end;
  end;


end;


const Error_NoDir = 'no-dir';
      Error_IsDir = 'is-dir';
      Error_Exists = 'exists';
      Error_Io_Error = 'io-error';
      Error_Not_Found =  'not-found';
      Error_Out_Of_Range = 'out-of-range';
      error_unknown_encoding = 'unknown-encoding';

var module: TXQNativeModule = nil;

procedure raiseFileError(code, message: string; const item: IXQValue = nil);
begin
  if item <> nil then message += '("'+item.toJoinedString()+'")';
  raise EXQEvaluationException.create(code, message, XMLNamespace_Expath_File, item);
end;

function xqToInt64(const v: IXQValue; out res: int64): boolean;
var
  temp: BigDecimal;
begin
  result := true;
  case v.kind of
    pvkInt64: res := v.toInt64;
    pvkFloat: begin
      if IsNan(v.toFloat) or (IsInfinite(v.toFloat) and (v.toFloat < 0)) then begin
        exit(false);
      end else if IsInfinite(v.toFloat) then begin
        res := high(res);
      end else res := round(v.toFloat);
    end;
    {pvkBigDecimal:}else begin
      temp := round(v.toDecimal);
      if not isInt64(temp) then exit(false);
      res := BigDecimalToInt64(temp);
    end;
  end;
end;

function xqToUInt64(const v: IXQValue; out res: int64): boolean;
begin
  result := xqToInt64(v, res);
  if res < 0 then result := false;
end;

function normalizePath(const path: IXQValue): string;
var
  i: Integer;
begin
  result := path.toString;
  if strBeginsWith(result, 'file://') then begin
    delete(result, 1, 7);
    {$ifdef windows}
    if strBeginsWith(result, '/') then delete(result, 1, 1);
    {$endif};
    result := urlHexDecode(result)
  end;
  for i := 1 to length(result) do
    if result[i] in AllowDirectorySeparators then result[i] := DirectorySeparator
end;

function suffixDirectoy(const s: string): string;
begin
  result := s;
  if not strEndsWith(result, DirectorySeparator) and DirectoryExists(result) then result += DirectorySeparator;
end;

function FileExistsAsTrueFile(const Filename: string): boolean;
{$ifdef windows}
var
  temp: DWORD;
{$endif}
begin
  {$ifdef windows}
  temp := FileGetAttr(Filename);
  result := (temp <> $ffffffff) and ((temp and FILE_ATTRIBUTE_DIRECTORY) = 0);
  {$else}
  result := FileExists(Filename) and not DirectoryExists(Filename);
  {$endif}
end;

function FileOrDirectoryExists(const Filename: string): boolean;
begin
  {$ifdef windows}
  result := DWORD(FileGetAttr(Filename)) <> $ffffffff;
  {$else}
  result := FileExists(Filename);
  {$endif}
end;

function exists(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := xqvalue(FileOrDirectoryExists(normalizePath(args[0])));
end;

function is_dir(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := xqvalue(DirectoryExists(normalizePath(args[0])));
end;

function is_file(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := xqvalue(FileExistsAsTrueFile(args[0].toString));
end;

function last_modified(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  dateTime: TDateTime;
  fn: String;
  search: TRawByteSearchRec;
  dt: TXQValueDateTime;
begin
  ignore(context);
  fn := normalizePath(args[0]);
  if sysutils.FindFirst(fn, faAnyFile, search) <> 0 then
    raiseFileError(ifthen(FileOrDirectoryExists(normalizePath(args[0])), Error_Io_Error, Error_Not_Found), 'Could not get age', args[0] );
  dateTime := FileDateToDateTime(search.Time);
  sysutils.FindClose(search);
  dt := TXQValueDateTime.create(baseSchema.dateTime, dateTime);
  dt.value.timezone:=GetLocalTimeOffset;;
  result := dt;
end;

function size(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  code: String;
  path: String;
  f: file of byte;
begin
  ignore(context);
  {$IOCHECKS ON}
  path := normalizePath(args[0]);
  if DirectoryExists(path) then exit(xqvalue(0));

  try
    AssignFile(f, path);
    reset(f);
    result := xqvalue(system.FileSize(f));
    CloseFile(f);
  except
    if FileOrDirectoryExists(path) then code := Error_Io_Error
    else code := Error_Not_Found;
    raiseFileError(code, 'Failed to get size', args[0]);
  end;
end;

function writeOrAppendSomething(const filename: IXQValue; append: boolean; data: rawbytestring; offset: int64 = -1): IXQValue;
var f: TFileStream;
    mode: word;
    path: AnsiString;
    errcode: String;
begin
  path := normalizePath(filename);
  if append then if not FileExists(path) then append := false;
  if append then mode := fmOpenReadWrite
  else mode := fmCreate;
  try
    f := TFileStream.Create(path, mode);
  except
    on e: EStreamError do begin
      errcode := Error_Io_Error;
      if DirectoryExists(path) then errcode := Error_IsDir
      else begin
        path := strBeforeLast(path, AllowDirectorySeparators);
        if (path <> '') and  not DirectoryExists(path) then errcode := Error_NoDir;      ;
      end;
      raiseFileError(errcode, 'Failed to open file for writing/appending', filename);
    end;
  end;
  try
    if offset >= 0 then begin
      f.Position := offset;
      if offset > f.Size then raiseFileError(Error_Out_Of_Range, Error_Out_Of_Range, filename);
    end else if append then f.position := f.size;
    if length(data) > 0 then
      try
        f.WriteBuffer(data[1], length(data));
      except
        on e: EStreamError do
          raiseFileError(Error_Io_Error, 'Failed to write', filename);
      end;
  finally
    f.free;
  end;
  result := xqvalue();
end;

function writeOrAppendSerialized({%H-}argc: SizeInt; args: PIXQValue; append: boolean): IXQValue;
var
  temp: TXQueryEngine;
  data: IXQValue;
begin
  temp := TXQueryEngine.create;
  temp.VariableChangelog.add('data', args[1]);
  if argc = 3 then temp.VariableChangelog.add('args', args[2])
  else temp.VariableChangelog.add('args', xqvalue());
  data := temp.evaluateXQuery3('serialize($data, $args)'); //todo call serialization directly, handle encoding
  temp.free;
  result := writeOrAppendSomething(args[0], append, data.toString);
end;

function writeOrAppendText({%H-}argc: SizeInt; args: PIXQValue; append: boolean; text: string): IXQValue;
var
  data: String;
  enc: TSystemCodePage;
begin
  data := text;
  if argc = 3 then begin
    enc := strEncodingFromName(args[2].toString);
    if enc = CP_NONE then raise EXQEvaluationException.create(error_unknown_encoding, 'Unknown encoding: '+args[2].toString, XMLNamespace_Expath_File, args[2]);
    data := strChangeEncoding(data, CP_UTF8, enc);
  end;
  result := writeOrAppendSomething(args[0], append, data);
end;

function append(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := writeOrAppendSerialized(argc, args, true);
end;
function append_Binary(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := writeOrAppendSomething(args[0], true, (args[1] as TXQValueString).toRawBinary);
end;
function append_Text(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := writeOrAppendText(argc, args, true, args[1].toString);
end;
function append_Text_Lines(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := writeOrAppendText(argc, args, true, args[1].toJoinedString(LineEnding) + LineEnding);
end;

function write(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := writeOrAppendSerialized(argc, args, false);
end;
function write_Binary(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  offset: int64;
begin
  ignore(context);
  offset := -1;
  if argc >= 3 then if not xqToUInt64(args[2], offset) then raiseFileError(Error_Out_Of_Range, Error_Out_Of_Range, args[2]);
  result := writeOrAppendSomething(args[0], argc >= 3, (args[1] as TXQValueString).toRawBinary, offset);
end;
function write_Text(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := writeOrAppendText(argc, args, false, args[1].toString);
end;
function write_Text_Lines(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  result := writeOrAppendText(argc, args, false, args[1].toJoinedString(LineEnding) + LineEnding);
end;


type TDirCopier = class(TFileLister)
  dest: string;
  source: String;
  procedure foundSomething(const dir, current: String; const search: TRawByteSearchRec); override;
  class procedure checkResult(const res: boolean; const fn: string);
end;
procedure TDirCopier.foundSomething(const dir, current: String; const search: TRawByteSearchRec);
begin
  if search.Attr and faDirectory <> 0 then begin
    checkResult(CreateDir(dest + current), current);
  end else
    checkResult(CopyFile(source + current, dest + current), current);
  inherited;
end;
class procedure TDirCopier.checkResult(const res: boolean; const fn: string);
begin
  if not res then raiseFileError(Error_Io_Error, 'Failed to copy ' + fn);
end;



function copy(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  source, dest: String;
  copier: TDirCopier;
begin
  ignore(context);
  source := normalizePath(args[0]);
  dest := normalizePath(args[1]);
  if source = dest then raiseFileError(Error_Io_Error, 'source = dest', args[0]);
  try
    if DirectoryExists(dest) then dest := strAddPathSeparator(dest) + strFileName(source);
    if DirectoryExists(source) then begin
      if FileExistsAsTrueFile(dest) then raiseFileError(Error_Exists, 'Target cannot be overriden', args[1]);
      ForceDirectories(dest);
      copier := TDirCopier.Create;
      try
        copier.source := strAddPathSeparator(source);
        copier.dest := strAddPathSeparator(dest);
        copier.startSearch(copier.source);
      finally
        copier.free;
      end;
    end else begin
      if not FileOrDirectoryExists(source) then raiseFileError(Error_Not_Found, 'No source', args[0]);
      TDirCopier.checkResult(CopyFile(source, dest), dest);
    end;
  except
    on EStreamError do TDirCopier.checkResult(false, dest);
  end;
  result := xqvalue();
end;

function create_dir(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  dir, code: String;
begin
  ignore(context);
  dir := normalizePath(args[0]);
  if not ForceDirectories(dir) then begin
    code := Error_Io_Error;
    repeat
      if FileExistsAsTrueFile(dir) then begin code := Error_Exists; break; end;
      dir := strBeforeLast(dir, DirectorySeparator);
    until dir = '';
    raiseFileError( code, 'Failed to create directories', args[0] );
  end;
  result := xqvalue();
end;

function create_temp_dir(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  dir: String;
begin
  ignore(context);
  if argc = 3 then begin
    dir := normalizePath(args[2]);
    if not DirectoryExists(dir) then raiseFileError(Error_NoDir, 'Invalid directory', args[2]);
  end
  else dir := GetTempDir();
  dir := dir + DirectorySeparator + args[0].toString + IntToHex(Random($FFFFFFFF),8) + args[1].toString;
  if not ForceDirectories(dir) then raiseFileError(Error_Io_Error, 'Failed');
  result := xqvalue(dir);
end;

function create_temp_file(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  dir: String;
begin
  ignore(context);
  if argc = 3 then begin
    dir := normalizePath(args[2]);
    if not DirectoryExists(dir) then raiseFileError(Error_NoDir, 'Invalid directory', args[2]);
  end
  else dir := GetTempDir();
  dir := dir + DirectorySeparator + args[0].toString + IntToHex(Random($FFFFFFFF),8) + args[1].toString;
  if not ForceDirectories(strResolveURI('/', dir)) then raiseFileError(Error_Io_Error, 'Failed');
  strSaveToFile(dir, '');
  result := xqvalue(dir);
end;



type TDirDeleter = class(TFileLister)
  dirs: tstringlist;
  procedure foundSomething(const dir, current: String; const search: TRawByteSearchRec); override;
  procedure startSearch(path: string; prefixForOutput: string=''); override;
  class procedure checkResult(const res: boolean; const fn: string);
end;

procedure TDirDeleter.startSearch(path: string; prefixForOutput: string);
begin
  inherited startSearch(path,prefixForOutput);
  dirs.Add(path);
end;

class procedure TDirDeleter.checkResult(const res: boolean; const fn: string);
begin
  if not res then raiseFileError(Error_Io_Error, 'Failed to delete ' + fn);
end;

procedure TDirDeleter.foundSomething(const dir, current: String; const search: TRawByteSearchRec);
begin
  inherited;
  //writeln(search.Attr and faDirectory, dir, ' ',current);
  if search.Attr and faDirectory <> 0 then
    dirs.Add(current)
  else
    TDirDeleter.checkResult(SysUtils.DeleteFile(current), current);
end;

function delete(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  path: String;
  recursive: Boolean;
  deleter: TDirDeleter;
  i: Integer;
begin
  ignore(context);
  path := normalizePath(args[0]);
  recursive := (argc = 2) and args[1].toBoolean;
  if not FileOrDirectoryExists(path) then raiseFileError(Error_Not_Found, 'Cannot delete something not existing', args[0]);
  if not DirectoryExists(path) then begin
    TDirDeleter.checkResult(SysUtils.DeleteFile(path), path)
  end else if recursive then begin
    deleter := TDirDeleter.Create;
    deleter.dirs := TStringList.Create;
    try
      deleter.startSearch(path, path);
      for i := 0 to Deleter.dirs.count - 1 do
        RemoveDir(deleter.dirs[i]);  //for some weird reason this returns false, even when the file was removed
      TDirDeleter.checkResult(not DirectoryExists(path), path);
    finally
      deleter.dirs.free;
      deleter.free
    end;
  end
  else  if not RemoveDir(path) then
    raiseFileError(ifthen(myList(args[0], false, false).getSequenceCount = 0, Error_Io_Error, Error_IsDir), 'Failed to delete ' + path);
  result := xqvalue();
end;

type TXQFileLister = class(TFileLister)
  seq: TXQVList;
  filter: TWrappedRegExpr;
  procedure foundSomething(const dir, current: String; const search: TRawByteSearchRec); override;
end;

procedure TXQFileLister.foundSomething(const dir, current: String; const search: TRawByteSearchRec);
var
  cur: String;
begin
  inherited;
  cur := current;
  if ((faDirectory and search.Attr) <> 0) and not strEndsWith(cur, DirectorySeparator) then cur += DirectorySeparator;
  if (filter = nil) or wregexprMatches(filter, current) then begin
    seq.add(xqvalue(cur));
  end;
end;


function myList(const path: IXQValue; relative, recurse: boolean; mask: string = '*'): IXQValue;
var
  dir, transformedMask: String;
  c: Char;
  lister: TXQFileLister;
begin
  dir := normalizePath(path);

  lister := TXQFileLister.Create;
  try
    lister.recurse := recurse;
    lister.seq := TXQVList.create();
    if (mask <> '*') and (mask <> '') then begin
      transformedMask := '';
      for c in mask do
        case c of
          '.', '^', '$', '{', '}', '\': transformedMask += '\' + c;
          '*': transformedMask += '.*';
          '?': transformedMask += '.';
          else transformedMask += c;
        end;
      transformedMask += '$';
      lister.filter := wregexprParse(transformedMask, [{$ifdef windows}wrfIgnoreCase{$endif}]);
    end;

    lister.startSearch(dir, ifthen(relative, '', dir));
    result := TXQValueSequence.create(lister.seq);
    lister.seq := nil;
    xqvalueSeqSqueeze(result);
    if result.getSequenceCount = 0 then
      if not DirectoryExists(dir) then
        raiseFileError(Error_NoDir, 'Could not list', path);
  finally
    if lister.filter <> nil then wregexprFree(lister.filter);
    lister.seq.free;
    lister.free
  end;
end;

function list(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  mask: String;
begin
  ignore(context);
  if argc >= 3 then mask := args[2].toString
  else mask := '*';
  result := myList(args[0], true, (argc >= 2) and args[1].toBoolean, mask)
end;

function move(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  source: String;
  dest: String;
begin
  ignore(context);
  source := normalizePath(args[0]);
  dest := normalizePath(args[1]);

  if DirectoryExists(dest) then dest := strAddPathSeparator(dest) + strFileName(source);
  if DirectoryExists(source) then begin
    if FileExistsAsTrueFile(dest) then raiseFileError(Error_Exists, 'Target cannot be overriden', args[1]);
  end else begin
    if not FileOrDirectoryExists(source) then raiseFileError(Error_Not_Found, 'No source', args[0]);
    {$ifdef windows}if FileExistsAsTrueFile(dest) then if not sysutils.DeleteFile(dest) then
      raiseFileError(Error_Io_Error, 'Destination exists', args[1]);
    {$endif}
  end;

  if not RenameFile(source, dest) then raiseFileError(Error_Io_Error, 'Moving failed', args[0]);
  result := xqvalue();
end;


function readFromFile(const fn: String; from: int64 = 0; length: int64 = -1): rawbytestring;
var
  stream: TFileStream;
  errcode: String;
begin
  try
    stream := TFileStream.Create(fn, fmOpenRead);
    try
      if from < 0 then raiseFileError(Error_Out_Of_Range, IntToStr(from) + ' < 0');
      if length = -1 then length := stream.Size - from;
      if length + from > stream.Size then raiseFileError(Error_Out_Of_Range, IntToStr(from)+' + ' +IntToStr(length) + ' > ' + IntToStr(stream.Size));
      SetLength(result, length);
      stream.Position := from;
      if length > 0 then
        stream.ReadBuffer(result[1], length);
    finally
      stream.free;
    end;
  except
    on e: EStreamError do begin
      errcode := Error_Io_Error;
      if DirectoryExists(fn) then errcode := Error_IsDir
      else if not FileOrDirectoryExists(fn) then errcode := Error_Not_Found;
      raiseFileError(errcode, 'Failed to open file for reading', xqvalue(fn));
    end;
    on e: EOutOfMemory do begin //raised for a directory,wtf??
      errcode := Error_Io_Error;
      if DirectoryExists(fn) then errcode := Error_IsDir
      else if not FileOrDirectoryExists(fn) then errcode := Error_Not_Found;
      raiseFileError(errcode, 'Failed to open file for reading', xqvalue(fn));
    end;
  end;
end;



function read_binary(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  from: int64;
  len: int64;
  rangeErr: Boolean;
begin
  ignore(context);
  from := 0;
  len := -1;
  rangeErr := false;
  if argc >= 2 then rangeErr := rangeErr or not xqToUInt64(args[1], from);
  if argc >= 3 then rangeErr := rangeErr or not xqToUInt64(args[2], len);
  if rangeErr then raiseFileError(Error_Out_Of_Range, Error_Out_Of_Range, args[2]);
  result := TXQValueString.create(baseSchema.base64Binary, base64.EncodeStringBase64(readFromFile(normalizePath(args[0]), from, len)));
end;

function read_text(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  data: rawbytestring;
  enc: TSystemCodePage;
begin
  ignore(context);
  data := readFromFile(normalizePath(args[0]));
  if argc = 1 then result := xqvalue(data)
  else begin
    enc := strEncodingFromName(args[1].toString);
    if enc = CP_NONE then raiseFileError(error_unknown_encoding, error_unknown_encoding, args[1]);
    result := xqvalue(strChangeEncoding(data,  enc, CP_UTF8));
  end;
end;

function strFileName({normalized}path: string): string;
var
  lastSep: LongInt;
begin
  lastSep := strlastIndexOf(path, AllowDirectorySeparators);
  if lastsep = length(path) then begin
    system.delete(path, length(path), 1);
    lastSep := strlastIndexOf(path, AllowDirectorySeparators);
  end;
  if lastSep <= 0 then path := path
  else path := strCopyFrom(path, lastSep + 1);
  result := path;
end;

function name({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(strFileName(normalizePath(args[0])));
end;


function resolve_path(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  path: String;
begin
  ignore(context);
  path := fileNameExpand(normalizePath(args[0]));
  result := xqvalue(suffixDirectoy(path));
end;

function parent(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  path: String;
begin
  ignore(context);
  path := strBeforeLast(resolve_path(context,argc, args).toString, AllowDirectorySeparators);
  if path = '' then exit(xqvalue);
  result := xqvalue(suffixDirectoy(path));
end;

function children(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  ignore(context);
  Result := myList(args[0], false, false);
end;

function path_to_native(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  dir: String;
begin
  ignore(context);
  dir := suffixDirectoy(ResolveDots(fileNameExpand(normalizePath(args[0]))));
  if not FileOrDirectoryExists(dir) then raiseFileError(Error_Not_Found, 'Path does not exists: ', args[0]);
  result := xqvalue(dir);
end;

function path_to_uri(const context: TXQEvaluationContext; {%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  path: String;
begin
  ignore(context);
  path := fileNameExpandToURI(normalizePath(args[0]));
  {$ifdef windows}path := StringReplace(path, '\', '/', [rfReplaceAll]);{$endif}
  result := xqvalue(TInternetAccess.urlEncodeData(path, ueXPathFromIRI) );
end;

function dir_separator({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(DirectorySeparator);
end;

function line_separator({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(LineEnding);
end;

function path_separator({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(PathSeparator);
end;

function temp_dir({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := xqvalue(GetTempDir());
end;



procedure registerModuleFile;
begin
  if Assigned(module) then exit;

  module := TXQNativeModule.create(XMLNamespace_Expath_File);
  module.registerFunction('exists', @exists, ['($path as xs:string) as xs:boolean']);
  module.registerFunction('is-dir', @is_dir, ['($path as xs:string) as xs:boolean']);
  module.registerFunction('is-file', @is_file, ['($path as xs:string) as xs:boolean']);
  module.registerFunction('last-modified', @last_modified, ['($path as xs:string) as xs:dateTime']);
  module.registerFunction('size', @size, ['($file as xs:string) as xs:integer']);

  module.registerFunction('append', @append, ['($file as xs:string, $items as item()*) as empty-sequence()', '($file as xs:string, $items as item()*, $params as element(output:serialization-parameters)) as empty-sequence()']);
  module.registerFunction('append-binary', @append_binary, ['($file as xs:string, $value as xs:base64Binary) as empty-sequence()']);
  module.registerFunction('append-text', @append_text, ['($file as xs:string, $value as xs:string) as empty-sequence()','($file as xs:string, $value as xs:string, $encoding as xs:string) as empty-sequence()']);
  module.registerFunction('append-text-lines', @append_text_lines, ['($file as xs:string, $values as xs:string*) as empty-sequence()', '($file as xs:string, $lines as xs:string*, $encoding as xs:string) as empty-sequence()']);
  module.registerFunction('copy', @copy, ['($source as xs:string, $target as xs:string) as empty-sequence()']);
  module.registerFunction('create-dir', @create_dir, ['($dir as xs:string) as empty-sequence()']);
  module.registerFunction('create-temp-dir', @create_temp_dir, ['($prefix as xs:string, $suffix as xs:string) as xs:string', '($prefix as xs:string, $suffix as xs:string, $dir as xs:string) as xs:string']);
  module.registerFunction('create-temp-file', @create_temp_file, ['($prefix as xs:string, $suffix as xs:string) as xs:string', '($prefix as xs:string, $suffix as xs:string, $dir as xs:string) as xs:string']);
  module.registerFunction('delete', @delete, ['($path as xs:string) as empty-sequence()', '($path as xs:string, $recursive as xs:boolean) as empty-sequence()']);
  module.registerFunction('list', @list, ['($dir as xs:string) as xs:string*', '($dir as xs:string, $recursive as xs:boolean) as xs:string*', '($dir as xs:string, $recursive as xs:boolean, $pattern as xs:string) as xs:string*']);
  module.registerFunction('move', @move, ['($source as xs:string, $target as xs:string) as empty-sequence()']);
  module.registerFunction('read-binary', @read_binary, ['($file as xs:string) as xs:base64Binary', '($file as xs:string, $offset as xs:integer) as xs:base64Binary', '($file as xs:string, $offset as xs:integer, $length as xs:integer) as xs:base64Binary']);
  module.registerFunction('read-text', @read_text, ['($file as xs:string) as xs:string', '($file as xs:string, $encoding as xs:string) as xs:string']);
  module.registerInterpretedFunction('read-text-lines', '($file as xs:string) as xs:string*',                          'x:lines(file:read-text($file           ))');
  module.registerInterpretedFunction('read-text-lines', '($file as xs:string, $encoding as xs:string) as xs:string*',  'x:lines(file:read-text($file, $encoding))');
  module.registerFunction('write', @write, ['($file as xs:string, $items as item()*) as empty-sequence()', '($file as xs:string, $items as item()*, $params as element(Q{http://www.w3.org/2010/xslt-xquery-serialization}serialization-parameters)) as empty-sequence()']);
  module.registerFunction('write-binary', @write_binary, ['($file as xs:string, $value as xs:base64Binary) as empty-sequence()', '($file as xs:string, $value as xs:base64Binary, $offset as xs:integer) as empty-sequence()']);
  module.registerFunction('write-text', @write_text, ['($file as xs:string, $value as xs:string) as empty-sequence()', '($file as xs:string, $value as xs:string, $encoding as xs:string) as empty-sequence()']);
  module.registerFunction('write-text-lines', @write_text_lines, ['($file as xs:string, $values as xs:string*) as empty-sequence()', '($file as xs:string, $values as xs:string*, $encoding as xs:string) as empty-sequence()']);

  module.registerFunction('name', @name, ['($path as xs:string) as xs:string']);
  module.registerFunction('parent', @parent, ['($path as xs:string) as xs:string?']);
  module.registerFunction('path-to-native', @path_to_native, ['($path as xs:string) as xs:string']);
  module.registerFunction('children', @children, ['($path as xs:string) as xs:string*']);
  module.registerFunction('path-to-uri', @path_to_uri, ['($path as xs:string) as xs:anyURI']);
  module.registerFunction('resolve-path', @resolve_path, ['($path as xs:string) as xs:string']);

  module.registerFunction('dir-separator', @dir_separator, ['() as xs:string']);
  module.registerFunction('line-separator', @line_separator, ['() as xs:string']);
  module.registerFunction('path-separator', @path_separator, ['() as xs:string']);
  module.registerFunction('temp-dir', @temp_dir, ['() as xs:string']);
  module.registerInterpretedFunction('base-dir', '() as xs:string', 'Q{http://expath.org/ns/file}parent(static-base-uri())');
  module.registerInterpretedFunction('current-dir', '() as xs:string', 'Q{http://expath.org/ns/file}resolve-path(".")');


  TXQueryEngine.registerNativeModule(module);
end;



initialization
  XMLNamespace_Expath_File := TNamespace.make('http://expath.org/ns/file', 'file');

finalization
  module.free;

end.

