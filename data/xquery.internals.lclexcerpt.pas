//copied from Lazarus/the LCL to reduce dependancies
unit xquery.internals.lclexcerpt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ResolveDots(const AFilename: string): string;

type
  TCopyFileFlag = (
    cffOverwriteFile,
    cffCreateDestDirectory,
    cffPreserveTime
    );
  TCopyFileFlags = set of TCopyFileFlag;
function CopyFile(const SrcFilename, DestFilename: String;
                  Flags: TCopyFileFlags=[cffOverwriteFile]; ExceptionOnError: Boolean=False): Boolean;



//not copied from the lcl, but based on it

function strAddPathSeparator(path: string): string;

type TFileLister = object
  recurse: boolean;
  constructor init;
  procedure foundSomething(const dir, current: String; const search: TRawByteSearchRec); virtual;
  procedure startSearch(path: string; prefixForOutput: string = ''); virtual;
end;



implementation

uses   {$ifdef unix}BaseUnix{$endif}
  {$ifdef windows}windows{$endif}
    ;


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
var SrcPos, DestPos, l, DirStart: SizeInt;
  c: char;
  MacroPos: SizeInt;
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
















constructor TFileLister.init;
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





function strAddPathSeparator(path: string): string;
begin
  if path = '' then path := '.' + DirectorySeparator;
  if not (path[length(path)] in AllowDirectorySeparators) then path += DirectorySeparator;
  result := path;
end;


end.

