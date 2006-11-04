unit uStrUtils;

interface

uses
  Classes, SysUtils;

type
  //only result = true to continue.
  TOnDoFileEvent = function (const aFileName: string): Boolean;

const
  {$IFDEF MSWINDOWS}
  DriveLetters     = ['a'..'z', 'A'..'Z'];
  PathDevicePrefix = '\\.\';
  PathUncPrefix    = '\\';
  {$ENDIF MSWINDOWS}
  AnsiDecDigits              = ['0'..'9'];


{可以递归调用处理子目录以及旗下的文件}
function ProcessFolder(const aFolder, aPatterns: string; OnFile: TOnDoFileEvent): Boolean;
{
only process the files in the aPath
  @param aPatterns  the search patterns: *.dpr;*.dfm
}
function ProcessFiles(const aPath:string; aPatterns: string; OnFile: TOnDoFileEvent): Boolean;

function ExtractFileBaseName(const aFileName: string): string;

//come from JCL
function PathIsDiskDevice(const Path: string): Boolean;
function PathIsUNC(const Path: string): Boolean;
function PathIsAbsolute(const Path: string): Boolean;

implementation

function ExtractFileBaseName(const aFileName: string): string;
var
  i: integer;
begin
  i := AnsiPos('.', aFileName);
  if i <= 0 then i := Length(aFileName);
  Result := Copy(aFileName, 1, i-1);
end;

function PathIsDiskDevice(const Path: string): Boolean;
{$IFDEF UNIX}
var
  FullPath: string;
  F: PIOFile;
  Buffer: array [0..255] of Char;
  MountEntry: TMountEntry;
  FsTypes: TStringList;

  procedure GetAvailableFileSystems(const List: TStrings);
  var
    F: TextFile;
    S: string;
  begin
    AssignFile(F, '/proc/filesystems');
    Reset(F);
    repeat
      Readln(F, S);
      if Pos('nodev', S) = 0 then // how portable is this ?
        List.Add(Trim(S));
    until Eof(F);
    List.Add('supermount');
    CloseFile(F);
  end;

begin
  Result := False;

  SetLength(FullPath, _POSIX_PATH_MAX);
  if realpath(PChar(Path), PChar(FullPath)) = nil then
    RaiseLastOSError;
  StrResetLength(FullPath);
  
  FsTypes := TStringList.Create;
  try
    GetAvailableFileSystems(FsTypes);
    F := setmntent(_PATH_MOUNTED, 'r'); // PATH_MOUNTED is deprecated,
                                        // but PATH_MNTTAB is defective in Libc.pas
    try
      // get drives from mtab
      while not Result and (getmntent_r(F, MountEntry, Buffer, SizeOf(Buffer)) <> nil) do
        if FsTypes.IndexOf(MountEntry.mnt_type) <> -1 then
          Result := MountEntry.mnt_dir = FullPath;

    finally
      endmntent(F);
    end;
  finally
    FsTypes.Free;
  end;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
begin
  Result := Copy(Path, 1, Length(PathDevicePrefix)) = PathDevicePrefix;
end;
{$ENDIF MSWINDOWS}

function PathIsUNC(const Path: string): Boolean;

{$IFDEF MSWINDOWS}

const
  cUNCSuffix = '?\UNC';

var
  {$IFDEF CLR}
  Index, LenPath: Integer;
  {$ELSE}
  P: PChar;
  {$ENDIF}

  function AbsorbSeparator: Boolean;
  begin
    {$IFDEF CLR}
    Result := (Index <> 0) and (Path[Index] = PathDelim);
    if Result then
      Inc(Index);
    {$ELSE ~CLR}
    Result := (P <> nil) and (P^ = PathDelim);
    if Result then
      Inc(P);
    {$ENDIF ~CLR}
  end;

  function AbsorbMachineName: Boolean;
  var
    NonDigitFound: Boolean;
  begin
    // a valid machine name is a string composed of the set [a-z, A-Z, 0-9, -, _] but it may not
    // consist entirely out of numbers
    Result := True;
    NonDigitFound := False;
    {$IFDEF CLR}
    while (Index <= LenPath) and (Path[Index] <> PathDelim) do
    begin
      if AnsiChar(Path[Index]) in ['a'..'z', 'A'..'Z', '-', '_', '.'] then
      begin
        NonDigitFound := True;
        Inc(Index);
      end
      else
      if AnsiChar(Path[Index]) in AnsiDecDigits then
        Inc(Index)
      else
      begin
        Result := False;
        Break;
      end;
    end;
    {$ELSE ~CLR}
    while (P^ <> #0) and (P^ <> PathDelim) do
    begin
      if P^ in ['a'..'z', 'A'..'Z', '-', '_', '.'] then
      begin
        NonDigitFound := True;
        Inc(P);
      end
      else
      if P^ in AnsiDecDigits then
        Inc(P)
      else
      begin
        Result := False;
        Break;
      end;
    end;
    {$ENDIF ~CLR}
    Result := Result and NonDigitFound;
  end;

  function AbsorbShareName: Boolean;
  const
    InvalidCharacters =
      ['<', '>', '?', '/', ',', '*', '+', '=', '[', ']', '|', ':', ';', '"', '''']; //'
  begin
    // a valid share name is a string composed of a set the set !InvalidCharacters note that a
    // leading '$' is valid (indicates a hidden share)
    Result := True;
    {$IFDEF CLR}
    while (Index <= LenPath) and (Path[Index] <> '\') do
    begin
      if AnsiChar(Path[Index]) in InvalidCharacters then
      begin
        Result := False;
        Break;
      end;
      Inc(Index);
    end;
    {$ELSE ~CLR}
    while (P^ <> #0) and (P^ <> '\') do
    begin
      if P^ in InvalidCharacters then
      begin
        Result := False;
        Break;
      end;
      Inc(P);
    end;
    {$ENDIF ~CLR}
  end;

begin
  Result := Copy(Path, 1, Length(PathUncPrefix)) = PathUncPrefix;
  if Result then
  begin
    {$IFDEF CLR}
    Index := Length(PathUncPrefix);
    if Path.StartsWith(PathUncPrefix + cUNCSuffix) then
      Inc(Index, Length(cUNCSuffix))
    else
      Result := AbsorbSeparator and AbsorbMachineName;
    {$ELSE ~CLR}
    if Copy(Path, 1, Length(PathUncPrefix + cUNCSuffix)) = PathUncPrefix + cUNCSuffix then
      P := @Path[Length(PathUncPrefix + cUNCSuffix)]
    else
    begin
      P := @Path[Length(PathUncPrefix)];
      Result := AbsorbSeparator and AbsorbMachineName;
    end;
    {$ENDIF ~CLR}
    Result := Result and AbsorbSeparator;
    if Result then
    begin
      Result := AbsorbShareName;
      // remaining, if anything, is path and or filename (optional) check those?
    end;
  end;
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}

begin
  Result := False;
end;

{$ENDIF UNIX}

function PathIsAbsolute(const Path: string): Boolean;
{$IFDEF CLR}
begin
  Result := System.IO.Path.IsPathRooted(Path);
end;
{$ELSE ~CLR}
{$IFDEF MSWINDOWS}
var
  I: Integer;
{$ENDIF MSWINDOWS}
begin
  Result := False;
  if Path <> '' then
  begin
    {$IFDEF UNIX}
    Result := (Path[1] = PathDelim);
    {$ENDIF UNIX}
    {$IFDEF MSWINDOWS}
    if not PathIsUnc(Path) then
    begin
      I := 0;
      if PathIsDiskDevice(Path) then
        I := Length(PathDevicePrefix);
      Result := (Length(Path) > I + 2) and (Path[I + 1] in DriveLetters) and
        (Path[I + 2] = ':') and (Path[I + 3] = PathDelim);
    end
    else
      Result := True;
    {$ENDIF MSWINDOWS}
  end;
end;
{$ENDIF ~CLR}

function ProcessFiles(const aPath:string; aPatterns: string; OnFile: TOnDoFileEvent): Boolean;
var 
  p: integer;
  SrchRec: TSearchRec;
  Pattern: string;
begin
  //WriteLn('ProcessFiles ' + aPath);
  Result := True;
  //first process files
  while aPatterns <> '' do
  begin
    P := Pos(';', aPatterns);
    if P > 0 then
    begin
      Pattern := Copy(aPatterns, 1, p-1);
      Delete(aPatterns, 1, p);
    end
    else begin
      Pattern := aPatterns;
      aPatterns := '';
    end;
    if FindFirst(aPath + Pattern, faAnyFile - faDirectory, SrchRec) = 0 then
    try
      Repeat
        //iStream := TFileStream.Create(SrchRec.Name, fmOpenRead);
        //WriteLn('ProcessFile ' + aPath + SrchRec.Name);
        if Assigned(OnFile) then Result := OnFile(aPath + SrchRec.Name);
        if not Result then break;
        //ProcessAFile(SrchRec.Name);
      until FindNext(SrchRec) <> 0;
    finally
      FindClose(SrchRec);
    end;//try
  end; //while
end;

//the aFolder must IncludeTrailingPathDelimiter!!
function ProcessFolder(const aFolder, aPatterns: string; OnFile: TOnDoFileEvent): Boolean;
var
  DirSrchRec: TSearchRec;
begin
  Result := True;
  //aFolder := IncludeTrailingPathDelimiter(aFolder);
  if FindFirst(aFolder + '*.*', faDirectory, DirSrchRec) = 0 then
  try
    Repeat
      if (DirSrchRec.Name <> '.') and (DirSrchRec.Name <> '..') 
      and ((DirSrchRec.Attr and faDirectory) = faDirectory) then
      begin
        //WriteLn('Enter ' + aFolder + DirSrchRec.Name);
        //ChDir(DirSrchRec.Name);
        Result := ProcessFolder(aFolder + DirSrchRec.Name + PathDelim, aPatterns, OnFile);
        //if not Result then WriteLn('break on ' + aFolder);
        if not Result then Break;
        //ChDir('..');
      end;
    until FindNext(DirSrchRec) <> 0;
  finally
    FindClose(DirSrchRec);
  end;//try
  if Result then
    Result := ProcessFiles(aFolder, aPatterns, OnFile);
end;

end.