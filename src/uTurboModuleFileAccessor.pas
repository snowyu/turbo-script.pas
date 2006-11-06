unit uTurboModuleFileAccessor;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uStrUtils
  , uTurboConsts
  , uTurboExecutor 
  , uTurboAccessor
  ;

type
  TTurboModuleFileAccessor = class(TTurboModuleAccessor)
  private
    function GetIncludeDirs: string;
    procedure SetIncludeDirs(const Value: string);
  protected
    FIncludeDirs: TStringList;
    function GetModuleFileName(aModuleName: String): string;
    function iRequire(const aModuleName: string; const aModuleClass:
            TTurboModuleClass; const aGlobal: PTurboGlobalOptions; const
            IsLoaded: Boolean): TCustomTurboModule; override;
    {: trim invalid includes paths. }
    procedure TrimInvalidPaths;
  public
    constructor Create;
    destructor Destroy; override;
    function AddIncludeDir(const aDir: string): Integer;
    function GetModuleStream(const aModuleName: String): TStream; override;
    function RemoveIncludeDir(const aDir: string): Integer;
    function SaveModule(const aModule: TCustomTurboModule): Boolean; override;
    property IncludeDirs: string read GetIncludeDirs write SetIncludeDirs;
  end;


{: translate the full module name to compiled module file name.}
function ModuleNameToFileName(const aModuleName: string): string;

implementation

function ModuleNameToFileName(const aModuleName: string): string;
begin
  Result := aModuleName + cTurboCompiledUnitFileExt;
end;

{
*************************** TTurboModuleFileAccessor ***************************
}
constructor TTurboModuleFileAccessor.Create;
begin
  inherited Create;
  FIncludeDirs := TStringList.Create;
  with FIncludeDirs do
  begin
    Delimiter := ';';
    Duplicates := dupIgnore;
  end;
end;

destructor TTurboModuleFileAccessor.Destroy;
begin
  FreeAndNil(FIncludeDirs);
  inherited Destroy;
end;

function TTurboModuleFileAccessor.AddIncludeDir(const aDir: string): Integer;
begin
  Result := -1;
  if aDir <> '' then
  begin
    if DirectoryExists(ExpandFileName(aDir)) then
      Result := FIncludeDirs.Add(aDir);
  end;
end;

function TTurboModuleFileAccessor.GetIncludeDirs: string;
begin
  Result := FIncludeDirs.DelimitedText;
end;

function TTurboModuleFileAccessor.GetModuleFileName(aModuleName: String):
        string;
var
  I: Integer;
  vFolder: string;
begin
  aModuleName := ModuleNameToFileName(aModuleName);
  Result := ExpandFileName(aModuleName);
  if FileExists(Result) then
  begin
    Exit;
  end;

  for I := 0 to FIncludeDirs.Count - 1 do
  begin
    vFolder := IncludeTrailingPathDelimiter(FIncludeDirs[i]);
    if vFolder <> '' then
    begin
      Result := ExpandFileName(vFolder + aModuleName);
      if FileExists(Result) then Exit;
    end;
  end;
  Result := '';
end;

function TTurboModuleFileAccessor.GetModuleStream(const aModuleName: String):
        TStream;
var
  vFileName: string;
begin
  Result := nil;

  vFileName := GetModuleFileName(aModuleName);
  if vFileName <> '' then
  begin
    Result := TFileStream.Create(vFileName, fmOpenRead);
  end;
end;

function TTurboModuleFileAccessor.iRequire(const aModuleName: string; const
        aModuleClass: TTurboModuleClass; const aGlobal: PTurboGlobalOptions;
        const IsLoaded: Boolean): TCustomTurboModule;
var
  vStream: TStream;
  vFileName: string;
begin
  Result := nil;
  vFileName := GetModuleFileName(aModuleName);
  if vFileName <> '' then
  try
    Result := aModuleClass.Create;
    Result.Name := aModuleName;
    Result.GlobalOptions := aGlobal;
    if IsLoaded then
    begin
      vStream := TFileStream.Create(vFileName, fmOpenRead);
      if Assigned(vStream) then
      try
        Result.LoadFromStream(vStream);
      finally
        vStream.Free;
      end;
    end;
  except
    FreeAndNil(Result);
  end;
end;

function TTurboModuleFileAccessor.RemoveIncludeDir(const aDir: string): Integer;
begin
  Result := FIncludeDirs.IndexOf(aDir);
  if Result <> -1 then
    FIncludeDirs.Delete(Result);
end;

function TTurboModuleFileAccessor.SaveModule(const aModule:
        TCustomTurboModule): Boolean;
begin
  with aModule do SaveToFile(ModuleNameToFileName(Name));
  Result := True;
end;

procedure TTurboModuleFileAccessor.SetIncludeDirs(const Value: string);
begin
  FIncludeDirs.DelimitedText := Value;
  TrimInvalidPaths;
end;

procedure TTurboModuleFileAccessor.TrimInvalidPaths;
var
  I: Integer;
  s: string;
begin
  for I := FIncludeDirs.Count -1  downto 0 do
  begin
    s := FIncludeDirs[I];
    if (s = '') or not DirectoryExists(ExpandFileName(s)) then
      FIncludeDirs.Delete(I);
  end;
end;


initialization
  GTurboModuleManager.RegisterAccessor(TTurboModuleFileAccessor, True);
end.
