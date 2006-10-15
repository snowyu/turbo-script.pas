unit uTurboModuleFileAccessor;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
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
    procedure GetModuleFileName(const aModuleName: String);
    function iRequire(const aModuleName: String; const IsLoaded: Boolean):
            TCustomTurboModule; override;
    {: trim invalid includes paths. }
    procedure TrimInvalidPaths;
  public
    constructor Create;
    destructor Destroy; override;
    function GetModuleStream(const aModuleName: String): TStream; override;
    function SaveModule(const aModule: TCustomTurboModule): Boolean; override;
    property IncludeDirs: string read GetIncludeDirs write SetIncludeDirs;
  end;


implementation

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

function TTurboModuleFileAccessor.GetIncludeDirs: string;
begin
  Result := FIncludeDirs.DelimitedText;
end;

procedure TTurboModuleFileAccessor.GetModuleFileName(const aModuleName: String);
begin
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

function TTurboModuleFileAccessor.iRequire(const aModuleName: String; const
        IsLoaded: Boolean): TCustomTurboModule;
begin
end;

function TTurboModuleFileAccessor.SaveModule(const aModule:
        TCustomTurboModule): Boolean;
begin
end;

procedure TTurboModuleFileAccessor.SetIncludeDirs(const Value: string);
begin
  FIncludeDirs.DelimitedText := Value;
  TrimInvalidPaths;
end;

procedure TTurboModuleFileAccessor.TrimInvalidPaths;
begin
end;



end.
