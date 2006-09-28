unit uTurboModuleFileAccessor;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uTurboScriptConsts
  , uTurboExecutor 
  , uTurboScriptAccessor
  ;

type
  TTurboModuleFileAccessor = class(TTurboModuleAccessor)
  private
    function GetIncludeDirs: string;
    procedure SetIncludeDirs(const Value: string);
  protected
    FIncludeDirs: TStringList;
    {: trim invalid includes paths. }
    procedure TrimInvalidPaths;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModuleStream(const aModuleName: String; const aStream:
            TStream): Boolean; override;
    function Require(const aModuleName: String; const IsLoaded: Boolean):
            TCustomTurboExecutor; override;
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

function TTurboModuleFileAccessor.LoadModuleStream(const aModuleName: String;
        const aStream: TStream): Boolean;
begin
end;

function TTurboModuleFileAccessor.Require(const aModuleName: String; const
        IsLoaded: Boolean): TCustomTurboExecutor;
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
