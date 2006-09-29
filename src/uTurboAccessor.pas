unit uTurboScriptAccessor;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uTurboScriptConsts
  , uTurboExecutor 
  ;

type
  TTurboScriptAccessorClass = class of TTurboScriptAccessor; 
  TTurboModuleAccessorClass = class of TTurboModuleAccessor;
 
  TTurboScriptAccessor = class;
  {: 模块装入保存机制 }
  { Description
  实际的装载、卸载发生在这里，管理从文件或数据库加载模块，模块的唯一性。
  }
  TTurboScriptAccessor = class(TCustomTurboObject)
  public
    {: if find then create and return the module Executor else return nil. }
    { Description
    @param IsLoaded whether load the module body to memory.
     
    if IsLoaded then means Delphi "uses unit".
    }
    function Require(const aModuleName: String; const IsLoaded: Boolean):
            TCustomTurboModule; virtual; abstract;
    {: load the module to the aStream }
    function SaveModule(const aModule: TCustomTurboModule): Boolean; virtual;
            abstract;
  end;

  TTurboModuleAccessor = class(TTurboScriptAccessor)
  public
    {: Load the Module body into Module.Memory. }
    procedure LoadModule(const aModule: TCustomTurboModule);
    {: load the module to the aStream }
    function LoadModuleStream(const aModuleName: String; const aStream:
            TStream): Boolean; virtual; abstract;
  end;

  TTurboModuleManager = class(TTurboScriptAccessor)
  private
    FModules: TList;
    function GetItems(Index: Integer): TCustomTurboModule;
  protected
    procedure DoBeforeTheModuleFree(Sender: TObject);
    property Modules: TList read FModules;
  public
    constructor Create;
    destructor Destroy; override;
    {: Clear and Free all Modules. }
    procedure Clear;
    {: Return the Module Count }
    function Count: Integer;
    function Require(const aModuleName: String; const IsLoaded: Boolean):
            TCustomTurboModule; override;
    function SaveModule(const aModule: TCustomTurboModule): Boolean; override;
    property Items[Index: Integer]: TCustomTurboModule read GetItems; default;
  end;

  TTurboModuleAccessorList = class(TList)
  public
    destructor Destroy; override;
  end;


function GTurboModuleAccessorClasses: TTurboModuleAccessorList;
function DefaultTurboModuleAccessor: TTurboModuleAccessor;

procedure RegisterModuleAccessor(const Accessor: TTurboModuleAccessor; const
        IsDefault: Boolean = False);

implementation

var
  FTurboModuleAccessorClasses: TTurboModuleAccessorList;
  FDefaultModuleAccessor: TTurboModuleAccessor;
  
function DefaultTurboModuleAccessor: TTurboModuleAccessor;
begin
  Result := FDefaultModuleAccessor;
end;

function GTurboModuleAccessorClasses: TTurboModuleAccessorList;
begin
  if FTurboModuleAccessorClasses = nil then
  begin
    FDefaultModuleAccessor := nil;
    FTurboModuleAccessorClasses := TTurboModuleAccessorList.Create;
  end;
  Result := FTurboModuleAccessorClasses;
end;

procedure RegisterModuleAccessor(const Accessor: TTurboModuleAccessor; const
        IsDefault: Boolean);
var
  i: integer;
begin
  i := GTurboModuleAccessorClasses.IndexOf(Accessor);
  if i < 0 then
  begin
    FTurboModuleAccessorClasses.Add(Accessor);
    if IsDefault then FDefaultModuleAccessor := Accessor;
  end;
end;

{
***************************** TTurboModuleAccessor *****************************
}
procedure TTurboModuleAccessor.LoadModule(const aModule: TCustomTurboModule);
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    if LoadModuleStream(aModule.Name, vStream) then
    begin
      aModule.LoadFromStream(vStream);
    end;
  finally
    vStream.Free;
  end;
end;

{
***************************** TTurboModuleManager ******************************
}
constructor TTurboModuleManager.Create;
begin
  inherited Create;
  FModules := TList.Create;
end;

destructor TTurboModuleManager.Destroy;
begin
  Clear;
  FreeAndNil(FModules);
  inherited Destroy;
end;

procedure TTurboModuleManager.Clear;
begin
  while FModules.Count > 0 do
    TCustomTurboModule(FModules[0]).Free;
end;

function TTurboModuleManager.Count: Integer;
begin
  Result := FModules.Count;
end;

procedure TTurboModuleManager.DoBeforeTheModuleFree(Sender: TObject);
begin
  FModules.Remove(Sender);
end;

function TTurboModuleManager.GetItems(Index: Integer): TCustomTurboModule;
begin
  Result := TCustomTurboModule(FModules[Index]);
end;

function TTurboModuleManager.Require(const aModuleName: String; const IsLoaded:
        Boolean): TCustomTurboModule;
var
  I: Integer;
begin
  For i := 0 to FModules.Count - 1 do
  begin
    Result := TCustomTurboModule(FModules[i]);
    if Result.Name = aMoudleName then
    begin
      if IsLoaded and not Result.IsLoaded then
        Result.Load;
      Exit;
    end;
  end;

  for i := 0 to GTurboModuleAccessorClasses.Count - 1 do
  begin
    Result := TTurboScriptAccessor(FTurboModuleAccessorClasses[i]).Require(aModuleName, IsLoaded);
    if Result <> nil then
    begin
      FModules.Add(Result);
      Result.FreeNotification(DoBeforeTheModuleFree);
      Exit;
    end;
  end;
  Result := nil;
end;

function TTurboModuleManager.SaveModule(const aModule: TCustomTurboModule):
        Boolean;
begin
  Result := DefaultTurboModuleAccessor.SaveModule(aModule);
end;

{
*************************** TTurboModuleAccessorList ***************************
}
destructor TTurboModuleAccessorList.Destroy;
begin
  FTurboModuleAccessorClasses := nil;
  inherited Destroy;
end;


initialization
  FTurboModuleAccessorClasses := TTurboModuleAccessorList.Create;
finalization
  FreeAndNil(FTurboModuleAccessorClasses);
end.
