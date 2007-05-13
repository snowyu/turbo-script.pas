{: 模块装入保存机制 }
{ Description
实际的装载、卸载发生在这里，管理从文件或数据库加载模块，模块的唯一性。
}
unit uTurboAccessor;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uTurboConsts
  , uTurboExecutor 
  ;

type
  TTurboAccessorClass = class of TTurboAccessor; 
  TTurboModuleAccessorClass = class of TTurboModuleAccessor;
 
  TTurboAccessor = class;
  {: the pure abstract accessor class: the module accessor and ModuleManager
          are derived from it. }
  { Description
  }
  TTurboAccessor = class(TCustomTurboObject)
  protected
    {: if find then create and return the module Executor else return nil. }
    { Description
    @param IsLoaded whether load the module body to memory.
     
    if IsLoaded then means Delphi "uses unit".
    }
    function iRequire(const aModuleName: string; const aModuleClass:
            TTurboModuleClass; const IsLoaded: Boolean): TCustomTurboModule;
            virtual; abstract;
  public
    {: load the module to the aStream }
    function SaveModule(const aModule: TCustomTurboModule): Boolean; virtual;
            abstract;
  end;

  {: Abstract TurboModule Accessor[singleton] }
  TTurboModuleAccessor = class(TTurboAccessor)
  public
    {: if found then create the Stream and load the module to the Stream else
            return nil. }
    function GetModuleStream(const aModuleName: String): TStream; virtual;
            abstract;
    {: Load the Module body into Module.Memory. }
    procedure LoadModule(const aModule: TCustomTurboModule);
    function Require(const aModuleName: string; const aModuleClass:
            TTurboModuleClass; const IsLoaded: Boolean): TCustomTurboModule;
  end;

  {: The TurboScript Module Manager }
  { Description
  the GTurboModuleManager global 
  variable manages all the loaded 
  modules. And register the new 
  ModuleAccessor to GTurboModuleManager.

  Require method return the 
  specified module. It first 
  searches in the memory if 
  not found, try load from 
  the module through registered
  module accessores.
  }
  TTurboModuleManager = class(TTurboAccessor)
  private
    FDefaultAccessor: TTurboModuleAccessor;
    FModules: TList;
    function GetItems(Index: Integer): TCustomTurboModule;
  protected
    procedure DoBeforeTheModuleFree(Sender: TObject);
    function iRequire(const aModuleName: string; const aModuleClass:
            TTurboModuleClass; const IsLoaded: Boolean): TCustomTurboModule;
            override;
    property Modules: TList read FModules;
  public
    constructor Create;
    destructor Destroy; override;
    {: Inserts a new module at the end of the Modules list. if already exists
            then return -1 else return the index. }
    function Add(const aModule: TCustomTurboModule): Integer;
    {: Clear and Free all Modules. }
    procedure Clear;
    {: Return the Module Count }
    function Count: Integer;
    {: Returns the index of the first occurrence in the list of a specified
            module name. }
    function IndexOf(const aModuleName: String): Integer;
    function RegisterAccessor(const AccessorClass: TTurboModuleAccessorClass;
            const IsDefault: Boolean = False): Boolean;
    {: if find then create and return the module Executor else return nil. }
    { Description
    @param IsLoaded whether load the module body to memory.
     
    if IsLoaded then means Delphi "uses unit".

    }
    function Require(const aModuleName: String; const aModuleClass:
            TTurboModuleClass; const IsLoaded: Boolean): TCustomTurboModule;
    function SaveModule(const aModule: TCustomTurboModule): Boolean; override;
    {: if the module is no accessor, use this instead. }
    property DefaultAccessor: TTurboModuleAccessor read FDefaultAccessor write
            FDefaultAccessor;
    property Items[Index: Integer]: TCustomTurboModule read GetItems; default;
  end;

  TTurboModuleAccessorList = class(TList)
  private
    function GetItems(Index: Integer): TTurboModuleAccessor;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    destructor Destroy; override;
    function IndexOf(AccessorClass: TTurboModuleAccessorClass): Integer;
    property Items[Index: Integer]: TTurboModuleAccessor read GetItems; default;
  end;


function GTurboModuleManager: TTurboModuleManager;
function GTurboModuleAccessors: TTurboModuleAccessorList;

implementation

var
  FTurboModuleAccessors: TTurboModuleAccessorList;
  FTurboModuleMgr: TTurboModuleManager;
  

{
***************************** TTurboModuleAccessor *****************************
}
procedure TTurboModuleAccessor.LoadModule(const aModule: TCustomTurboModule);
var
  vStream: TStream;
begin
  vStream := GetModuleStream(aModule.Name);
  if Assigned(vStream) then
  try
      aModule.LoadFromStream(vStream);
  finally
    vStream.Free;
  end;
end;

function TTurboModuleAccessor.Require(const aModuleName: string; const
        aModuleClass: TTurboModuleClass; const IsLoaded: Boolean):
        TCustomTurboModule;
begin
  Result := iRequire(aModuleName, aModuleClass, IsLoaded);
  if Assigned(Result) then
    Result.Accessor := Self;
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

function TTurboModuleManager.Add(const aModule: TCustomTurboModule): Integer;
begin
  Result := IndexOf(aModule.Name);
  if Result = -1 then
  begin
    Result := FModules.Add(aModule);
    if Result >= 0 then
      aModule.FreeNotification(DoBeforeTheModuleFree);
  end
  else //already exists.
    Result := -1;
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

function TTurboModuleManager.IndexOf(const aModuleName: String): Integer;
begin
  for Result := 0 to FModules.Count - 1 do
  begin
    if TCustomTurboModule(FModules[Result]).Name = aModuleName then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TTurboModuleManager.iRequire(const aModuleName: string; const
        aModuleClass: TTurboModuleClass; const IsLoaded: Boolean):
        TCustomTurboModule;
var
  I: Integer;
begin
  For i := 0 to FModules.Count - 1 do
  begin
    Result := TCustomTurboModule(FModules[i]);
    if Result.Name = aModuleName then
    begin
      if IsLoaded and not Result.IsLoaded then
        Result.Load;
      Exit;
    end;
  end;

  for i := 0 to GTurboModuleAccessors.Count - 1 do
  begin
    Result := TTurboModuleAccessor(FTurboModuleAccessors[i]).Require(aModuleName, aModuleClass, IsLoaded);
    if Result <> nil then
    begin
      FModules.Add(Result);
      Result.FreeNotification(DoBeforeTheModuleFree);
      Exit;
    end;
  end;
  Result := nil;
end;

function TTurboModuleManager.RegisterAccessor(const AccessorClass:
        TTurboModuleAccessorClass; const IsDefault: Boolean = False): Boolean;
var
  I: Integer;
  vAccessor: TTurboModuleAccessor;
begin
  Result := False;
  if Assigned(AccessorClass) then
  begin
    I := GTurboModuleAccessors.IndexOf(AccessorClass);
    if I < 0 then
    begin
      vAccessor := AccessorClass.Create;
      try
      FTurboModuleAccessors.Add(vAccessor);
      except
        FreeAndNil(vAccessor);
        raise;
      end;
    end
    else
      vAccessor := FTurboModuleAccessors[I];
    if IsDefault then DefaultAccessor := vAccessor;
    Result := True;
  end;
end;

function TTurboModuleManager.Require(const aModuleName: String; const
        aModuleClass: TTurboModuleClass; const IsLoaded: Boolean):
        TCustomTurboModule;
begin
  Result := iRequire(aModuleName, aModuleClass, IsLoaded);
end;

function TTurboModuleManager.SaveModule(const aModule: TCustomTurboModule):
        Boolean;
begin
  Result := False;
  if aModule.IsLoaded then
  begin
    if not Assigned(aModule.Accessor) then
      aModule.Accessor := DefaultAccessor;
    if Assigned(aModule.Accessor) then
      Result := DefaultAccessor.SaveModule(aModule);
  end;
end;

{
*************************** TTurboModuleAccessorList ***************************
}
destructor TTurboModuleAccessorList.Destroy;
begin
  FTurboModuleAccessors := nil;
  inherited Destroy;
end;

function TTurboModuleAccessorList.GetItems(Index: Integer):
        TTurboModuleAccessor;
begin
  Result := TTurboModuleAccessor(inherited Get(Index));
end;

function TTurboModuleAccessorList.IndexOf(AccessorClass:
        TTurboModuleAccessorClass): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if AccessorClass = TTurboModuleAccessor(Items[Result]).ClassType then
      Exit;
  end;
  Result := -1;
end;

procedure TTurboModuleAccessorList.Notify(Ptr: Pointer; Action:
        TListNotification);
begin
  if (Action = lnDeleted) and (TObject(Ptr) is TObject) then
  begin
    TObject(Ptr).Free;
    Ptr := nil;
  end;
end;



function GTurboModuleManager: TTurboModuleManager;
begin
  if FTurboModuleMgr = nil then
  begin
    FTurboModuleMgr := TTurboModuleManager.Create;
  end;
  Result := FTurboModuleMgr;
end;

function GTurboModuleAccessors: TTurboModuleAccessorList;
begin
  if FTurboModuleAccessors = nil then
  begin
    FTurboModuleAccessors := TTurboModuleAccessorList.Create;
  end;
  Result := FTurboModuleAccessors;
end;

initialization
  FTurboModuleAccessors := TTurboModuleAccessorList.Create;
finalization
  FreeAndNil(FTurboModuleMgr);
  FreeAndNil(FTurboModuleAccessors);
end.
