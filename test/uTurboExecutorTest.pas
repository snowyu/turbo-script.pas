{Turbo Script Executor automatic Test}
unit uTurboExecutorTest;

{$I Setting.INC}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, //QueryPerformanceCounter
  {$ENDIF}
  {$IFDEF DEBUG}
  DbugIntf,
  {$ENDIF}
  Classes,
  SysUtils,
  Math,
  TypInfo,
  TestFramework
  , uTurboScriptConsts
  , uTurboExecutor
  ;


type
  { abstract }
  TTest_TurboExecutor = class (TTestCase)
  protected
    FApp: TTurboAppDomain;
  protected
    class function GetExecutorClass: TCustomTurboExecutor;virtual; abstract; //need override to confirm the FExecutorClass
    procedure SetUp;override;
    procedure TearDown;override;
  public
  published
    procedure Test_VM_AlignStr;
    procedure Test_VM_StoreInt64;
  end;


implementation


procedure TTest_TurboExecutor.SetUp;
begin
  if not Assigned(FTestObject) then
  begin
    FApp := TTurboAppDomain.Create;
    FApp.ExecutorClass := GetExecutorClass;
  end;
end;

procedure TTest_TurboExecutor.TearDown;
begin
  FreeAndNil(FApp);
end;

property TTest_TurboExecutor.Test_VM_AlignStr;
begin
end;

procedure TTest_TurboExecutor.Test_VM_StoreInt64;
begin
  //(int64 addr) --
  //检查执行后堆栈是否为空；数据是否存在指定的地址上；如果输入错误，是否会触发OutOfDataStack错误！
end;

Initialization
  {RegisterTests('TurboScript suites',
                [
                 TTest_TurboExecutor.Suite
                ]);//}
finalization
end.
