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
    FTestObject: TCustomTurboExecutor;
  protected
    procedure CreateTestObject;virtual;abstract;
    procedure Setup;override;
    procedure TearDown;override;
  public
  published
  end;


implementation


procedure TTest_TurboExecutor.Setup;
begin
  if not Assigned(FTestObject) then
    CreateTestObject;
end;

procedure TTest_TurboExecutor.TearDown;
begin
  FreeAndNil(FTestObject);
end;


Initialization
  {RegisterTests('TurboScript suites',
                [
                 TTest_TurboExecutor.Suite
                ]);//}
finalization
end.
