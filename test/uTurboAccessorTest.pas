{Turbo Script Accessor automatic Test}
unit uTurboAccessorTest;

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
  {
  }
  TTest_CustomTurboAccessor = class (TTestCase)
  protected
    FTestObject: TCustomTurboModule;
  protected
    procedure CreateTestObject;virtual;
    procedure Setup;override;
    procedure TearDown;override;
  public
  published
    //Code Mem Access Methods:
    procedure Test_AlignMem;
    procedure Test_AddBufferToMem;
    procedure Test_AddByteToMem;
    procedure Test_AddIntToMem;
  end;


implementation

procedure TTest_CustomTurboAccessor.CreateTestObject;
begin
  FTestObject := TCustomTurboModule.Create();
end;

procedure TTest_CustomTurboAccessor.Setup;
begin
  if not Assigned(FTestObject) then
    CreateTestObject;
end;

procedure TTest_CustomTurboAccessor.TearDown;
begin
  FreeAndNil(FTestObject);
end;

procedure TTest_CustomTurboAccessor.Test_AddBufferToMem;
var
  i: Integer;
  vBuf: PChar;
  s: string;
begin
  i := FTestObject.UsedMemory;
  s := '_____18a9-012 Test My Add Buffer To Code Memory Xzxvvei79832#@#jnjfv^*)x____'#0;
  FTestObject.AddBufferToMem(s[1], Length(s));
  CheckEquals(Length(s), FTestObject.UsedMemory-i, 'the size of buffer is mismatch.');
  Integer(vBuf) := i + Integer(FTestObject.Memory);
  CheckEquals(s, String(vBuf), 'the buffer content is mismatch.');
end;

procedure TTest_CustomTurboAccessor.Test_AddByteToMem;
var
  i: Integer;
  vBuf: PByte;
  s: byte;
begin
  i := FTestObject.UsedMemory;
  s := 111;
  FTestObject.AddByteToMem(s);
  CheckEquals(1, FTestObject.UsedMemory-i, 'the byte size error.');
  Integer(vBuf) := i + Integer(FTestObject.Memory);
  CheckEquals(s, vBuf^, 'the byte content is mismatch.');
end;

procedure TTest_CustomTurboAccessor.Test_AddIntToMem;
var
  i: Integer;
  vBuf: PInteger;
  s: Integer;
begin
  Test_AlignMem;
  i := FTestObject.UsedMemory;
  s := 11112222;
  FTestObject.AddIntToMem(s);
  CheckEquals(SizOf(Integer), FTestObject.UsedMemory-i, 'the Integer size error.');
  Integer(vBuf) := i + Integer(FTestObject.Memory);
  CheckEquals(s, vBuf^, 'the Integer content is mismatch.');
end;


Initialization
  RegisterTests('TurboScript suites',
                [
                 TTest_CustomTurboAccessor.Suite
                 //, TTest_MeList.Suite
                ]);//}
finalization
end.
