{Turbo Script Memory Module automatic Test}
unit uTurboModuleTest;

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
  , uTurboConsts
  , uTurboExecutor
  ;


type
  {
  }
  TTest_CustomTurboModule = class (TTestCase)
  protected
    FTestObject: TCustomTurboModule;
  protected
    procedure CreateTestObject;virtual;
    procedure SetUp;override;
    procedure TearDown;override;
    procedure TestClear;
    procedure TestAddMem;
    procedure TestAddData;
  public
  published
    //Code Mem Access Methods:
    procedure Test_AddMem;
    procedure Test_AddData;
  end;


implementation

procedure TTest_CustomTurboModule.CreateTestObject;
begin
  FTestObject := TCustomTurboModule.Create();
end;

procedure TTest_CustomTurboModule.SetUp;
begin
  if not Assigned(FTestObject) then
    CreateTestObject;
end;

procedure TTest_CustomTurboModule.TearDown;
begin
  FreeAndNil(FTestObject);
end;

procedure TTest_CustomTurboModule.TestClear;
begin
  FTestObject.ClearMemory;
  with FTestObject do
  begin
    CheckEquals(0, UsedMemory, 'the UsedMemory is mismatch.');
    CheckEquals(SizeOf(TTurboPreservedDataMemory), UsedDataSize, 'the UsedDataSize is mismatch.');
  end;
end;

procedure TTest_CustomTurboModule.Test_AddMem;
begin
  TestClear;
  TestAddMem;
  TestClear;
end;

procedure TTest_CustomTurboModule.Test_AddData;
begin
  TestClear;
  TestAddData;
  TestClear;
end;

procedure TTest_CustomTurboModule.TestAddMem;
var
 p: pointer;
 i: integer;
 vSize: integer;
 s: string;
begin
  with FTestObject do
  begin
    i := Random(MaxInt);
    vSize := UsedMemory;
    p := Pointer(UsedMemory);
    AddIntToMem(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Integer);
    CheckEquals(vSize, UsedMemory, 'the addint UsedMemory is mismatch.');
    CheckEquals(i, PInteger(p)^, 'the addint value is mismatch.');
    Check(MemorySize>=UsedMemory, 'the MemorySize should greater or equ UsedMemory.');

    i := Random($FF);
    p := Pointer(UsedMemory);
    AddByteToMem(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Byte);
    CheckEquals(vSize, UsedMemory, 'the addbyte UsedMemory is mismatch.');
    CheckEquals(i, PByte(p)^, 'the addbyte value is mismatch.');
    Check(MemorySize>=UsedMemory, 'the MemorySize should greater or equ UsedMemory.');

    AlignMem; //align by dword border.
    vSize := (vSize + 3) and $FFFFFFFC;
    CheckEquals(vSize, UsedMemory, 'the Align is mismatch.');
    CheckEquals(0, vSize mod SizeOf(Integer), 'the Align does not work(Size mod SizeOf(Integer) should be 0)..');
    Check(MemorySize>=UsedMemory, 'the MemorySize should greater or equ UsedMemory.');

    {i := Random($FFFF);
    p := Pointer(UsedMemory);
    AddWord(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Word);
    CheckEquals(vSize, UsedMemory, 'the addword UsedMemory is mismatch.');
    CheckEquals(i, PWord(p)^, 'the addword value is mismatch.');
    Check(MemorySize>=UsedMemory, 'the MemorySize should greater or equ UsedMemory.');

    s := 'Hello 测试字符串PChar内存分配 Tao Is Nothing!!!';
    p := Pointer(UsedMemory);
    AddPCharToMem(s);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + Length(s) + 1;
    CheckEquals(vSize, UsedMemory, 'the addPChar UsedMemory is mismatch.');
    CheckEquals(s, PChar(p), 'the addPchar value is mismatch.');
    Check(MemorySize>=UsedMemory, 'the MemorySize should greater or equ UsedMemory.');
    //}

    s := 'Hello 测试Buffer 内存分配 Tao Is Buffer Nothing!!!';
    p := Pointer(UsedMemory);
    AddBufferToMem(s[1], Length(s));
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + Length(s);
    CheckEquals(vSize, UsedMemory, 'the addbuffer UsedMemory is mismatch.');
    CheckEqualsMem(@s[1], p, Length(s), 'the addbuffer value is mismatch.');
    Check(MemorySize>=UsedMemory, 'the MemorySize should greater or equ UsedMemory.');

    i := Random($FFF) + 10;
    p := Pointer(UsedMemory);
    AllocSpace(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + i;
    CheckEquals(vSize, UsedMemory, 'the AllocSpace UsedMemory is mismatch.');
    Check(MemorySize>=UsedMemory, 'the MemorySize should greater or equ UsedMemory.');

  end;
end;

procedure TTest_CustomTurboModule.TestAddData;
var
 p: pointer;
 i: integer;
 vSize: integer;
 s: string;
begin
  with FTestObject do
  begin
    i := Random(MaxInt);
    vSize := UsedDataSize;
    p := Pointer(UsedDataSize);
    AddIntToData(i);
    p := Pointer(Integer(DataMemory) + Integer(p));
    vSize := vSize + SizeOf(Integer);
    CheckEquals(vSize, UsedDataSize, 'the addint UsedDataSize is mismatch.');
    CheckEquals(i, PInteger(p)^, 'the addint value is mismatch.');
    Check(DataMemorySize>=UsedDataSize, 'the DataMemorySize should greater or equ UsedDataSize.');

    i := Random($FF);
    p := Pointer(UsedDataSize);
    AddByteToData(i);
    p := Pointer(Integer(DataMemory) + Integer(p));
    vSize := vSize + SizeOf(Byte);
    CheckEquals(vSize, UsedDataSize, 'the addbyte UsedDataSize is mismatch.');
    CheckEquals(i, PByte(p)^, 'the addbyte value is mismatch.');
    Check(DataMemorySize>=UsedDataSize, 'the DataMemorySize should greater or equ UsedDataSize.');

    AlignData; //align by dword border.
    vSize := (vSize + 3) and $FFFFFFFC;
    CheckEquals(vSize, UsedDataSize, 'the Align is mismatch.');
    CheckEquals(0, vSize mod SizeOf(Integer), 'the Align does not work(Size mod SizeOf(Integer) should be 0)..');
    Check(DataMemorySize>=UsedDataSize, 'the DataMemorySize should greater or equ UsedDataSize.');

    {i := Random($FFFF);
    p := Pointer(UsedDataSize);
    AddWord(i);
    p := Pointer(Integer(DataMemory) + Integer(p));
    vSize := vSize + SizeOf(Word);
    CheckEquals(vSize, UsedDataSize, 'the addword UsedDataSize is mismatch.');
    CheckEquals(i, PWord(p)^, 'the addword value is mismatch.');
    Check(DataMemorySize>=UsedDataSize, 'the DataMemorySize should greater or equ UsedDataSize.');
    //}

    s := 'Hello 测试字符串PChar内存分配 Tao Is Nothing!!!';
    p := Pointer(UsedDataSize);
    AddPCharToData(s);
    p := Pointer(Integer(DataMemory) + Integer(p));
    vSize := vSize + Length(s) + 1;
    CheckEquals(vSize, UsedDataSize, 'the addPChar UsedDataSize is mismatch.');
    CheckEquals(s, PChar(p), 'the addPchar value is mismatch.');
    Check(DataMemorySize>=UsedDataSize, 'the DataMemorySize should greater or equ UsedDataSize.');

    s := 'Hello 测试Buffer 内存分配 Tao Is Buffer Nothing!!!很好的不错风格仅仅只是测试sdJlrrjei#@$##$%^%HBRTBfdfdfgdfg32323##$#$#2121ej jedj98rurfhfwer33$$444rrertertetefjuoisssssssssssssssssss';
    s := s + s + s + s;
    p := Pointer(UsedDataSize);
    AddBufferToData(s[1], Length(s));
    p := Pointer(Integer(DataMemory) + Integer(p));
    vSize := vSize + Length(s);
    CheckEquals(vSize, UsedDataSize, 'the addbuffer UsedDataSize is mismatch.');
    CheckEqualsMem(@s[1], p, Length(s), 'the addbuffer value is mismatch.');
    Check(DataMemorySize>=UsedDataSize, 'the DataMemorySize should greater or equ UsedDataSize');

    i := Random($FFF) + 10;
    p := Pointer(UsedDataSize);
    AllocDataSpace(i);
    p := Pointer(Integer(DataMemory) + Integer(p));
    vSize := vSize + i;
    CheckEquals(vSize, UsedDataSize, 'the AllocSpace UsedDataSize is mismatch');
    Check(DataMemorySize>=UsedDataSize, 'the DataMemorySize should greater or equ UsedDataSize');

  end;
end;

Initialization
  RegisterTests('TurboScript suites',
                [
                 TTest_CustomTurboModule.Suite
                 //, TTest_MeList.Suite
                ]);//}
finalization
end.
