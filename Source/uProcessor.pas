unit uProcessor;

interface

uses
  SysUtils, Classes;

type
  TProcessorState = (psRunning, psFinished, psNoData, psBadData, 
    psBadOp, psDivZero, psOverFlow);
  TProcessorStates = set of TProcessorState;
  //TRegister = integer;
  PAddress = ^TAddress;
  TAddress = record
  end;
  {: Code Field Address }
  TCodeField = packed record
    {: 调用方式 }
    CallStyle: Byte;
    NextCFA: Pointer;
  end;

  TMemoryArray = array of byte;
  TStack = TMemoryArray;

  TInstruction = (
    inPrepare,
    {## Arithmatic instuctions }
    //the integer operation
    inAddInt, //Add
    inSubInt, //subtract
    inIncInt, //add 1
    inIncNInt, //add N
    inDecInt, //subtract 1
    inDecNInt, //subtract N
    inMULInt, //multiply 
    inDIVInt, //divide

    {## Logical instuctions }
    //the integer operation
    inEQUInt,
    inNEQInt, // not equ
    inLESInt, //less than
    inLEQInt, //less than and equ
    inGERInt, //greater than
    inGEQInt, //greater than and equ
    inNOT, //Negate(NOT)
    inAND,
    inOR,
    inXOR,

    {## Memory Operation Instruction }
    inSetValue,
    inGetValue,
    
    {## Proc Operation Instruction }
    inJMP,
    inJZ,
    inJNZ,
    inCall,
    inReturn,
    inNoop,

    {## Stack Operation Instuction }
    inRSPush //Push Integer
    , inRSPop //Pop Integer

    {## Parameter Stack Operation Instuction }
    , inPush  //Push Integer
    //, inPop //the same as the Drop
    , inDrop
    , inDUP
    , inSWAP
    , inOVER
    , inROT
  ); 
  { Summary : The Virtual Machine Code }
  { Description:
    虚拟机机器码: 采用1参数码 
  }
  TVMCode = Record
    OpCode: TInstruction;
    OpParam: Integer;
  end;
  
type
  TCustomProcessor = class(TObject)
  private
    FMemorySize: Integer;
    FStackSize: Integer;
    procedure SetMemorySize(Value: Integer);
    procedure SetStackSize(Value: Integer);
  protected
    FIR: TInstruction;
    FMemory: TMemoryArray;
    FPC: Integer;
    FSP: Integer;
    FStatus: TProcessorStates;
    Stack: TStack;
    procedure iRSPop;
    procedure iRSPush;
  public
    procedure Execute(const aAddress: Integer = 0); virtual;
    procedure ExecuteInstruction(const aInstruction: TInstruction); virtual;
    procedure Init; virtual;
    procedure LoadFromFile(const aFileName: String);
    procedure LoadFromStream(const aStream: TStream); virtual; abstract;
    procedure SaveToFile(const aFile: String);
    procedure SaveToStream(const aStream: TStream); virtual; abstract;
    procedure Stop; virtual; abstract;
    property IR: TInstruction read FIR;
    property MemorySize: Integer read FMemorySize write SetMemorySize;
    property PC: Integer read FPC write FPC;
    property SP: Integer read FSP write FSP;
    property StackSize: Integer read FStackSize write SetStackSize;
    property Status: TProcessorStates read FStatus;
  end;
  
  TStackProcessor = class(TCustomProcessor)
  private
    FParameterStack: TStack;
    FParameterStackSize: Integer;
    PSP: Integer;
    procedure SetParameterStackSize(const Value: Integer);
  public
    procedure ExecuteInstruction(const aInstruction: TInstruction); override;
    procedure Init; override;
    property ParameterStackSize: Integer read FParameterStackSize write
            SetParameterStackSize;
  end;
  
  TForthProcessor = class(TStackProcessor)
  protected
    FWorkingRegister: Integer;
    FXRegister: Integer;
  public
    procedure iExecute;
    procedure iNext;
  end;
  

implementation

{
******************************* TCustomProcessor *******************************
}
procedure TCustomProcessor.Execute(const aAddress: Integer = 0);
begin
  Init;
  //
  while FPC < Length(FMemory) do
  begin
    FIR := TInstruction(@FMemory[FPC]);
    ExecuteInstruction(FIR);
    Inc(FPC, SizeOf(TInstruction));
  end;
end;

procedure TCustomProcessor.ExecuteInstruction(const aInstruction: TInstruction);
begin
end;

procedure TCustomProcessor.Init;
begin
  PC := 0;
  SP := StackSize;
end;

procedure TCustomProcessor.iRSPop;
begin
end;

procedure TCustomProcessor.iRSPush;
begin
end;

procedure TCustomProcessor.LoadFromFile(const aFileName: String);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

procedure TCustomProcessor.SaveToFile(const aFile: String);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, mCreate);
  try
    SaveToStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

procedure TCustomProcessor.SetMemorySize(Value: Integer);
begin
  if FMemorySize <> Value then
  begin
    FMemorySize := Value;
    SetLength(FMemory, FMemorySize);
  end;
end;

procedure TCustomProcessor.SetStackSize(Value: Integer);
begin
  if FStackSize <> Value then
  begin
    FStackSize := Value;
    SetLength(FStack, FStackSize);
  end;
end;

{
******************************* TStackProcessor ********************************
}
procedure TStackProcessor.ExecuteInstruction(const aInstruction: TInstruction);
var
  LTemp: Integer;
begin
  inherited ExecuteInstruction(aInstruction);
  case aInstruction of
    inAddInt:
    begin
      LTemp := Integer(@FParameterStack[PSP]);
      Inc(PSP, SizeOf(Integer));
      FParameterStack[PSP] := FParameterStack[PSP] + LTemp;
    end;
    inSubInt:
    begin
      LTemp := Integer(@FParameterStack[PSP]);
      Inc(PSP, SizeOf(Integer));
      FParameterStack[PSP] := FParameterStack[PSP] - LTemp;
    end;
    inIncInt:
    begin
      Inc(Integer(@FParameterStack[PSP]));
    end;
    inDecInt:
    begin
      Dec(Integer(@FParameterStack[PSP]));
    end;
    inIncNInt:
    begin
      LTemp := Integer(@FParameterStack[PSP]);
      Inc(PSP, SizeOf(Integer));
      Inc(Integer(@FParameterStack[PSP]), LTemp);
    end;
    inDecNInt:
    begin
      LTemp := Integer(@FParameterStack[PSP]);
      Inc(PSP, SizeOf(Integer));
      Dec(Integer(@FParameterStack[PSP]), LTemp);
    end;
    inMULInt:
    begin
      LTemp := Integer(@FParameterStack[PSP]);
      Inc(PSP, SizeOf(Integer));
      FParameterStack[PSP] := FParameterStack[PSP] * LTemp;
    end;
    inDIVInt:
    begin
      LTemp := Integer(@FParameterStack[PSP]);
      Inc(PSP, SizeOf(Integer));
      FParameterStack[PSP] := FParameterStack[PSP] div LTemp;
    end;
  end;
end;

procedure TStackProcessor.Init;
begin
  inherited Init;
  PSP := FParameterStackSize;
end;

procedure TStackProcessor.SetParameterStackSize(const Value: Integer);
begin
  if FParameterStackSize <> Value then
  begin
    FParameterStackSize := Value;
    SetLength(FParameterStack, FParameterStackSize);
  end;
end;

{
******************************* TForthProcessor ********************************
}
procedure TForthProcessor.iExecute;
begin
end;

procedure TForthProcessor.iNext;
begin
  FWorkingRegister := Integer(@FMemory[FPC]);
  //JMP (FWorkingRegister)
  FPC := FWorkingRegister;
end;


initialization
finalization
end.
