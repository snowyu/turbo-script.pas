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

  TMemoryArray = array of Integer;
  TStack = TMemoryArray; 

  TInstruction = (
    inPrepare,
    {## Arithmatic instuctions }
    inAdd, //Add
    inSub, //subtract
    inInc, //add 1
    inDec, //subtract 1
    inMUL, //multiply 
    inDIV, //divide

    {## Logical instuctions }
    inEQU,
    inNEQ, // not equ
    inLES, //less than
    inLEQ, //less than and equ
    inGET, //greater than
    inGEQ, //greater than and equ
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
    inPush,
    inPop,
    inDrop,
    inDUP,
    inSWAP,
    inOVER,
    inROT
  ); 
  
type
  TCustomProcessor = class (TObject)
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
  public
    procedure Execute; virtual; abstract;
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
  
  TStackProcessor = class (TCustomProcessor)
  end;
  
  TForthProcessor = class (TStackProcessor)
  end;
  

implementation

{
******************************* TCustomProcessor *******************************
}
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


initialization
finalization
end.
