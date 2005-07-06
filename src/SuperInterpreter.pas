unit SuperInterpreter;

interface

uses
  SysUtils, Classes
  , uSuperScriptConsts
  , uSuperExecutor 
  ;

type
  TMemoryArray = array of byte;
  TStack = TMemoryArray;
  TVMMethodList = array [TVMInstruction] of TMethod;

  //TForthLibs = array of TForthLib;
  PForthLib = ^ TForthLib; 
  TForthLib = packed record
    PriorLib: Integer; //PForthLib;
    LastWord: Integer; //PForthWord; //前一个单词 0 means 为最前面。
    NameLen: Byte;
    //Name: array[0..NameLen] of char; //it's a PChar, the last char is #0
  end;
  //TForthWords = array of TForthWord;
  PForthWord = ^ TForthWord;
  TForthWord = packed record //ITC (Indirect Threaded Code)
    PriorWord: Integer; //PForthWord; //前一个单词 0 means 为最前面。
    Precedence: Byte; //优先级, 0=low, 1=high equals 1 for an IMMEDIATE word
    //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
    //this can be extent to private, protected, public, etc
    Visibility: Byte; 
    CallStyle: Byte;
    CodeFieldStyle: Byte;
    //其实就是直接指向的某个单词的PFA，不过那个单词的PFA就是直接执行的机器码而已。
    CFA: LongWord;
    //the Param Field Length 
    ParamFieldLength: LongWord;
    NameLen: Byte;
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //ParameterFields: array of Integer; //大多数情况下是PForthWord，但是少数情况下是数据或VM Codes
  end;
  TProcessorState = (psRunning, psCompiling, psFinished, psNoData, psBadData, 
    psBadOp, psDivZero, psOverFlow);
  TProcessorStates = set of TProcessorState;

  TSuperInterpreter = class(TCustomSuperExecutor)
  private
    FForthLibs: TForthLibs;
    FMemorySize: Integer;
    FParameterStack: TStack;
    FParameterStackSize: Integer;
    FPLibEntry: PForthLib;
    FPSP: Integer;
    FStackSize: Integer;
    FUsedMemory: Integer;
    procedure SetMemorySize(Value: Integer);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetStackSize(const Value: Integer);
  protected
    FInternalProcList: TVMMethodList;
    FIR: TInstruction;
    FMemory: TMemoryArray;
    FPC: Integer;
    FSP: Integer;
    FStatus: TProcessorStates;
    Stack: TStack;
    function ExecuteCFA(const aCFA: Integer): Integer; override;
    procedure Init; override;
  public
    constructor Create;
    procedure ExecuteInstruction(const aInstruction: TInstruction); virtual;
    function GetWordCFA(const aWord: string): Integer; override;
    procedure InitProcList;
    procedure LoadFromStream(const aStream: TStream); override;
    procedure SaveToStream(const aStream: TStream); override;
    property ForthLibs: TForthLibs read FForthLibs write FForthLibs;
    property IR: TInstruction read FIR;
    property MemorySize: Integer read FMemorySize write SetMemorySize;
    property ParameterStackSize: Integer read FParameterStackSize write
            SetParameterStackSize;
    property PC: Integer read FPC write FPC;
    property PLibEntry: PForthLib read FPLibEntry write FPLibEntry;
    property PSP: Integer read FPSP write FPSP;
    property SP: Integer read FSP write FSP;
    property StackSize: Integer read FStackSize write SetStackSize;
    property Status: TProcessorStates read FStatus;
    property UsedMemory: Integer read FUsedMemory write FUsedMemory;
  end;
  

implementation

{
****************************** TSuperInterpreter *******************************
}
constructor TSuperInterpreter.Create;
begin
  inherited Create;
  StackSize := cDefaultStackSize;
  ParameterStackSize := cDefaultParamStackSize;
end;

function TSuperInterpreter.ExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := inherited ExecuteCFA(aCFA);
end;

procedure TSuperInterpreter.ExecuteInstruction(const aInstruction:
        TInstruction);
begin
end;

function TSuperInterpreter.GetWordCFA(const aWord: string): Integer;
begin
  Result := inherited GetWordCFA(aWord);
end;

procedure TSuperInterpreter.Init;
begin
  PC := 0;
  SP := StackSize;
  FPSP := FParameterStackSize;
  SetLength(FMemory, 0);
end;

procedure TSuperInterpreter.InitProcList;
begin
  FInternalProcList[inAddInt] := iVMAddInt;
end;

procedure TSuperInterpreter.LoadFromStream(const aStream: TStream);
var
  I: Integer;
  J: Integer;
  LDumy: LongWord;
  LPriorLibEntry: LongWord;
  LByte: Byte;
  LPriorWordEntry: LongWord;
begin
  inherited LoadFromStream(aStream);
  Init;
  with aStream do
  begin
    Read(LDumy, SizeOf(LDumy));
    if (LDumy > 0) and (FileType = ftProgram) then
    begin
      ParameterStackSize := LDumy;
    end;
  
    Read(LDumy, SizeOf(LDumy));
    if (LDumy > 0) and (FileType = ftProgram) then
    begin
      StackSize := LDumy;
    end;
  
    UsedMemory := Size - Poision + Length(FMemory);
    SetLength(FMemory, UsedMemory);
    Read(@FMemory[0], Length(FMemory));
    LDumy := PLongWord(@FMemory[0])^;
    PLibEntry := PForthLib(@FMemory[LDumy]);
    {
      So the Memory Mirror is
      Lib Entry: FMemory[0]
    }
    {//SetLength(FForthLibs, Length(FForthLibs)+1);
    //the Lib Entry
    Read(LDumy, SizeOf(LDumy));
    Seek(LDumy, soFromBeginning);
    I := 0;
    repeat
      SetLength(FForthLibs, I+1);
  
      //the PriorLibEntry
      Read(LPriorLibEntry, SizeOf(LPriorLibEntry));
      //the Lib Name Length
      Read(LByte, SizeOf(LByte));
      with FForthLibs[I] do
      begin
        SetLength(Name, LByte);
        Read(PChar(Name), LByte);
      end;
      //the LastWord Entry
      Read(LDumy, SizeOf(LDumy));
      Seek(LDumy, soFromBeginning);
      J := 0;
      repeat
        SetLength(FForthLibs[I].Words, J+1);
        //the Prior Word Entry
        Read(LPriorWordEntry, SizeOf(LPriorLibEntry));
        //the Lib Name Length
        Read(LByte, SizeOf(LByte));
        with FForthLibs[I].Words[J] do
        begin
          SetLength(Name, LByte);
          Read(PChar(Name), LByte);
          Read(Precedence, SizeOf(Precedence));
          Read(Visibility, SizeOf(Visibility));
          Read(CallStyle, SizeOf(CallStyle));
          Read(CodeFieldStyle, SizeOf(CodeFieldStyle));
          Read(CFA, SizeOf(CFA));
          //the PF Len
          Read(LDumy, Sizeof(LDumy));
          SetLength(ParameterFields, LDumy);
          Read(@ParameterFields[0], LDumy);
        end;
        if LPriorWordEntry <> 0 then
        begin
          Inc(J);
          Seek(LPriorWordEntry, soFromBeginning);
        end;
      unit LPriorWordEntry = 0;
      if LPriorLibEntry <> 0 then
      begin
        Inc(I);
        Seek(LPriorLibEntry, soFromBeginning);
      end;
    until LPriorLibEntry = 0;
  //}
  
  end;
end;

procedure TSuperInterpreter.SaveToStream(const aStream: TStream);
begin
  inherited SaveToStream(aStream);
end;

procedure TSuperInterpreter.SetMemorySize(Value: Integer);
begin
  if FMemorySize <> Value then
  begin
    FMemorySize := Value;
    SetLength(FMemory, FMemorySize);
  end;
end;

procedure TSuperInterpreter.SetParameterStackSize(const Value: Integer);
begin
  if FParameterStackSize <> Value then
  begin
    FParameterStackSize := Value;
    SetLength(FParameterStack, FParameterStackSize);
  end;
end;

procedure TSuperInterpreter.SetStackSize(const Value: Integer);
begin
  if FStackSize <> Value then
  begin
    FStackSize := Value;
    SetLength(FStack, FStackSize);
  end;
end;


end.
