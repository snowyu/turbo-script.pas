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

  TForthLibs = array of TForthLib; 
  TForthLib = packed record
    Name: string; //1 DWord(Pointer)
    Words: TForthWords;
  end;
  TForthWords = array of TForthWord;
  PForthWord = ^ TForthWord;
  TForthWord = packed record //ITC (Indirect Threaded Code)
    //PriorWord: PForthWord; //前一个单词 -1 means 为最前面。
    Name: String;
    Precedence: Byte; //优先级, 0=low, 1=high equals 1 for an IMMEDIATE word
    //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
    //this can be extent to private, protected, public, etc
    Visibility: Byte; 
    CallStyle: Byte;
    CodeFieldStyle: Byte;
    CFA: Integer;
    ParameterFields: array of Integer; //大多数情况下是PForthWord，但是少数情况下是数据或VM Codes
  end;
  TProcessorState = (psRunning, psCompiling, psFinished, psNoData, psBadData, 
    psBadOp, psDivZero, psOverFlow);
  TProcessorStates = set of TProcessorState;

  TSuperInterpreter = class(TCustomSuperExecutor)
  private
    FForthLibs: TForthLibs;
    FParameterStack: TStack;
    FParameterStackSize: Integer;
    FPSP: Integer;
    FStackSize: Integer;
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetStackSize(const Value: Integer);
  protected
    FIR: TInstruction;
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
    procedure LoadFromStream(const aStream: TStream); override;
    procedure SaveToStream(const aStream: TStream); override;
    property ForthLibs: TForthLibs read FForthLibs write FForthLibs;
    property IR: TInstruction read FIR;
    property ParameterStackSize: Integer read FParameterStackSize write
            SetParameterStackSize;
    property PC: Integer read FPC write FPC;
    property PSP: Integer read FPSP write FPSP;
    property SP: Integer read FSP write FSP;
    property StackSize: Integer read FStackSize write SetStackSize;
    property Status: TProcessorStates read FStatus;
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
  
    SetLength(FForthLibs, Length(FForthLibs)+1);
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
  
  
  end;
end;

procedure TSuperInterpreter.SaveToStream(const aStream: TStream);
begin
  inherited SaveToStream(aStream);
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
