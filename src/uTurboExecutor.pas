{: The abstract super script executor module. }
{ Description

��ס����Ķ�ջ(����X86�Ķ�ջ����)�� ѹ�����ǵ�ַ���٣��������ǵ�ַ����
When an item is pushed onto the stack, the processor decrements the ESP
register, then writes the item at the new top of stack. When an item is popped
off the stack, the
processor reads the item from the top of stack, then increments the ESP register


Ҳ�������� TTurboProgam ���У����������������ջ������ջ���ڴ棺
  ParameterStackSize := 4096;
  GetMem(ParameterStack, ParameterStackSize);
  ReturnStackSize := 4096;
  GetMem(ReturnStack, ReturnStackSize);
}
unit uTurboExecutor;

interface

uses
  SysUtils, Classes
  , uTurboScriptConsts
  ;

type
  TTurboScriptOption = (soOptimize, soLoadOnDemand, soBindingRuntime);
  TTurboScriptOptions = set of TTurboScriptOption;

  TCustomTurboPEFormat = class;
  TCustomTurboExecutor = class;
  TTurboProgram = class;
  {: the abstract Portable Executable File Format. }
  { Description
  �ض�λ��ַ:
   1.Import���е�ģ��(DLL��ForthDLLģ��)�����ĵ�ַ
   2.Relocation�����Ե�ַ�����Ҫ���¼���ģ�
  }
  TCustomTurboPEFormat = class(TObject)
  private
    FImageBase: Integer;
  protected
    ExportTable: Integer;
    {: ������ʹ�õ�ģ���еĺ��� }
    { Description
    ����Import���е�ģ��(DLL��ForthDLLģ��)
    }
    ImportTable: Integer;
    {: �ض�λ��ַ�� }
    { Description
    �������ļ��еľ��Ի���ַ����ImageBase($0000)
    So �µľ��Ե�ַӦ����: OrgAddress - ImageBase + @CodeArea[0]
    }
    RelocationTable: Pointer;
  public
    {: Ԥ��Ĵ������ַ }
    property ImageBase: Integer read FImageBase write FImageBase;
  end;

  {: the abstract turbo script executor. }
  { Description
  Load the script into memory, and execute the script(maybe translate it into
  native language first).

  �����ģ������������ģ�飨��������������ô�죿
  ������ν������Ǻ��ڰ��������顣

  �Ƿ�֧��Ƕ����ģ�飿
  �����֧�֣���ô�ͱ������������Modules���ԣ���������ʵ�֡�
  ���Ҫ����Ƕ����ģ���ڵĺ�����ô��Ҫ: Module.SubModule.Func.
  ��������������װ�صĸ��Ӷȣ��Ӵ�ʱ�䣬����Ч�ʡ�
  �ҵ��뷨���ǣ����ļ����������Ŀ¼����ʽʵ����ģ�顣��Ȼ�������ݿ�Ҳ���編���ơ�
  ���������ģ��װ���ʱ��ֻװ�����֣���������������ִ��ʱ����Ҫװ�롣
  ��׼����ģ��װ�뱣����ƣ���Executor�Ϸ��룺 TTurboScriptAccessor��

  Note:Ϊ�˱������¼����ַ��ȫ���������ƫ������
  }
  TCustomTurboExecutor = class(TObject)
  private
    function GetIsLoaded: Boolean;
    function GetLastErrorCode: TTurboForthProcessorErrorCode;
    function GetStatus: TTurboForthProcessorStates;
    function GetTIB: string;
    procedure SetIsLoaded(const Value: Boolean);
    procedure SetMemorySize(Value: Integer);
    procedure SetStatus(Value: TTurboForthProcessorStates);
    procedure SetTIB(Value: string);
  protected
    {: The Code Memory }
    FMemory: Pointer;
    FMemorySize: Integer;
    FModuleDate: TTimeStamp;
    FModuleType: TTurboScriptModuleType;
    FModuleVersion: LongWord;
    FName: string;
    FOptions: TTurboScriptOptions;
    {: : the Parameter Stack }
    FParameterStack: Pointer;
    {: : the Parameter Stack }
    FParameterStackSize: Integer;
    FPC: Integer;
    FProg: TTurboProgram;
    {: : Return(Proc) Stack }
    { Description
    ���ض�ջ
    }
    FReturnStack: Pointer;
    {: : Return(Proc) Stack }
    { Description
    ���ض�ջ
    }
    FReturnStackSize: Integer;
    FRP: Integer;
    {: the Parameter Stack(or data stack) Pointer }
    FSP: Integer;
    {: The Current TIB Index }
    { Description
    FTIBIndex : Text[FTIBIndex]
    }
    FTextIndex: Integer;
    FUsedMemory: Integer;
    procedure Grow;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddIntToMem(const aInt: Integer);
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function ExecuteCFA(const aCFA: Integer): Integer; virtual;
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function ExecuteWord(const aWord: string): Integer;
    procedure FinalizeExecution; virtual;
    function GetModuleMemoryAddr(aModuleIndex: Integer): Pointer;
    function GetWordCFA(const aWord: string): Integer; virtual;
    {: Init the Virtual Machine. }
    procedure InitExecution; virtual;
    {: reset the stack pointers. }
    procedure Reset;
    procedure Stop;
    { Description
    if not loaded, then only the name and some options loaded but the body(
    Memory).
    }
    property IsLoaded: Boolean read GetIsLoaded write SetIsLoaded;
    property LastErrorCode: TTurboForthProcessorErrorCode read GetLastErrorCode;
    {: The Code Memory }
    property Memory: Pointer read FMemory write FMemory;
    {: : the Memory Size. }
    { Description
    warining: if in the running status, you may be get trouble!!
    }
    property MemorySize: Integer read FMemorySize write SetMemorySize;
    { Description
    (The ModuleDate field indicates the number of calendar days since the start
    of the calendar (the number of days since 1/1/0001 plus one).)
    See Also TTimeStamp
    }
    property ModuleDate: TTimeStamp read FModuleDate write FModuleDate;
    property ModuleType: TTurboScriptModuleType read FModuleType write
            FModuleType;
    property ModuleVersion: LongWord read FModuleVersion write FModuleVersion;
    property Name: string read FName write FName;
    property Options: TTurboScriptOptions read FOptions write FOptions;
    {: : the Parameter Stack }
    { Description
    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ParameterStack: Pointer read FParameterStack write FParameterStack;
    {: : the Parameter Stack }
    { Description
    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ParameterStackSize: Integer read FParameterStackSize write
            FParameterStackSize;
    {: : program counter. }
    { Description
    program counter, which contains the address 
    in memory of the instruction that is the next 
    to be executed. 
    }
    property PC: Integer read FPC write FPC;
    property Prog: TTurboProgram read FProg write FProg;
    {: : Return(Proc) Stack }
    { Description
    ���ض�ջ

    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ReturnStack: Pointer read FReturnStack write FReturnStack;
    {: : Return(Proc) Stack }
    { Description
    ���ض�ջ

    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ReturnStackSize: Integer read FReturnStackSize write
            FReturnStackSize;
    {: : return stack pointer(TOS). }
    { Description
    stack pointer, a register that points to the area 
    in memory utilized as the main return stack.

    the RP0-StackSize <= the stack memory < RP0.
    }
    property RP: Integer read FRP write FRP;
    {: the Parameter Stack(or data stack) Pointer }
    property SP: Integer read FSP write FSP;
    {: the current status of the script. }
    property Status: TTurboForthProcessorStates read GetStatus write SetStatus;
    {: the script source(TIB) }
    { Description
    TIB: text input Buffer
    }
    property TIB: string read GetTIB write SetTIB;
    {: �Ѿ�ʹ�õ��ڴ� }
    { Description
    Ҳ����ָ�����Ŀ����ڴ棺
    �Ӹõ�ַ����ڴ�δ�ã�FMemory[UsedMemory] 
    }
    property UsedMemory: Integer read FUsedMemory write FUsedMemory;
  end;

  TTurboProgram = class(TObject)
  private
    FOptions: TTurboScriptOptions;
    FParameterStack: Pointer;
    FParameterStackSize: Integer;
    FStack: Pointer;
    FStackSize: Integer;
    FStatus: TTurboForthProcessorStates;
    procedure SetExecutor(const Value: TCustomTurboExecutor);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetStackSize(const Value: Integer);
  protected
    FExecutor: TCustomTurboExecutor;
  public
    { Description
    Ϊ���ڶ�� executor �й���������ջ������ջ�Լ����в�����
    }
    constructor Create;
    destructor Destroy; override;
    {: the turbo script program class }
    { Description
    allocate the memory to return stack and param stack.
    }
    procedure Execute(aTimeOut : Integer = 0);
    {: Stops the execution of the script program as soon as possible.  }
    { Description
    Remember: If TTurboProgram is executing an external procedure the program
    won't stop until this procedure returns.
    }
    procedure Stop;
    property Executor: TCustomTurboExecutor read FExecutor write SetExecutor;
    property Options: TTurboScriptOptions read FOptions write FOptions;
    property ParameterStack: Pointer read FParameterStack write FParameterStack;
    property ParameterStackSize: Integer read FParameterStackSize write
            SetParameterStackSize;
    {: :Return Stack }
    property Stack: Pointer read FStack write FStack;
    {: : Return Stack Size }
    property StackSize: Integer read FStackSize write SetStackSize;
    {: the current status of the script. }
    property Status: TTurboForthProcessorStates read FStatus write FStatus;
  end;


  PPreservedCodeMemory = ^ TPreservedCodeMemory;
  //the typecast for code memory area to get the parameters
  TPreservedCodeMemory = packed record
    States: TTurboForthProcessorStates;
    Executor: TCustomTurboExecutor;
    //this Module unique Index in this program, allocated by compiler.
    ModuleIndex: Integer;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    TIBLength: Integer; //#TIB the text buffer length
    ToIn: Integer; //>IN the text buffer current index
    TIB: array [0..cMAXTIBCount-1] of char; //'TIB
    LastErrorCode: TTurboForthProcessorErrorCode;
    LastWordEntry: Pointer;
    LastVarEntry: Pointer;
  end;

implementation

{
***************************** TCustomTurboExecutor *****************************
}
constructor TCustomTurboExecutor.Create;
begin
  inherited Create;
  //FStatus := [];
  MemorySize := SizeOf(TPreservedCodeMemory) + cDefaultFreeMemSize;
  FUsedMemory := SizeOf(TPreservedCodeMemory);
end;

destructor TCustomTurboExecutor.Destroy;
begin
  FreeMem(FMemory);
  FMemory := nil;
  FMemorySize := 0;
  inherited Destroy;
end;

procedure TCustomTurboExecutor.AddIntToMem(const aInt: Integer);
var
  p: Pointer;
begin
  if FUsedMemory >= FMemorySize then
    Grow;
  Integer(p) := Integer(FMemory) + FUsedMemory;
  PInteger(P)^ := aInt;
  Inc(FUsedMemory, SizeOf(Integer));
end;

function TCustomTurboExecutor.ExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := -1;
  if (psRunning in Status) then
    raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);

  Include(PPreservedCodeMemory(FMemory).States, psRunning);
end;

function TCustomTurboExecutor.ExecuteWord(const aWord: string): Integer;
var
  aCFA: Integer;
begin
  aCFA := GetWordCFA(aWord);
  if aCFA >=0 then
    Result := ExecuteCFA(aCFA)
  else
    Result := -1;
end;

procedure TCustomTurboExecutor.FinalizeExecution;
begin
end;

function TCustomTurboExecutor.GetIsLoaded: Boolean;
begin
  Result := Assigned(FMemory) and (psLoaded in PPreservedCodeMemory(FMemory).States);
end;

function TCustomTurboExecutor.GetLastErrorCode: TTurboForthProcessorErrorCode;
begin
  Result := PPreservedCodeMemory(FMemory).LastErrorCode;
end;

function TCustomTurboExecutor.GetModuleMemoryAddr(aModuleIndex: Integer):
        Pointer;
begin
end;

function TCustomTurboExecutor.GetStatus: TTurboForthProcessorStates;
begin
  Result := PPreservedCodeMemory(FMemory).States;
end;

function TCustomTurboExecutor.GetTIB: string;
var
  I: Integer;
begin
  i := PPreservedCodeMemory(FMemory).TIBLength;
  if i > 0 then
  begin
    SetLength(Result, i);
    Move(PPreservedCodeMemory(FMemory).TIB, PChar(Result)^, i);
  end
  else
    Result := '';
end;

function TCustomTurboExecutor.GetWordCFA(const aWord: string): Integer;
begin
  Result := -1;
end;

procedure TCustomTurboExecutor.Grow;
begin
  MemorySize := FMemorySize + FMemorySize div 4;
end;

procedure TCustomTurboExecutor.InitExecution;
begin
  if (psRunning in Status) then
    raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);
  //MemorySize := cLastWordEntryOffset + cDefaultFreeMemSize;
  //FUsedMemory := cLastWordEntryOffset;


  {PInteger(@FMemory[cTIBLengthOffset])^ := 0;
  PInteger(@FMemory[cToINOffset])^ := 0;
  PChar(@FMemory[cTIBOffset])^ := #0;
  PChar(@FMemory[cLastWordEntryOffset-1])^ := #0;
  }
  with PPreservedCodeMemory(FMemory)^ do
  begin
    TIBLength := 0;
    ToIn := 0;
    TIB[0] := #0;
    LastWordEntry := nil;
    LastErrorCode := errNone;
  end;
  PPreservedCodeMemory(FMemory).Executor := Self;
  //FParameterStack := Prog.ParameterStack;
  //FParamStackSize := Prog.ParameterStackSize*SizeOf(Pointer);
  PPreservedCodeMemory(FMemory).ParamStackBase := FParameterStack;
  PPreservedCodeMemory(FMemory).ParamStackSize := FParameterStackSize;
  SP := Integer(FParameterStack) + FParameterStackSize;

  //FReturnStack := Prog.Stack;
  //FReturnStackSize := Prog.StackSize*SizeOf(Pointer);
  PPreservedCodeMemory(FMemory).ReturnStackBase := FReturnStack;
  PPreservedCodeMemory(FMemory).ReturnStackSize := FReturnStackSize;
  RP := Integer(FReturnStack) + FReturnStackSize;
end;

procedure TCustomTurboExecutor.Reset;
begin
  SP := Integer(FParameterStack) + FParameterStackSize;
  RP := Integer(FReturnStack) + FReturnStackSize;
  PPreservedCodeMemory(FMemory).LastErrorCode := errNone;
end;

procedure TCustomTurboExecutor.SetIsLoaded(const Value: Boolean);
begin
  if not Assigned(FMemory) then exit;
  if Value then
    Include(PPreservedCodeMemory(FMemory).States, psLoaded)
  else
    Exclude(PPreservedCodeMemory(FMemory).States, psLoaded);
end;

procedure TCustomTurboExecutor.SetMemorySize(Value: Integer);
begin
  if FMemorySize <> Value then
  begin
    FMemorySize := Value;
    ReallocMem(FMemory, FMemorySize);
  end;
end;

procedure TCustomTurboExecutor.SetStatus(Value: TTurboForthProcessorStates);
begin
  PPreservedCodeMemory(FMemory).States := Value;
end;

procedure TCustomTurboExecutor.SetTIB(Value: string);
var
  I: Integer;
begin
  if Value <> '' then
  begin
    i := Length(Value);
    if i >= cMAXTIBCount then
    begin
      i := cMAXTIBCount-1;
      SetLength(Value, i);
      Value := Value + #0;
    end;
    Move(PChar(Value)^, PPreservedCodeMemory(FMemory).TIB, i+1);
    PPreservedCodeMemory(FMemory).ToIN := 0;
    PPreservedCodeMemory(FMemory).TIBLength := i;
  end
  else begin
    PPreservedCodeMemory(FMemory).TIB[0] := #0;
    PPreservedCodeMemory(FMemory).TIBLength := 0;
  end;
end;

procedure TCustomTurboExecutor.Stop;
begin
  Exclude(PPreservedCodeMemory(FMemory).States, psRunning);
end;

{
******************************** TTurboProgram *********************************
}
constructor TTurboProgram.Create;
begin
  inherited Create;
end;

destructor TTurboProgram.Destroy;
begin
  FreeMem(FStack);
  FStack := nil;
  FreeMem(FParameterStack);
  FParameterStack := nil;
  inherited Destroy;
end;

procedure TTurboProgram.Execute(aTimeOut : Integer = 0);
begin
end;

procedure TTurboProgram.SetExecutor(const Value: TCustomTurboExecutor);
begin
  if FExecutor <> Value then
  begin
  //ff
    FExecutor := Value;
  end;
end;

procedure TTurboProgram.SetParameterStackSize(const Value: Integer);
begin
  if not (psRunning in Status) and (FParameterStackSize <> Value) then
  begin
    FParameterStackSize := Value;
    ReallocMem(FParameterStack, (FParameterStackSize+1)*SizeOf(Pointer));
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboProgram.SetStackSize(const Value: Integer);
begin
  if not (psRunning in Status) and (FStackSize <> Value) then
  begin
    FStackSize := Value;
    ReallocMem(FStack, (FStackSize+1)*SizeOf(Pointer));
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboProgram.Stop;
begin
  //sF
end;


end.
