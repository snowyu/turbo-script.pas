{1 The abstract super script executor module. }
{{

记住这里的堆栈(采用X86的堆栈规则)： 压入则是地址减少，弹出则是地址增加
When an item is pushed onto the stack, the processor decrements the ESP
register, then writes the item at the new top of stack. When an item is popped
off the stack, the
processor reads the item from the top of stack, then increments the ESP register
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
  {: The current status of the turbo script }
  {
    @PARAM ssNotLoaded only the name and soem options loaded but the body(Memory) in not loaded yet.
  }
  TTurboScriptStatus = (ssNotLoaded, ssLoaded, ssRunning, ssPaused);

  TCustomTurboPEFormat = class;
  TCustomTurboExecutor = class;
  TTurboProgram = class;
  {1 the abstract Portable Executable File Format. }
  {{
  重定位地址:
   1.Import表中的模块(DLL或ForthDLL模块)函数的地址
   2.Relocation表：绝对地址表项（需要重新计算的）
  }
  TCustomTurboPEFormat = class(TObject)
  private
    FImageBase: Integer;
  protected
    ExportTable: Integer;
    {1 程序所使用的模块中的函数 }
    {{
    表述Import表中的模块(DLL或ForthDLL模块)
    }
    ImportTable: Integer;
    {1 重定位地址表 }
    {{
    保存于文件中的绝对基地址总是ImageBase($0000)
    So 新的绝对地址应该是: OrgAddress - ImageBase + @CodeArea[0]
    }
    RelocationTable: Pointer;
  public
    {1 预设的代码基地址 }
    property ImageBase: Integer read FImageBase write FImageBase;
  end;

  {1 the abstract turbo script executor. }
  {{
  Load the script into memory, and execute the script(maybe translate it into
  native language first).

  如果该模块引用了其他模块（函数，变量）怎么办？
  答；无所谓，这就是后期绑定做的事情。

  是否支持嵌套子模块？
  答：如果支持，那么就必须在这里加上Modules属性，这样即可实现。
  如果要调用嵌套子模块内的函数那么就要: Module.SubModule.Func.
  但是这样就增加装载的复杂度，加大时间，降低效率。
  我的想法则是：在文件中以相对子目录的形式实现子模块。当然还有数据库也是如法炮制。
  另外对于子模块装入的时候只装入名字，其他的主体则在执行时候按需要装入。
  我准备将模块装入保存机制，从Executor上分离： TTurboScriptAccessor。

  Note:为了避免重新计算地址，全部采用相对偏移量！
  }
  TCustomTurboExecutor = class(TObject)
  private
    function GetTIB: string;
    procedure SetMemorySize(Value: Integer);
    procedure SetTIB(const Value: string);
  protected
    {1 The Code Memory }
    FMemory: Pointer;
    FMemorySize: Integer;
    FModuleDate: TTimeStamp;
    FModuleType: TTurboScriptModuleType;
    FModuleVersion: LongWord;
    FName: string;
    FOptions: TTurboScriptOptions;
    {1 : the Parameter Stack }
    FParameterStack: Pointer;
    FPC: Integer;
    FProg: TTurboProgram;
    FRP: Integer;
    {1 the Parameter Stack(or data stack) Pointer }
    FSP: Integer;
    {1 : Return(Proc) Stack }
    {{
    返回堆栈
    }
    FStack: Pointer;
    FStatus: TTurboScriptStatus;
    {1 The Current TIB Index }
    {{
    FTIBIndex : Text[FTIBIndex]
    }
    FTextIndex: Integer;
    FUsedMemory: Integer;
    {1 : Run the CFA word. }
    {{
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function ExecuteCFA(const aCFA: Integer): Integer; virtual;
    {1 Init the Virtual Machine. }
    procedure InitExecution; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {1 : Run the Virtual Machine. }
    {{
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function ExecuteWord(const aWord: string): Integer;
    procedure FinalizeExecution; virtual;
    function GetWordCFA(const aWord: string): Integer; virtual;
    procedure Stop;
    {1 The Code Memory }
    property Memory: Pointer read FMemory write FMemory;
    {1 : the Memory Size. }
    {{
    warining: if in the running status, you may be get trouble!!
    }
    property MemorySize: Integer read FMemorySize write SetMemorySize;
    {{
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
    {1 : the Parameter Stack }
    {{
    指向栈底： @Stack[0]
    压入减小
    }
    property ParameterStack: Pointer read FParameterStack write FParameterStack;
    {1 : program counter. }
    {{
    program counter, which contains the address 
    in memory of the instruction that is the next 
    to be executed. 
    }
    property PC: Integer read FPC write FPC;
    property Prog: TTurboProgram read FProg write FProg;
    {1 : return stack pointer(TOS). }
    {{
    stack pointer, a register that points to the area 
    in memory utilized as the main return stack.

    the RP0-StackSize <= the stack memory < RP0.
    }
    property RP: Integer read FRP write FRP;
    {1 the Parameter Stack(or data stack) Pointer }
    property SP: Integer read FSP write FSP;
    {1 : Return(Proc) Stack }
    {{
    返回堆栈

    指向栈底： @Stack[0]
    压入减小
    }
    property Stack: Pointer read FStack write FStack;
    {1 the current status of the script. }
    property Status: TTurboScriptStatus read FStatus write FStatus;
    {1 the script source(TIB) }
    {{
    TIB: text input Buffer
    }
    property TIB: string read GetTIB write SetTIB;
    {1 已经使用的内存 }
    {{
    也就是指向最大的可用内存：
    从该地址起的内存未用：FMemory[UsedMemory] 
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
    FStatus: TTurboScriptStatus;
    procedure SetExecutor(const Value: TCustomTurboExecutor);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetStackSize(const Value: Integer);
  protected
    FExecutor: TCustomTurboExecutor;
  public
    constructor Create;
    destructor Destroy; override;
    {1 the turbo script program class }
    {{
    allocate the memory to return stack and param stack.
    }
    procedure Execute(aTimeOut : Integer = 0);
    {1 Stops the execution of the script program as soon as possible.  }
    {{
    Remember: If TTurboProgram is executing an external procedure the program
    won't stop until this procedure returns.
    }
    procedure Stop;
    property Executor: TCustomTurboExecutor read FExecutor write SetExecutor;
    property Options: TTurboScriptOptions read FOptions write FOptions;
    property ParameterStack: Pointer read FParameterStack write FParameterStack;
    property ParameterStackSize: Integer read FParameterStackSize write
            SetParameterStackSize;
    {1 :Return Stack }
    property Stack: Pointer read FStack write FStack;
    {1 : Return Stack Size }
    property StackSize: Integer read FStackSize write SetStackSize;
    {1 the current status of the script. }
    property Status: TTurboScriptStatus read FStatus write FStatus;
  end;


implementation

{
***************************** TCustomTurboExecutor *****************************
}
constructor TCustomTurboExecutor.Create;
begin
  inherited Create;
end;

destructor TCustomTurboExecutor.Destroy;
begin
  FreeMem(FMemory);
  FMemory := nil;
  FMemorySize := 0;
  inherited Destroy;
end;

function TCustomTurboExecutor.ExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := -1;
  if FStatus >= ssRunning then
    raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);
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

function TCustomTurboExecutor.GetTIB: string;
var
  I: Integer;
begin
  i := PInteger(@FMemory[cTIBLengthOffset])^;
  if i > 0 then
  begin
    SetLength(Result, i);
    Move(PChar(@FMemory[cTIBOffset])^, PChar(Result)^, i);
  end
  else
    Result := '';
end;

function TCustomTurboExecutor.GetWordCFA(const aWord: string): Integer;
begin
  Result := -1;
end;

procedure TCustomTurboExecutor.InitExecution;
begin
  if FStatus >= ssRunning then
    raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);
  //MemorySize := cLastWordEntryOffset + cDefaultFreeMemSize;
  //FUsedMemory := cLastWordEntryOffset;
  MemorySize := SizeOf(TPreservedCodeMemory) + cDefaultFreeMemSize;
  FUsedMemory := SizeOf(TPreservedCodeMemory);


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
  end;

  FParameterStack := Prog.ParameterStack;
  PPreservedCodeMemory(FMemory).ParamStackBase := FParameterStack;
  PPreservedCodeMemory(FMemory).ParamStackSize := Prog.ParameterStackSize*SizeOf(Pointer);
  SP := Integer(FParameterStack) + PPreservedCodeMemory(FMemory).ParamStackSize;

  FReturnStack := Prog.Stack;
  PPreservedCodeMemory(FMemory).ReturnStackBase := FReturnStack;
  PPreservedCodeMemory(FMemory).ReturnStackSize := Prog.StackSize*SizeOf(Pointer);
  RP := Integer(Stack) + PPreservedCodeMemory(FMemory).ReturnStackSize;
end;

procedure TCustomTurboExecutor.SetMemorySize(Value: Integer);
begin
  if FMemorySize <> Value then
  begin
    FMemorySize := Value;
    ReallocMem(FMemory, FMemorySize);
  end;
end;

procedure TCustomTurboExecutor.SetTIB(const Value: string);
var
  I: Integer;
begin
  if Value <> '' then
  begin
    i := Length(Value);
    if i >= cMAXTIBCount then i := cMAXTIBCount-1;
    Move(PChar(Value)^, FMemory[cTIBOffset], i);
    FMemory[cTIBOffset+i] := 0;
    PInteger(@FMemory[cToINOffset])^ := 0;
    PInteger(@FMemory[cTIBLengthOffset])^ := i;
  end
  else begin
    FMemory[cTIBOffset] := 0;
    PInteger(@FMemory[cTIBLengthOffset])^ := 0;
  end;
end;

procedure TCustomTurboExecutor.Stop;
begin
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
  if (Status < ssRunning) and (FParameterStackSize <> Value) then
  begin
    FParameterStackSize := Value;
    ReallocMem(FParameterStack, (FParameterStackSize+1)*SizeOf(Pointer));
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboProgram.SetStackSize(const Value: Integer);
begin
  if (Status < ssRunning) and (FStackSize <> Value) then
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
