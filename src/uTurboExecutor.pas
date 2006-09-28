{: The abstract super script executor module. }
{ Description

记住这里的堆栈(采用X86的堆栈规则)： 压入则是地址减少，弹出则是地址增加
When an item is pushed onto the stack, the processor decrements the ESP
register, then writes the item at the new top of stack. When an item is popped
off the stack, the
processor reads the item from the top of stack, then increments the ESP register


也可以脱离 TTurboProgam 运行，但是你必须分配参数栈＆返回栈的内存：
  ParameterStackSize := 4096;
  GetMem(ParameterStack, ParameterStackSize);
  ReturnStackSize := 4096;
  GetMem(ReturnStack, ReturnStackSize);
}
unit uTurboExecutor;

interface

{$I TurboScript.inc}

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
  重定位地址:
   1.Import表中的模块(DLL或ForthDLL模块)函数的地址
   2.Relocation表：绝对地址表项（需要重新计算的）
  }
  TCustomTurboPEFormat = class(TObject)
  private
    FImageBase: Integer;
  protected
    ExportTable: Integer;
    {: 程序所使用的模块中的函数 }
    { Description
    表述Import表中的模块(DLL或ForthDLL模块)
    }
    ImportTable: Integer;
    {: 重定位地址表 }
    { Description
    保存于文件中的绝对基地址总是ImageBase($0000)
    So 新的绝对地址应该是: OrgAddress - ImageBase + @CodeArea[0]
    }
    RelocationTable: Pointer;
  public
    {: 预设的代码基地址 }
    property ImageBase: Integer read FImageBase write FImageBase;
  end;

  {: the abstract turbo script executor. }
  { Description
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
  TCustomTurboExecutor = class(TCustomTurboObject)
  private
    FAccessor: TTurboModuleAccessor;
    FIsLoaded: Boolean;
    function GetLastErrorCode: TTurboForthProcessorErrorCode;
    function GetModuleType: TTurboModuleType;
    function GetStatus: TTurboForthProcessorStates;
    function GetTIB: string;
    procedure SetMemorySize(Value: Integer);
    procedure SetModuleType(Value: TTurboModuleType);
    procedure SetStatus(Value: TTurboForthProcessorStates);
    procedure SetTIB(Value: string);
  protected
    {: The Code Memory }
    FMemory: Pointer;
    FMemorySize: Integer;
    FModuleDate: TTimeStamp;
    FModuleUnloadNotifies: TList;
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
    返回堆栈
    }
    FReturnStack: Pointer;
    {: : Return(Proc) Stack }
    { Description
    返回堆栈
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
    {: Finalize after the execution. }
    procedure FinalizeExecution; virtual;
    procedure Grow;
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function iExecuteCFA(const aCFA: Integer): Integer; virtual;
    {: Init before the Execution. }
    procedure InitExecution; virtual;
    procedure SendUnloadNotification;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddIntToMem(const aInt: Integer);
    {: reduce the memory size to initialization state }
    procedure ClearMemory;
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function ExecuteCFA(const aCFA: Integer): Integer;
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function ExecuteWord(const aWord: string): Integer;
    function FindUnloadNotification(aProc: TNotifyEvent): Integer;
    function GetWordCFA(const aWord: string): Integer; virtual;
    {: Load the body into the memory. }
    { Description
    Note: accessor must be assigned first.
    }
    procedure Load;
    {: Load the VM Module from stream. }
    { Description
    @param Count 0 means all.
    }
    procedure LoadFromStream(const aStream: TStream; Count: Integer = 0);
    {: //triggered when some module is free, remove if from the UnloadNotifies list. }
    procedure NotifyModuleFree(Sender: TObject);
    {: //triggered when some module is unloaded  }
    procedure NotifyModuleUnloaded(Sender: TObject);
    procedure RemoveUnloadNotification(aProc: TNotifyEvent);
    {: find and load the module into memory. }
    { Description
    add self to the module Unload notification.
    }
    function RequireModule(const aModuleName: ShortString):
            TCustomTurboExecutor;
    {: reset the stack pointers. }
    procedure Reset;
    {: save FMemory to stream }
    procedure SaveToStream(const aStream: TStream);
    procedure Stop;
    {: unload from memory }
    procedure Unload;
    {: Ensures that a object is notified that the executor is going to be
            unloaded. }
    procedure UnloadNotification(aProc: TNotifyEvent);
    property Accessor: TTurboModuleAccessor read FAccessor write FAccessor;
    { Description
    if not loaded, then only the name and some options loaded but the body(
    Memory).
    }
    property IsLoaded: Boolean read FIsLoaded write FIsLoaded;
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
    property ModuleType: TTurboModuleType read GetModuleType write
            SetModuleType;
    property ModuleVersion: LongWord read FModuleVersion write FModuleVersion;
    {: the module full name(include path: Module.SubModule.ModuleName) }
    property Name: string read FName write FName;
    property Options: TTurboScriptOptions read FOptions write FOptions;
    {: : the Parameter Stack }
    { Description
    指向栈底： @Stack[0]
    压入减小
    }
    property ParameterStack: Pointer read FParameterStack write FParameterStack;
    {: : the Parameter Stack }
    { Description
    指向栈底： @Stack[0]
    压入减小
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
    返回堆栈

    指向栈底： @Stack[0]
    压入减小
    }
    property ReturnStack: Pointer read FReturnStack write FReturnStack;
    {: : Return(Proc) Stack }
    { Description
    返回堆栈

    指向栈底： @Stack[0]
    压入减小
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
    {: 已经使用的内存 }
    { Description
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
    FReturnStack: Pointer;
    FReturnStackSize: Integer;
    FStatus: TTurboForthProcessorStates;
    procedure SetExecutor(const Value: TCustomTurboExecutor);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetReturnStackSize(const Value: Integer);
  protected
    FExecutor: TCustomTurboExecutor;
  public
    { Description
    为了在多个 executor 中管理共享数据栈＆返回栈以及运行参数。
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
    property ReturnStack: Pointer read FReturnStack write FReturnStack;
    {: : Return Stack Size }
    property ReturnStackSize: Integer read FReturnStackSize write
            SetReturnStackSize;
    {: the current status of the script. }
    property Status: TTurboForthProcessorStates read FStatus write FStatus;
  end;


  PPreservedCodeMemory = ^ TPreservedCodeMemory;
  PTurboVariableEntry = ^ TTurboVariableEntry;
  PTurboModuleEntry = ^ TTurboModuleEntry;
  PTurboWordEntry = ^ TTurboWordEntry;
  PTurboSymbolEntry = ^ TTurboSymbolEntry;

  //For type-cast the Mem
  TTurboWordEntry = packed record
    Prior: PTurboWordEntry; //前一个单词 0 means 为最前面。

    Options: TTurboForthWordOptions;
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord;
    {NameLen: Byte;
    Name: array [0..255] of char;}
    Name: ShortString; //packed
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //其实就是直接指向的某个单词的PFA，不过那个单词的PFA就是直接执行的机器码而已。
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //大多数情况下是PForthWord，但是少数情况下是数据或VM Codes
  end;

  TTurboVariableEntry = packed record
    Prior: PTurboVariableEntry; 
  end;
  
  TTurboModuleEntry = packed record
    Prior: PTurboModuleEntry; //nil means no more
    //ModuleIndex: integer;
    Module: TCustomTurboExecutor; //nil means not assigned(or loaded).
    Name: ShortString; //packed string, the full module name with path.
  end;

  TTurboSymbolEntry = packed record
    Prior: PTurboSymbolEntry; //nil means no more
  end;

  //the typecast for code memory area to get the parameters
  TPreservedCodeMemory = packed record
    States: TTurboForthProcessorStates;
    Executor: TCustomTurboExecutor;
    //##abondoned:this Module unique Index in this program, allocated by compiler.
    //##ModuleIndex: Integer;
    ModuleType: TTurboModuleType;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ParamStackBottom: Pointer;
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    ReturnStackBottom: Pointer;
    ToIn: Integer; //>IN the text buffer current index
    TIBLength: Integer; //#TIB the text buffer length
    TIB: array [0..cMAXTIBCount-1] of char; //'TIB
    LastErrorCode: TTurboForthProcessorErrorCode;
    //如果ModuleType是模块，那么就是装载运行该模块前执行的初始化过程，入口地址
    //如果是函数，则是该函数的入口地址
    InitializeProc: Pointer; 
    FinalizeProc: Pointer; //如果是模块的话
    //last Used(import) module entry.
    LastModuleEntry: PTurboModuleEntry;
    //有名字的函数链表，指向最后一个函数入口。
    LastWordEntry: PTurboWordEntry;
    //有名字的变量链表
    LastVariableEntry: PTurboVariableEntry;
    //RTTI 符号表 链表
    LastSymbolEntry: PTurboSymbolEntry;
  end;
  
  TTurboModuleStreamHeader = packed record
    Id: array [0..cFORTHHeaderMagicIdLen-1] of char;
    Version: LongWord;
    BuildDate: TTimeStamp;
  end;
  
procedure TurboConvertAddrRelatedToAbsolute(Mem: PPreservedCodeMemory);
procedure TurboConvertAddrAbsoluteToRelated(Mem: PPreservedCodeMemory);

implementation

uses
  uTurboScriptAccessor;

{
***************************** TCustomTurboExecutor *****************************
}
constructor TCustomTurboExecutor.Create;
begin
  inherited Create;
  FModuleUnloadNotifies := TList.Create;

  ClearMemory;
end;

destructor TCustomTurboExecutor.Destroy;
begin
  Unload;

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

procedure TCustomTurboExecutor.ClearMemory;
begin
  MemorySize := SizeOf(TPreservedCodeMemory) + cDefaultFreeMemSize;
  FUsedMemory := SizeOf(TPreservedCodeMemory);

  with PPreservedCodeMemory(FMemory)^ do
  begin
    LastWordEntry := nil;
    LastVariableEntry := nil;
    LastSymbolEntry := nil;
    LastModuleEntry := nil;
    States := [];
  end;

  IsLoaded := False;
end;

function TCustomTurboExecutor.ExecuteCFA(const aCFA: Integer): Integer;
begin
  InitExecution;
  Result := iExecuteCFA(aCFA);
  FinalizeExecution;
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
  //Apply the SP to TProgram.SP.
end;

function TCustomTurboExecutor.FindUnloadNotification(aProc: TNotifyEvent):
        Integer;
var
  ProcMethod: TMethod;
begin
  for Result := 0 to FModuleUnloadNotifies.Count div 2 - 1 do
  begin
    ProcMethod.Code := FModuleUnloadNotifies.Items[Result * 2];
    ProcMethod.Data := FModuleUnloadNotifies.Items[Result * 2 + 1];
    if (ProcMethod.Code = TMethod(aProc).Code) and (ProcMethod.Data = TMethod(aProc).Data) then
      Exit;
  end;
  Result := -1;
end;

function TCustomTurboExecutor.GetLastErrorCode: TTurboForthProcessorErrorCode;
begin
  Result := PPreservedCodeMemory(FMemory).LastErrorCode;
end;

function TCustomTurboExecutor.GetModuleType: TTurboModuleType;
begin
  Result := PPreservedCodeMemory(FMemory).ModuleType;
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

function TCustomTurboExecutor.iExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := -1;
  //if (psRunning in Status) then
    //raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);
end;

procedure TCustomTurboExecutor.InitExecution;
begin
  if not FIsLoaded then
    raise ETurboScriptError.CreateRes(@rsTurboScriptNotLoadedError);

  if (psRunning in Status) then
    raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);

  //MemorySize := cLastWordEntryOffset + cDefaultFreeMemSize;
  //FUsedMemory := cLastWordEntryOffset;
  {
  with PPreservedCodeMemory(FMemory)^ do
  begin
    Executor := Self;
    TIBLength := 0;
    ToIn := 0;
    TIB[0] := #0;
    LastErrorCode := errNone;
  end;
  //FParameterStack := Prog.ParameterStack;
  //FParamStackSize := Prog.ParameterStackSize*SizeOf(Pointer);
  PPreservedCodeMemory(FMemory).ParamStackBase := FParameterStack;
  PPreservedCodeMemory(FMemory).ParamStackSize := FParameterStackSize;
  //if SP = 0 then
    SP := Integer(FParameterStack) + FParameterStackSize;

  //FReturnStack := Prog.Stack;
  //FReturnStackSize := Prog.StackSize*SizeOf(Pointer);
  PPreservedCodeMemory(FMemory).ReturnStackBase := FReturnStack;
  PPreservedCodeMemory(FMemory).ReturnStackSize := FReturnStackSize;
  //if RP = 0 then
    RP := Integer(FReturnStack) + FReturnStackSize;
  //}

  Include(PPreservedCodeMemory(FMemory).States, psRunning);
end;

procedure TCustomTurboExecutor.Load;
begin
  if not IsLoaded and Assigned(FAccessor) then
  begin
    Accessor.LoadModule(Self);
  end;
end;

procedure TCustomTurboExecutor.LoadFromStream(const aStream: TStream; Count:
        Integer = 0);
var
  vHeader: TTurboModuleStreamHeader;
begin
  if Count <= 0 then
  begin
    Count := aStream.Size;
    aStream.Position := 0;
  end;

  if Count < (SizeOf(TPreservedCodeMemory)+ SizeOf(TTurboModuleStreamHeader)) then
    raise ETurboScriptError.CreateRes(@rsInvalidTurboScriptStreamError);

  aStream.ReadBuffer(vHeader, SizeOf(TTurboModuleStreamHeader));

  if vHeader.Id <> cFORTHHeaderMagicId then
    raise ETurboScriptError.CreateRes(@rsInvalidTurboScriptStreamError);

  ModuleVersion := vHeader.Version;
  ModuleDate := vHeader.BuildDate;

  MemorySize := Count;
  aStream.ReadBuffer(FMemory^, Count);
  Reset;
  TurboConvertAddrRelatedToAbsolute(FMemory);

  FIsLoaded := True;
end;

procedure TCustomTurboExecutor.NotifyModuleFree(Sender: TObject);
var
  I: Integer;
begin
  RemoveUnloadNotification(TCustomTurboExecutor(Sender).NotifyModuleUnloaded);
end;

procedure TCustomTurboExecutor.NotifyModuleUnloaded(Sender: TObject);
begin
  //TODO: apply the TTurboModuleEntry Executor to nil!!
end;

procedure TCustomTurboExecutor.RemoveUnloadNotification(aProc: TNotifyEvent);
var
  I: Integer;
begin
  i := FindUnloadNotification(aProc);
  if i >= 0 then
    FModuleUnloadNotifies.Delete(i);
end;

function TCustomTurboExecutor.RequireModule(const aModuleName: ShortString):
        TCustomTurboExecutor;
begin
  //Result := Lib.Require(aModuleName);
  if Assigned(Result) then
  begin
    Result.UnloadNotification(NotifyModuleUnloaded);
    FreeNotification(Result.NotifyModuleFree);
  end;
end;

procedure TCustomTurboExecutor.Reset;
begin
  PPreservedCodeMemory(FMemory).ParamStackBase := FParameterStack;
  PPreservedCodeMemory(FMemory).ParamStackSize := FParameterStackSize;

  SP := Integer(FParameterStack) + FParameterStackSize;
  RP := Integer(FReturnStack) + FReturnStackSize;
  PPreservedCodeMemory(FMemory).LastErrorCode := errNone;
  PPreservedCodeMemory(FMemory).Executor := Self;

  PPreservedCodeMemory(FMemory).States := [];
end;

procedure TCustomTurboExecutor.SaveToStream(const aStream: TStream);
var
  vHeader: TTurboModuleStreamHeader;
begin
  if not FIsLoaded then
    raise ETurboScriptError.CreateRes(@rsTurboScriptNotLoadedError);

  vHeader.Id := cFORTHHeaderMagicId;
  vHeader.Version := ModuleVersion;
  vHeader.BuildDate := ModuleDate;
  aStream.WriteBuffer(vHeader, SizeOf(TTurboModuleStreamHeader));
  TurboConvertAddrAbsoluteToRelated(FMemory);
  aStream.WriteBuffer(FMemory^, FUsedMemory);
  TurboConvertAddrRelatedToAbsolute(FMemory);
end;

procedure TCustomTurboExecutor.SendUnloadNotification;
var
  I: Integer;
  ProcMethod: TMethod;
  Proc: TNotifyEvent Absolute ProcMethod;
begin
  for I := 0 to FModuleUnloadNotifies.Count div 2 - 1 do
  begin
    ProcMethod.Code := FModuleUnloadNotifies.Items[I * 2];
    ProcMethod.Data := FModuleUnloadNotifies.Items[I * 2 + 1];
    Proc(Self);
  end;
  FModuleUnloadNotifies.Clear;
end;

procedure TCustomTurboExecutor.SetMemorySize(Value: Integer);
begin
  if FMemorySize <> Value then
  begin
    FMemorySize := Value;
    ReallocMem(FMemory, FMemorySize);
  end;
end;

procedure TCustomTurboExecutor.SetModuleType(Value: TTurboModuleType);
begin
  PPreservedCodeMemory(FMemory).ModuleType := Value;
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

procedure TCustomTurboExecutor.Unload;
begin
  if FIsLoaded then
  begin
    SendUnloadNotification;
    ClearMemory;
  end;
end;

procedure TCustomTurboExecutor.UnloadNotification(aProc: TNotifyEvent);
begin
  if FindUnloadNotification(aProc) < 0 then
  begin
    FModuleUnloadNotifies.Insert(0, Pointer(TMethod(aProc).Data));
    FModuleUnloadNotifies.Insert(0, Pointer(TMethod(aProc).Code));
  end;
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
  FreeMem(FReturnStack);
  FReturnStack := nil;
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

procedure TTurboProgram.SetReturnStackSize(const Value: Integer);
begin
  if not (psRunning in Status) and (FReturnStackSize <> Value) then
  begin
    FReturnStackSize := Value;
    ReallocMem(FReturnStack, (FReturnStackSize+1)*SizeOf(Pointer));
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboProgram.Stop;
begin
  //sF
end;


type
  PTurboEntry = ^ TTurboEntry;
  TTurboEntry = packed record
    Prior: Pointer;
  end;

procedure TurboConvertEntryRelatedToAbsolute(Mem: Pointer; aEntry: PTurboEntry);
{$IFDEF PUREPASCAL}
begin
  while aEntry.Prior <> nil do
  begin
    Integer(aEntry.Prior) := Integer(aEntry.Prior) + Integer(Mem);
    aEntry := aEntry.Prior;   
  end;
end;
{$ELSE PUREPASCAL}
asm
@@Loop:
  MOV  ECX, [EDX].TTurboEntry.Prior
  CMP  ECX, 0
  JE   @@exit

  ADD  ECX, EAX
  MOV  [EDX].TTurboEntry.Prior, ECX
  MOV  EDX, [ECX].TTurboEntry.Prior
  JMP  @@Loop
@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TurboConvertAddrRelatedToAbsolute(Mem: PPreservedCodeMemory);
{$IFDEF PUREPASCAL}
begin
  if Assigned(Mem.InitializeProc) then
    Inc(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Inc(Integer(Mem.FinalizeProc), Integer(Mem));

  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastWordEntry);  
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastModuleEntry);  
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastVariableEntry);  
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastSymbolEntry);  
end;
{$ELSE PUREPASCAL}
asm
  MOV  EDX, [EAX].TPreservedCodeMemory.InitializeProc
  CMP  EDX, 0
  JE   @@Skip
  ADD  EDX, EAX
  MOV  [EAX].TPreservedCodeMemory.InitializeProc, EDX
@@Skip:
  MOV  EDX, [EAX].TPreservedCodeMemory.FinalizeProc
  CMP  EDX, 0
  JE   @@Skip2
  ADD  EDX, EAX
  MOV  [EAX].TPreservedCodeMemory.FinalizeProc, EDX

@@Skip2:
  LEA  EDX, [EAX].TPreservedCodeMemory.LastWordEntry
  CALL TurboConvertEntryRelatedToAbsolute 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastModuleEntry
  CALL TurboConvertEntryRelatedToAbsolute 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastVariableEntry
  CALL TurboConvertEntryRelatedToAbsolute 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastSymbolEntry
  CALL TurboConvertEntryRelatedToAbsolute 
end;
{$ENDIF PUREPASCAL}

procedure TurboConvertEntryAbsoluteToRelated(Mem: Pointer; aEntry: PTurboEntry);
{$IFDEF PUREPASCAL}
var
  P: PTurboEntry;
begin
 
  while aEntry.Prior <> nil do
  begin
    p := aEntry.Prior;
    Integer(aEntry.Prior) := Integer(aEntry.Prior) - Integer(Mem);
    //aEntry.Prior := t;
    aEntry := P.Prior;   
  end;
end;
{$ELSE PUREPASCAL}
asm
  PUSH EBX
  MOV  ECX, [EDX]

  CMP  ECX, 0
  JE   @@exit

  MOV  EBX, ECX
  SUB  EBX, EAX
  MOV  [EDX], EBX

@@Loop:
  MOV  EDX, [ECX].TTurboEntry.Prior
  CMP  EDX, 0
  JE   @@exit
  MOV  EBX, EDX
  SUB  EDX, EAX
  MOV  [ECX].TTurboEntry.Prior, EDX
  MOV  ECX, EBX
  JMP  @@Loop
@@exit:
  POP  EBX
end;
{$ENDIF PUREPASCAL}

procedure TurboConvertAddrAbsoluteToRelated(Mem: PPreservedCodeMemory);
{$IFDEF PUREPASCAL}
begin
  if Assigned(Mem.InitializeProc) then
    Dec(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Dec(Integer(Mem.FinalizeProc), Integer(Mem));
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastWordEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastModuleEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastVariableEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastSymbolEntry);  
end;
{$ELSE PUREPASCAL}
asm
  MOV  EDX, [EAX].TPreservedCodeMemory.InitializeProc
  CMP  EDX, 0
  JE   @@Skip
  SUB  EDX, EAX
  MOV  [EAX].TPreservedCodeMemory.InitializeProc, EDX
@@Skip:
  MOV  EDX, [EAX].TPreservedCodeMemory.FinalizeProc
  CMP  EDX, 0
  JE   @@Skip2
  SUB  EDX, EAX
  MOV  [EAX].TPreservedCodeMemory.FinalizeProc, EDX

@@Skip2:
  LEA  EDX, [EAX].TPreservedCodeMemory.LastWordEntry
  CALL TurboConvertEntryAbsoluteToRelated 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastModuleEntry
  CALL TurboConvertEntryAbsoluteToRelated 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastVariableEntry
  CALL TurboConvertEntryAbsoluteToRelated 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastSymbolEntry
  CALL TurboConvertEntryAbsoluteToRelated 
end;
{$ENDIF PUREPASCAL}

end.
