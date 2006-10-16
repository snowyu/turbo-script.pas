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
  , uMeTypes
  , uTurboConsts
  ;

type
  PPreservedCodeMemory = ^ TPreservedCodeMemory;
  PTurboVariableEntry = ^ TTurboVariableEntry;
  PTurboModuleEntry = ^ TTurboModuleEntry;
  PTurboWordEntry = ^ TTurboWordEntry;
  PTurboTypeInfoEntry = ^ TTurboTypeInfoEntry;

  TCustomTurboPEFormat = class;
  TCustomTurboModule = class;
  TCustomTurboExecutor = class;
  TTurboProgram = class;
  TTurboPrintCharEvent = procedure(Sender: TCustomTurboExecutor; aChar: Char) of object;
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

  {: the abstract turbo script Module. }
  { Description
  没有执行机构/

  Load the script into memory
  }
  TCustomTurboModule = class(TCustomTurboObject)
  private
    FAccessor: TObject;
    FIsLoaded: Boolean;
    FParent: TCustomTurboModule;
    FVisibility: TTurboVisibility;
    function GetInitializeProc: Integer;
    function GetLastErrorCode: TTurboProcessorErrorCode;
    function GetLastWordEntry: PTurboWordEntry;
    function GetMemorySize: Integer;
    function GetModuleType: TTurboModuleType;
    function GetStatus: TTurboProcessorStates;
    function GetTIB: string;
    function GetUsedMemory: Integer;
    procedure SetLastWordEntry(const Value: PTurboWordEntry);
    procedure SetMemorySize(Value: Integer);
    procedure SetModuleType(Value: TTurboModuleType);
    procedure SetStatus(Value: TTurboProcessorStates);
    procedure SetTIB(Value: string);
    procedure SetUsedMemory(Value: Integer);
  protected
    {: The Code Memory }
    FMemory: Pointer;
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
    procedure Grow(const aSize: Integer = 0);
    procedure SendUnloadNotification;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddBufferToMem(const aValue; aSize: Integer);
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddByteToMem(const aValue: Byte);
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddIntToMem(const aValue: Integer);
    procedure AllocSpace(const aSize: Integer);
    {: reduce the memory size to initialization state }
    procedure ClearMemory;
    function FindUnloadNotification(aProc: TNotifyEvent): Integer;
    {: 0 means not found. }
    function GetWordCFA(const aWord: string): Integer;
    {: Load the body into the memory. }
    { Description
    Note: accessor must be assigned first.
    }
    procedure Load;
    procedure LoadFromFile(const aFileName: String);
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
    function RequireModule(const aModuleName: ShortString): TCustomTurboModule;
    {: reset the stack pointers. }
    procedure Reset;
    procedure SaveToFile(const aFileName: String);
    {: save FMemory to stream }
    procedure SaveToStream(const aStream: TStream);
    {: 是否被存放在Parent 中. }
    function StoredInParent: Boolean;
    {: unload from memory }
    procedure Unload;
    {: Ensures that a object is notified that the executor is going to be
            unloaded. }
    procedure UnloadNotification(aProc: TNotifyEvent);
    {: the TurboModuleAccessor }
    property Accessor: TObject read FAccessor write FAccessor;
    property InitializeProc: Integer read GetInitializeProc;
    { Description
    if not loaded, then only the name and some options loaded but the body(
    Memory).
    }
    property IsLoaded: Boolean read FIsLoaded write FIsLoaded;
    property LastErrorCode: TTurboProcessorErrorCode read GetLastErrorCode;
    property LastWordEntry: PTurboWordEntry read GetLastWordEntry write
            SetLastWordEntry;
    {: The Code Memory }
    property Memory: Pointer read FMemory write FMemory;
    {: : the Memory Size. }
    { Description
    warining: if in the running status, you may be get trouble!!
    }
    property MemorySize: Integer read GetMemorySize write SetMemorySize;
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
    {: 它的父亲:被 Parser 或Tree使用. nil means root. }
    { Description
    如果该模块是私有的,那么内存就是使用的父亲的内存.
    }
    property Parent: TCustomTurboModule read FParent write FParent;
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
    property Status: TTurboProcessorStates read GetStatus write SetStatus;
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
    property UsedMemory: Integer read GetUsedMemory write SetUsedMemory;
    property Visibility: TTurboVisibility read FVisibility write FVisibility;
  end;

  {: the abstract turbo script executor. }
  { Description
  没有执行机构/

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
  TCustomTurboExecutor = class(TCustomTurboModule)
  protected
    FOnPrintChar: TTurboPrintCharEvent;
    procedure DoPrintChar(aChar: Char); virtual;
    {: Finalize after the execution. }
    procedure FinalizeExecution; virtual;
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function iExecuteCFA(const aCFA: Integer): Integer; virtual;
    {: Init before the Execution. }
    procedure InitExecution; virtual;
  public
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
    procedure Stop;
    property OnPrintChar: TTurboPrintCharEvent read FOnPrintChar write
            FOnPrintChar;
  end;

  TTurboProgram = class(TObject)
  private
    FOptions: TTurboScriptOptions;
    FParameterStack: Pointer;
    FParameterStackSize: Integer;
    FReturnStack: Pointer;
    FReturnStackSize: Integer;
    FStatus: TTurboProcessorStates;
    procedure SetExecutor(const Value: TCustomTurboModule);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetReturnStackSize(const Value: Integer);
  protected
    FExecutor: TCustomTurboModule;
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
    property Executor: TCustomTurboModule read FExecutor write SetExecutor;
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
    property Status: TTurboProcessorStates read FStatus write FStatus;
  end;



  TTurboExteralWordPFA = packed record
    ParamCount: Integer; 
    TypeInfo: PTurboTypeInfoEntry;  //nil means no RTTI info. the address is related.
  end;
  //For type-cast the Mem
  TTurboWordEntry = packed record
    Prior: PTurboWordEntry; //前一个单词 0 means 为最前面。

    Options: TTurboWordOptions;
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord;
    Name: ShortString; //packed
    {if CallStyle <> csForth then //external procedure
      //PFA: TTurboExteralWordPFA
      //ForStack 以Integer为单位的，不是真正意义上的参数个数
      //这些参数将从数据栈弹出
      ParamCount: Integer; 
      TypeInfo: PTuroboTypeInfoEntry;  //nil means no RTTI info. the address is related.
    }
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //其实就是直接指向的某个单词的PFA，不过那个单词的PFA就是直接执行的机器码而已。
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //大多数情况下是PForthWord，但是少数情况下是数据或VM Codes
  end;

  TTurboVariableEntry = packed record
    Prior: PTurboVariableEntry; 
    Addr: Pointer;
    Size: Integer;
    TypeInfo: PTurboTypeInfoEntry; //TODO: relocate it.
    Name: ShortString;//packed
  end;
  
  TTurboModuleEntry = packed record
    Prior: PTurboModuleEntry; //nil means no more
    //ModuleIndex: integer;
    Module: TCustomTurboModule; //nil means not assigned(or loaded).
    Name: ShortString; //packed string, the full module name with path.
  end;

  TTurboTypeInfoEntry = packed record
    Prior: PTurboTypeInfoEntry; //nil means no more
    //## abondoned following fields are TypeInfo: PMeType
    //## MeType: Pointer; //PMeType(@TTurboSymbolEntry.MeType) 
    TypeKind: TMeTypeKind;
    Name: ShortString; //packed ; maybe nil.
    //Others Fields
  end;

  //the typecast for code memory area to get the parameters
  TPreservedCodeMemory = packed record
    States: TTurboProcessorStates;
    Executor: TCustomTurboModule;
    //##abondoned:this Module unique Index in this program, allocated by compiler.
    //##ModuleIndex: Integer;
    ModuleType: TTurboModuleType;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ParamStackBottom: Pointer;
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    ReturnStackBottom: Pointer;
    UsedMemory: Integer;
    MemorySize: Integer; 
    ToIn: Integer; //>IN the text buffer current index
    TIBLength: Integer; //#TIB the text buffer length
    TIB: array [0..cMAXTIBCount-1] of char; //'TIB
    LastErrorCode: TTurboProcessorErrorCode;
    //如果ModuleType是模块，那么就是装载运行该模块前执行的初始化过程，入口地址
    //如果是函数，则是该函数的入口地址
    InitializeProc: Pointer; //it is the offset address of the FMemory
    FinalizeProc: Pointer; //如果是模块的话
    //last Used(import) module entry.
    LastModuleEntry: PTurboModuleEntry;
    //有名字的函数链表，指向最后一个函数入口。
    LastWordEntry: PTurboWordEntry;
    //有名字的变量链表
    LastVariableEntry: PTurboVariableEntry;
    //RTTI TypeInfo 链表
    LastTypeInfoEntry: PTurboTypeInfoEntry;
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
  uTurboAccessor;

{
****************************** TCustomTurboModule ******************************
}
constructor TCustomTurboModule.Create;
begin
  inherited Create;
  FModuleUnloadNotifies := TList.Create;

  ClearMemory;
end;

destructor TCustomTurboModule.Destroy;
begin
  Unload;

  FreeMem(FMemory);
  FMemory := nil;
  inherited Destroy;
end;

procedure TCustomTurboModule.AddBufferToMem(const aValue; aSize: Integer);
var
  p: Pointer;
begin
  if PPreservedCodeMemory(FMemory).UsedMemory >= MemorySize then
    Grow(aSize);
  Integer(p) := Integer(FMemory) + PPreservedCodeMemory(FMemory).UsedMemory;
  Move(aValue, p^, aSize);
  Inc(PPreservedCodeMemory(FMemory).UsedMemory, aSize);
end;

procedure TCustomTurboModule.AddByteToMem(const aValue: Byte);
var
  p: Pointer;
begin
  if PPreservedCodeMemory(FMemory).UsedMemory >= MemorySize then
    Grow;
  Integer(p) := Integer(FMemory) + PPreservedCodeMemory(FMemory).UsedMemory;
  PByte(P)^ := aValue;
  Inc(PPreservedCodeMemory(FMemory).UsedMemory, SizeOf(Byte));
end;

procedure TCustomTurboModule.AddIntToMem(const aValue: Integer);
var
  p: Pointer;
begin
  if PPreservedCodeMemory(FMemory).UsedMemory >= MemorySize then
    Grow;
  Integer(p) := Integer(FMemory) + PPreservedCodeMemory(FMemory).UsedMemory;
  PInteger(P)^ := aValue;
  Inc(PPreservedCodeMemory(FMemory).UsedMemory, SizeOf(Integer));
end;

procedure TCustomTurboModule.AllocSpace(const aSize: Integer);
begin
  if (PPreservedCodeMemory(FMemory).UsedMemory+ aSize) >= MemorySize then
    Grow(aSize);

  Inc(PPreservedCodeMemory(FMemory).UsedMemory, aSize);
end;

procedure TCustomTurboModule.ClearMemory;
begin
  ReallocMem(FMemory, SizeOf(TPreservedCodeMemory));
  with PPreservedCodeMemory(FMemory)^ do
  begin
    MemorySize := SizeOf(TPreservedCodeMemory);
    UsedMemory := SizeOf(TPreservedCodeMemory);
  end;
  //MemorySize := SizeOf(TPreservedCodeMemory); //+ cDefaultFreeMemSize;

  with PPreservedCodeMemory(FMemory)^ do
  begin
    InitializeProc := nil;
    FinalizeProc := nil;
    LastWordEntry := nil;
    LastVariableEntry := nil;
    LastTypeInfoEntry := nil;
    LastModuleEntry := nil;
    States := [];
  end;

  IsLoaded := False;
end;

function TCustomTurboModule.FindUnloadNotification(aProc: TNotifyEvent):
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

function TCustomTurboModule.GetInitializeProc: Integer;
begin
  Result := Integer(PPreservedCodeMemory(FMemory)^.InitializeProc);
end;

function TCustomTurboModule.GetLastErrorCode: TTurboProcessorErrorCode;
begin
  Result := PPreservedCodeMemory(FMemory).LastErrorCode;
end;

function TCustomTurboModule.GetLastWordEntry: PTurboWordEntry;
begin
  Result := PPreservedCodeMemory(FMemory).LastWordEntry;
end;

function TCustomTurboModule.GetMemorySize: Integer;
begin
  Result := PPreservedCodeMemory(FMemory).MemorySize;
end;

function TCustomTurboModule.GetModuleType: TTurboModuleType;
begin
  Result := PPreservedCodeMemory(FMemory).ModuleType;
end;

function TCustomTurboModule.GetStatus: TTurboProcessorStates;
begin
  Result := PPreservedCodeMemory(FMemory).States;
end;

function TCustomTurboModule.GetTIB: string;
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

function TCustomTurboModule.GetUsedMemory: Integer;
begin
  Result := PPreservedCodeMemory(FMemory).UsedMemory;
end;

function TCustomTurboModule.GetWordCFA(const aWord: string): Integer;
begin
  Result := Integer(LastWordEntry);
  while (Result <> 0) do
  begin
    if PTurboWordEntry(Result).Name = aWord then
    begin
      Result := Result + SizeOf(Pointer) + SizeOf(TTurboWordOptions) + SizeOf(LongWord) + 1
        + Length(aWord) - Integer(FMemory);
      exit;
    end;
    Result := Integer(PTurboWordEntry(Result).Prior);
  end;
  Result := 0;
end;

procedure TCustomTurboModule.Grow(const aSize: Integer = 0);
var
  I: Integer;
begin
  I := MemorySize div 4;
  if I < aSize then I := aSize;
  MemorySize := MemorySize + I;
end;

procedure TCustomTurboModule.Load;
begin
  if not IsLoaded and Assigned(FAccessor) then
  begin
    TTurboModuleAccessor(FAccessor).LoadModule(Self);
  end;
end;

procedure TCustomTurboModule.LoadFromFile(const aFileName: String);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(aFileName, fmOpenRead and fmShareDenyNone);
  try
    LoadFromStream(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TCustomTurboModule.LoadFromStream(const aStream: TStream; Count:
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

  MemorySize := Count-SizeOf(TTurboModuleStreamHeader);
  aStream.ReadBuffer(FMemory^, MemorySize);
  Reset;
  TurboConvertAddrRelatedToAbsolute(FMemory);

  FIsLoaded := True;
  //WriteLn('loaded ok.');
end;

procedure TCustomTurboModule.NotifyModuleFree(Sender: TObject);
begin
  RemoveUnloadNotification(TCustomTurboModule(Sender).NotifyModuleUnloaded);
end;

procedure TCustomTurboModule.NotifyModuleUnloaded(Sender: TObject);
begin
  //TODO: apply the TTurboModuleEntry Executor to nil!!
end;

procedure TCustomTurboModule.RemoveUnloadNotification(aProc: TNotifyEvent);
var
  I: Integer;
begin
  i := FindUnloadNotification(aProc);
  if i >= 0 then
    FModuleUnloadNotifies.Delete(i);
end;

function TCustomTurboModule.RequireModule(const aModuleName: ShortString):
        TCustomTurboModule;
begin
  Result := GTurboModuleManager.Require(aModuleName, True);
  if Assigned(Result) then
  begin
    Result.UnloadNotification(NotifyModuleUnloaded);
    FreeNotification(Result.NotifyModuleFree);
  end;
end;

procedure TCustomTurboModule.Reset;
begin
  PPreservedCodeMemory(FMemory).ParamStackBase := FParameterStack;
  PPreservedCodeMemory(FMemory).ParamStackSize := FParameterStackSize;

  SP := Integer(FParameterStack) + FParameterStackSize;
  RP := Integer(FReturnStack) + FReturnStackSize;
  PPreservedCodeMemory(FMemory).LastErrorCode := errNone;
  PPreservedCodeMemory(FMemory).Executor := Self;

  PPreservedCodeMemory(FMemory).States := [];
end;

procedure TCustomTurboModule.SaveToFile(const aFileName: String);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TCustomTurboModule.SaveToStream(const aStream: TStream);
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
  aStream.WriteBuffer(FMemory^, UsedMemory);
  TurboConvertAddrRelatedToAbsolute(FMemory);
end;

procedure TCustomTurboModule.SendUnloadNotification;
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

procedure TCustomTurboModule.SetLastWordEntry(const Value: PTurboWordEntry);
begin
  PPreservedCodeMemory(FMemory).LastWordEntry := Value;
end;

procedure TCustomTurboModule.SetMemorySize(Value: Integer);
var
  vOld: Pointer;
begin
  if PPreservedCodeMemory(FMemory).MemorySize <> Value then
  begin
    PPreservedCodeMemory(FMemory).MemorySize := Value;
    vOld := FMemory;
    ReallocMem(FMemory, Value);
    if Integer(vOld) <> Integer(FMemory) then

  end;
end;

procedure TCustomTurboModule.SetModuleType(Value: TTurboModuleType);
begin
  PPreservedCodeMemory(FMemory).ModuleType := Value;
end;

procedure TCustomTurboModule.SetStatus(Value: TTurboProcessorStates);
begin
  PPreservedCodeMemory(FMemory).States := Value;
end;

procedure TCustomTurboModule.SetTIB(Value: string);
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

procedure TCustomTurboModule.SetUsedMemory(Value: Integer);
begin
  if Value < SizeOf(TPreservedCodeMemory) then
    Value  := SizeOf(TPreservedCodeMemory);
  PPreservedCodeMemory(FMemory).UsedMemory := Value;
end;

function TCustomTurboModule.StoredInParent: Boolean;
begin
  Result := Assigned(Parent) and
     ((Parent.ModuleType = mtFunction) or (Visibility <= fvPrivate));
end;

procedure TCustomTurboModule.Unload;
begin
  if FIsLoaded then
  begin
    SendUnloadNotification;
    ClearMemory;
  end;
end;

procedure TCustomTurboModule.UnloadNotification(aProc: TNotifyEvent);
begin
  if FindUnloadNotification(aProc) < 0 then
  begin
    FModuleUnloadNotifies.Insert(0, Pointer(TMethod(aProc).Data));
    FModuleUnloadNotifies.Insert(0, Pointer(TMethod(aProc).Code));
  end;
end;

{
***************************** TCustomTurboExecutor *****************************
}
procedure TCustomTurboExecutor.DoPrintChar(aChar: Char);
begin
  if Assigned(FOnPrintChar) then
    FOnPrintChar(Self, aChar);
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
  FreeMem(FReturnStack);
  FReturnStack := nil;
  FreeMem(FParameterStack);
  FParameterStack := nil;
  inherited Destroy;
end;

procedure TTurboProgram.Execute(aTimeOut : Integer = 0);
begin
end;

procedure TTurboProgram.SetExecutor(const Value: TCustomTurboModule);
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
    while Assigned(aEntry) and (aEntry.Prior <> nil) do
    begin
      Integer(aEntry.Prior) := Integer(aEntry.Prior) + Integer(Mem);
      aEntry := aEntry.Prior;   
    end;
end;
{$ELSE PUREPASCAL}
asm
  CMP  EDX, 0
  JE   @@exit
@@Loop:
  MOV  ECX, [EDX].TTurboEntry.Prior
  CMP  ECX, 0
  JE   @@exit

  ADD  ECX, EAX
  MOV  [EDX].TTurboEntry.Prior, ECX
  MOV  EDX, ECX
  JMP  @@Loop
@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TurboConvertVarEntryRelatedToAbsolute(Mem: Pointer; aEntry: PTurboVariableEntry);
{$IFDEF PUREPASCAL}
begin
    while Assigned(aEntry) and (aEntry.Prior <> nil) do
    begin
      Integer(aEntry.Prior) := Integer(aEntry.Prior) + Integer(Mem);
      if Assigned(aEntry.TypeInfo) then
        Integer(aEntry.TypeInfo) := Integer(aEntry.TypeInfo) + Integer(Mem);  
      aEntry := aEntry.Prior;   
    end;
end;
{$ELSE PUREPASCAL}
asm
  CMP  EDX, 0
  JE   @@exit
@@Loop:
  MOV  ECX, [EDX].TTurboEntry.Prior
  CMP  ECX, 0
  JE   @@exit

  ADD  ECX, EAX
  MOV  [EDX].TTurboEntry.Prior, ECX
  CMP  [EDX].TTurboVariableEntry.TypeInfo, 0
  JE   @@skip
  MOV  ECX, [EDX].TTurboVariableEntry.TypeInfo
  ADD  ECX, EAX
  MOV  [EDX].TTurboVariableEntry.TypeInfo, ECX
  MOV  ECX, [EDX].TTurboEntry.Prior
@@skip:  
  MOV  EDX, ECX
  JMP  @@Loop
@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TurboConvertAddrRelatedToAbsolute(Mem: PPreservedCodeMemory);
{$IFDEF PUREPASCAL}
begin
  {if Assigned(Mem.InitializeProc) then
    Inc(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Inc(Integer(Mem.FinalizeProc), Integer(Mem));
}
  //TODO: 
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastTypeInfoEntry);  
  TurboConvertVarEntryRelatedToAbsolute(Mem, Mem.LastVariableEntry);  
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastWordEntry);  
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastModuleEntry);  
end;
{$ELSE PUREPASCAL}
asm
{  MOV  EDX, [EAX].TPreservedCodeMemory.InitializeProc
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
}
  LEA  EDX, [EAX].TPreservedCodeMemory.LastTypeInfoEntry
  CALL TurboConvertEntryRelatedToAbsolute 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastVariableEntry
  CALL TurboConvertVarEntryRelatedToAbsolute 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastWordEntry
  CALL TurboConvertEntryRelatedToAbsolute 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastModuleEntry
  CALL TurboConvertEntryRelatedToAbsolute 


end;
{$ENDIF PUREPASCAL}

procedure TurboConvertEntryAbsoluteToRelated(Mem: Pointer; aEntry: PTurboEntry);
{$IFDEF PUREPASCAL}
var
  P: PTurboEntry;
begin
    while Assigned(aEntry) and (aEntry.Prior <> nil) do
    begin
      p := aEntry.Prior;
      Integer(aEntry.Prior) := Integer(aEntry.Prior) - Integer(Mem);
      //aEntry.Prior := t;
      aEntry := P.Prior;   
    end;
end;
{$ELSE PUREPASCAL}
asm
  CMP  EDX, 0
  JE   @@exit1

  PUSH EBX
  MOV  ECX, [EDX].TTurboEntry.Prior

  CMP  ECX, 0
  JE   @@exit

  MOV  EBX, ECX
  SUB  EBX, EAX
  MOV  [EDX].TTurboEntry.Prior, EBX

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
@@exit1:
end;
{$ENDIF PUREPASCAL}

procedure TurboConvertVarEntryAbsoluteToRelated(Mem: Pointer; aEntry: PTurboVariableEntry);
{$IFDEF PUREPASCAL}
var
  P: PTurboVariableEntry;
begin
    while Assigned(aEntry) and (aEntry.Prior <> nil) do
    begin
      p := aEntry.Prior;
      Integer(aEntry.Prior) := Integer(aEntry.Prior) - Integer(Mem);
      if Assign(aEntry.TypeInfo) then
        Integer(aEntry.TypeInfo) := Integer(aEntry.TypeInfo) - Integer(Mem);
      //aEntry.Prior := t;
      aEntry := P.Prior;   
    end;
end;
{$ELSE PUREPASCAL}
asm
  CMP  ECX, 0
  JE   @@exit1

  PUSH EBX
  MOV  ECX, [EDX].TTurboEntry.Prior

  CMP  ECX, 0
  JE   @@exit

  MOV  EBX, [EDX].TTurboVariableEntry.TypeInfo 
  CMP  EBX, 0
  JE   @@skip1
  SUB  EBX, EAX
  MOV  [EDX].TTurboVariableEntry.TypeInfo, EBX 
@@skip1:
  MOV  EBX, ECX
  SUB  EBX, EAX
  MOV  [EDX].TTurboEntry.Prior, EBX

@@Loop:
  MOV  EDX, [ECX].TTurboEntry.Prior
  CMP  EDX, 0
  JE   @@exit
  MOV  EBX, [ECX].TTurboVariableEntry.TypeInfo 
  CMP  EBX, 0
  JE   @@skip
  SUB  EBX, EAX
  MOV  [ECX].TTurboVariableEntry.TypeInfo, EBX 
@@skip:
  MOV  EBX, EDX
  SUB  EDX, EAX
  MOV  [ECX].TTurboEntry.Prior, EDX
  MOV  ECX, EBX
  JMP  @@Loop
@@exit:
  POP  EBX
@@exit1:
end;
{$ENDIF PUREPASCAL}

procedure TurboConvertAddrAbsoluteToRelated(Mem: PPreservedCodeMemory);
{$IFDEF PUREPASCAL}
begin
{
  if Assigned(Mem.InitializeProc) then
    Dec(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Dec(Integer(Mem.FinalizeProc), Integer(Mem));
}
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastWordEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastModuleEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastVariableEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastTypeInfoEntry);  
end;
{$ELSE PUREPASCAL}
asm
{
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
}
  LEA  EDX, [EAX].TPreservedCodeMemory.LastWordEntry
  CALL TurboConvertEntryAbsoluteToRelated 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastModuleEntry
  CALL TurboConvertEntryAbsoluteToRelated 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastVariableEntry
  CALL TurboConvertEntryAbsoluteToRelated 

  LEA  EDX, [EAX].TPreservedCodeMemory.LastTypeInfoEntry
  CALL TurboConvertEntryAbsoluteToRelated 
end;
{$ENDIF PUREPASCAL}

end.
