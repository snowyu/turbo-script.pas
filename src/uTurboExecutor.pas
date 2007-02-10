{: The abstract super script executor module. }
{ Description

记住这里的堆栈(采用X86的堆栈规则)： 压入则是地址减少，弹出则是地址增加,
堆栈是悬在顶部，往下增长的。
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
  , TypInfo
  //, uMeTypes
  //, uMeProcType
  , uTurboConsts
  , uTurboMetaInfo
  ;

type
  {: the basic simple turbo types. }
  {
     @param ttSByte: Signed byte
     @param ttUByte: Unsigned byte
     @param ttSWord: Signed word
     @param ttUWord: Unsigned word
     @param ttSLong: Signed longword
     @param ttULong: Unsigned longword
     @param ttkString: ShortString
     @param ttkLString: AnsiString
     @param ttkWString: WideString
     @param ttkWChar: WideChar 
  }
  TTurboSimpleTypeKind = (
    ttkUnknown, ttkSByte, ttkUByte, ttkSWord, ttkUWord
    , ttkSLong, ttkULong, ttkQWord, ttkInt64
    , ttkSingle, ttkDouble, ttkExtended, ttkComp, ttkCurr
    , ttkVariant, ttkRecord, ttkArray, ttkDynArray, ttkClass, ttkObject 
    , ttkString, ttkLString, ttkMeString, ttkWString, ttkChar, ttkWChar
    , ttkMethod, ttkProcedure, ttkInterface, ttkPointer, ttkParam
    , ttkSet, ttkEnumeration
  );
  //TTurboSimpleTypes = array [TTurboSimpleTypeKind] of PMeType;

  TTurboGlobalOptions = record  //do not use the packed.
    States: TTurboProcessorStates;
    LastErrorCode: TTurboProcessorErrorCode;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ParamStackBottom: Pointer;
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    //if halt with errHalt then the ReturnStackBottom will be the Old RSP.   
    ReturnStackBottom: Pointer;
    //{:  放到系统单元库中？ 好处是移植自举，缺点是速度变慢吧。
    ErrorAddr: Pointer;
    ErrorProc: Pointer; 
    AssertErrorProc: Pointer;
    //The built-in I/O routines use InOutResult to store the value that 
    //the next call to the IOResult standard function will return.
    IOResult: tsInt;  
    //}
  end;


  //PTurboPreservedCodeMemory = ^ TTurboPreservedCodeMemory;
  PTurboPreservedDataMemory = ^ TTurboPreservedDataMemory;
  PTurboGlobalOptions = ^TTurboGlobalOptions;

  TCustomTurboPEFormat = class;
  TCustomTurboModule = class;
  TCustomTurboExecutor = class;
  TTurboProgram = class;
  TTurboModuleClass = class of TCustomTurboModule;
  TTurboExecutorClass = class of TCustomTurboExecutor;
  TTurboPrintCharEvent = procedure(Sender: TCustomTurboExecutor; aChar: Char) of object;
  TTurboPrintStringEvent = procedure(Sender: TCustomTurboExecutor; const aStr: String) of object;
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
    FOnReset: TNotifyEvent;
    function GetGlobalOptions: PTurboGlobalOptions;
    function GetLastModuleRefEntry: PTurboModuleRefEntry;
    function GetOptions: TTurboScriptOptions;
    procedure SetGlobalOptions(Value: PTurboGlobalOptions);
    procedure SetLastModuleRefEntry(Value: PTurboModuleRefEntry);
    procedure SetOptions(Value: TTurboScriptOptions);
  protected
    FAccessor: TObject;
    FDataMemory: Pointer;
    FIsLoaded: Boolean;
    FMaxDataMemorySize: tsInt;
    FMaxMemorySize: tsInt;
    {: The Code Memory }
    FMemory: Pointer;
    FModuleDate: TTimeStamp;
    FModuleUnloadNotifies: TList;
    FModuleVersion: LongWord;
    FName: string;
    FOwner: TCustomTurboModule;
    {: The Current TIB Index }
    { Description
    FTIBIndex : Text[FTIBIndex]
    }
    FTextIndex: Integer;
    FVisibility: TTurboVisibility;
    function GetDataMemorySize: tsInt;
    function GetInitializeProc: Integer;
    function GetLastErrorCode: TTurboProcessorErrorCode;
    function GetLastTypeInfoEntry: PTurboTypeInfoEntry;
    function GetLastVariableEntry: PTurboVariableEntry;
    function GetLastWordEntry: PTurboMethodEntry;
    function GetMemorySize: tsInt;
    function GetModuleType: TTurboModuleType;
    function GetParameterStack: Pointer;
    function GetParameterStackSize: Integer;
    function GetReturnStack: Pointer;
    function GetReturnStackSize: Integer;
    function GetRoot: TCustomTurboModule;
    function GetStatus: TTurboProcessorStates;
    function GetUsedDataSize: tsInt;
    function GetUsedMemory: tsInt;
    procedure Grow(const aSize: Integer = 0);
    procedure GrowData(const aSize: Integer = 0);
    procedure LoadUsedModules;
    procedure SendUnloadNotification;
    procedure SetDataMemorySize(Value: tsInt);
    procedure SetLastTypeInfoEntry(const Value: PTurboTypeInfoEntry);
    procedure SetLastVariableEntry(const Value: PTurboVariableEntry);
    procedure SetLastWordEntry(const Value: PTurboMethodEntry);
    procedure SetMemorySize(Value: tsInt);
    procedure SetModuleType(Value: TTurboModuleType);
    procedure SetOwner(const Value: TCustomTurboModule);
    procedure SetParameterStack(Value: Pointer);
    procedure SetParameterStackSize(Value: Integer);
    procedure SetReturnStack(Value: Pointer);
    procedure SetReturnStackSize(Value: Integer);
    procedure SetStatus(Value: TTurboProcessorStates);
    procedure SetUsedDataSize(Value: tsInt);
    procedure SetUsedMemory(Value: tsInt);
  public
    constructor Create(const aOwner: TCustomTurboModule = nil; aVisibility:
            TTurboVisibility = fvPublished); virtual;
    destructor Destroy; override;
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddBufferToData(const aValue; aSize: Integer);
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddBufferToMem(const aValue; aSize: Integer);
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddByteToData(const aValue: Byte);
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddByteToMem(const aValue: Byte);
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddIntToData(const aValue: Integer);
    {: add a integer to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddIntToMem(const aValue: Integer);
    {: add a OpCode to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddOpToMem(const aOpCode: TTurboVMInstruction);
    {: fill 0 to align the memory. }
    procedure AlignData;
    {: fill 0 to align the memory. }
    procedure AlignMem;
    procedure AllocDataSpace(const aSize: Integer);
    procedure AllocSpace(const aSize: Integer);
    {: reduce the memory size to initialization state }
    procedure ClearMemory;
    {: find used module name entry. }
    { Description
    nil means not found.
    }
    function FindModuleEntry(const aName: string): PTurboModuleRefEntry;
    {: nil means not found. }
    function FindTypeInfoEntry(const aName: string): PTurboTypeInfoEntry;
    function FindUnloadNotification(aProc: TNotifyEvent): Integer;
    {: nil means not found. }
    function FindVariableEntry(const aName: string): PTurboVariableEntry;
    {: nil means not found. }
    function FindWordEntry(const aName: string; const aCallStyle:
            TTurboCallStyle = csForth): PTurboMethodEntry;
    {: Get the Local forth word entry. }
    { Description
    Note: 0 means not found.
    }
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
    function RequireModule(const aModuleName: PChar): TCustomTurboModule;
    {: reset the stack pointers. }
    procedure Reset;
    procedure SaveToFile(const aFileName: String);
    {: save FMemory to stream }
    procedure SaveToStream(const aStream: TStream);
    {: 是否被存放在 Owner 中. }
    function StoredInOwner: Boolean;
    {: unload from memory }
    procedure Unload;
    {: Ensures that a object is notified that the executor is going to be
            unloaded. }
    procedure UnloadNotification(aProc: TNotifyEvent);
    {: the TurboModuleAccessor }
    property Accessor: TObject read FAccessor write FAccessor;
    property DataMemory: Pointer read FDataMemory write FDataMemory;
    {: : the Memory Size. }
    { Description
    warining: if in the running status, you may be get trouble!!
    }
    property DataMemorySize: tsInt read GetDataMemorySize write
            SetDataMemorySize;
    property GlobalOptions: PTurboGlobalOptions read GetGlobalOptions write
            SetGlobalOptions;
    property InitializeProc: Integer read GetInitializeProc;
    { Description
    if not loaded, then only the name and some options loaded but the body(
    Memory).
    }
    property IsLoaded: Boolean read FIsLoaded write FIsLoaded;
    property LastErrorCode: TTurboProcessorErrorCode read GetLastErrorCode;
    property LastModuleRefEntry: PTurboModuleRefEntry read
            GetLastModuleRefEntry write SetLastModuleRefEntry;
    property LastTypeInfoEntry: PTurboTypeInfoEntry read GetLastTypeInfoEntry
            write SetLastTypeInfoEntry;
    property LastVariableEntry: PTurboVariableEntry read GetLastVariableEntry
            write SetLastVariableEntry;
    property LastWordEntry: PTurboMethodEntry read GetLastWordEntry write
            SetLastWordEntry;
    {: The Code Memory }
    property Memory: Pointer read FMemory write FMemory;
    {: : the Memory Size. }
    { Description
    warining: if in the running status, you may be get trouble!!
    }
    property MemorySize: tsInt read GetMemorySize write SetMemorySize;
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
    property Options: TTurboScriptOptions read GetOptions write SetOptions;
    {: 它的父亲:被 Parser 或Tree使用. nil means root. }
    { Description
    如果该模块是私有的,那么内存就是使用的父亲的内存.
    }
    property Owner: TCustomTurboModule read FOwner write SetOwner;
    {: : the Parameter Stack }
    { Description
    指向栈底： @Stack[0]
    压入减小
    }
    property ParameterStack: Pointer read GetParameterStack write
            SetParameterStack;
    {: : the Parameter Stack }
    { Description
    指向栈底： @Stack[0]
    压入减小
    }
    property ParameterStackSize: Integer read GetParameterStackSize write
            SetParameterStackSize;
    {: : Return(Proc) Stack }
    { Description
    返回堆栈

    指向栈底： @Stack[0]
    压入减小
    }
    property ReturnStack: Pointer read GetReturnStack write SetReturnStack;
    {: : Return(Proc) Stack }
    { Description
    返回堆栈

    指向栈底： @Stack[0]
    压入减小
    }
    property ReturnStackSize: Integer read GetReturnStackSize write
            SetReturnStackSize;
    property Root: TCustomTurboModule read GetRoot;
    {: the current status of the script. }
    { Description
    the Memory is related address when the status is in the psConpiling
    until the status is not in the psConpiling.
    }
    property Status: TTurboProcessorStates read GetStatus write SetStatus;
    {: 已经使用的内存 }
    { Description
    也就是指向最大的可用内存：
    从该地址起的内存未用：FMemory[UsedMemory] 
    }
    property UsedDataSize: tsInt read GetUsedDataSize write SetUsedDataSize;
    {: 已经使用的内存 }
    { Description
    也就是指向最大的可用内存：
    从该地址起的内存未用：FMemory[UsedMemory] 
    }
    property UsedMemory: tsInt read GetUsedMemory write SetUsedMemory;
    property Visibility: TTurboVisibility read FVisibility write FVisibility;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
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
  TCustomTurboExecutor = class(TCustomTurboObject)
  private
    FOnPrintString: TTurboPrintStringEvent;
    procedure SetMemory(const Value: TCustomTurboModule);
  protected
    FMemory: TCustomTurboModule;
    FOnPrintChar: TTurboPrintCharEvent;
    FPC: Integer;
    FRP: Integer;
    {: the Parameter Stack(or data stack) Pointer }
    FSP: Integer;
    procedure DoPrintChar(aChar: Char); virtual;
    procedure DoPrintShortString(const aStr: ShortString);
    procedure DoPrintString(const aStr: String); virtual;
    procedure DoReset(Sender: TObject);
    {: Finalize after the execution. }
    procedure FinalizeExecution; virtual;
    function GetStatus: TTurboProcessorStates;
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function iExecuteCFA(const aCFA: Integer): Integer; virtual;
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function iExecuteExternalWord(const aWord: PTurboExteralMethodOptions;
            const aCallStyle: TTurboCallStyle): Integer;
    {: Init before the Execution. }
    procedure InitExecution; virtual;
    procedure SetStatus(Value: TTurboProcessorStates);
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
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function ExecuteWordEntry(const aWord: PTurboMethodEntry): Integer;
    procedure Stop;
    property Memory: TCustomTurboModule read FMemory write SetMemory;
    {: : program counter. }
    { Description
    program counter, which contains the address 
    in memory of the instruction that is the next 
    to be executed. 
    }
    property PC: Integer read FPC write FPC;
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
    { Description
    the Memory is related address when the status is in the psConpiling
    until the status is not in the psConpiling.
    }
    property Status: TTurboProcessorStates read GetStatus write SetStatus;
    property OnPrintChar: TTurboPrintCharEvent read FOnPrintChar write
            FOnPrintChar;
    property OnPrintString: TTurboPrintStringEvent read FOnPrintString write
            FOnPrintString;
  end;

  TTurboProgram = class(TCustomTurboModule)
  private
    FExecutorClass: TTurboExecutorClass;
    function GetExecutor: TCustomTurboExecutor;
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetReturnStackSize(const Value: Integer);
  protected
    FExecutor: TCustomTurboExecutor;
    FGlobalOptions: TTurboGlobalOptions;
    property Executor: TCustomTurboExecutor read GetExecutor;
  public
    { Description
    为了在多个 executor 中管理共享数据栈＆返回栈以及运行参数。
    }
    constructor Create(const aOwner: TCustomTurboModule = nil; aVisibility:
            TTurboVisibility = fvPublished); override;
    destructor Destroy; override;
    {: the turbo script program class }
    { Description
    allocate the memory to return stack and param stack.
    }
    procedure Execute(aTimeOut : Integer = 0);
    property ExecutorClass: TTurboExecutorClass read FExecutorClass write
            FExecutorClass;
    property ParameterStackSize: Integer read GetParameterStackSize write
            SetParameterStackSize;
    {: : Return Stack Size }
    property ReturnStackSize: Integer read GetReturnStackSize write
            SetReturnStackSize;
  end;



(*
  PTurboMethodTypeData = ^TTurboMethodTypeData;
  TTurboMethodTypeData = packed record
    MethodKind: TMethodKind;
    ParamCount: Byte;
    ParamList: array[0..1023] of Char;
    {ParamList: array[1..ParamCount] of
          record
            Flags: TParamFlags;
            TypeInfo: TTurboSimpleTypeKind or PTurboTypeInfoEntry;
            ParamName: ShortString;
          end;
     if MethodKind is function then 
       ResultType: ShortString}
  end;
  PTurboParamTypeData = ^TTurboParamTypeData;
  TTurboParamTypeData = packed record
    Flags: TParamFlags;
    TypeInfo: tsInt;//TTurboSimpleTypeKind or PTurboTypeInfoEntry
    ParamName: ShortString;
  end;
*)

  //the typecast for data memory area to get the parameters
  TTurboPreservedDataMemory = packed record
    Code: Pointer; //need relocate addr. point to the FMemory
    GlobalOptions: PTurboGlobalOptions;
    Executor: TCustomTurboExecutor;

    UsedMemory: tsInt;//实际使用的大小
    MemorySize: tsInt;//分配代码区的大小
    UsedDataSize: tsInt;
    DataSize: tsInt; 

    ModuleType: TTurboModuleType;
    ModuleOptions: TTurboScriptOptions;
    ModuleName: PShortString; //nil const, base name only.
    //if Module is Class then the ModuleParent is classParent
    //in LastModuleEntry 链表中
    ModuleParent: PTurboModuleRefInfo;

    //如果ModuleType是模块，那么就是装载运行该模块前执行的初始化过程，入口地址
    //如果是函数，则是该函数的入口地址
    InitializeProc: Pointer; //it is the offset address of the FMemory
    FinalizeProc: Pointer; //如果是模块的话
    //last Used(import) module entry.
    LastModuleRefEntry: PTurboModuleRefEntry;
    //有名字的函数链表，指向最后一个函数入口。
    LastWordEntry: PTurboMethodEntry;
    //有名字的变量链表
    LastVariableEntry: PTurboVariableEntry;
    //RTTI TypeInfo 链表
    LastTypeInfoEntry: PTurboTypeInfoEntry;
    //VMT: TTurboVirtualMethodTable; 实际上可以将 LastWordEntry 看做VMT! 暂时不管 
    //reserved: array [SizeOf() ] of byte; 
  end;

  //the typecast for code memory area to get the parameters
  //TTurboPreservedCodeMemory = packed record
    //Data: PTurboPreservedDataMemory; //point to the data memory. Or its the offset of the data in File .
  //end;
  
  TTurboModuleStreamHeader = packed record
    Id: array [0..cFORTHHeaderMagicIdLen-1] of char;
    Version: LongWord; //the file format version
    Revision: LongWord; //the file version
    BuildDate: TTimeStamp;
  end;
  
procedure TurboConvertAddrRelatedToAbsolute(Mem: Pointer; Data:
        PTurboPreservedDataMemory);
procedure TurboConvertAddrAbsoluteToRelated(Mem: Pointer; Data:
        PTurboPreservedDataMemory);
//remove registered types of this module
//procedure RemoveModuleTypes(const aModuleName: string);

//var 
  //SimpleTurboTypes: TTurboSimpleTypes;


implementation

uses
  uTurboAccessor;

const
  cCurrentModuleFileForamtVersionNo = 1; 
  
{
****************************** TCustomTurboModule ******************************
}
constructor TCustomTurboModule.Create(const aOwner: TCustomTurboModule = nil;
        aVisibility: TTurboVisibility = fvPublished);
begin
  inherited Create;
  FModuleUnloadNotifies := TList.Create;
  FOwner := aOwner;
  FVisibility := aVisibility;
  //FOptions := [soLoadOnDemand];
  ClearMemory;
end;

destructor TCustomTurboModule.Destroy;
begin
  Unload;

  FreeAndNil(FModuleUnloadNotifies);
  if not StoredInOwner then
  begin
    if ModuleType <> mtFunction then
      FreeMem(FDataMemory);
    FDataMemory := nil;
    FreeMem(FMemory);
    FMemory := nil;
  end;
  inherited Destroy;
end;

procedure TCustomTurboModule.AddBufferToData(const aValue; aSize: Integer);
var
  p: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if (UsedDataSize+aSize) > DataMemorySize then
      GrowData(aSize);
    Integer(p) := Integer(FDataMemory) + UsedDataSize;
    Move(aValue, p^, aSize);
    Inc(UsedDataSize, aSize);
  end;
end;

procedure TCustomTurboModule.AddBufferToMem(const aValue; aSize: Integer);
var
  p: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if (UsedMemory+aSize) > MemorySize then
      Grow(aSize);
    Integer(p) := Integer(FMemory) + UsedMemory;
    Move(aValue, p^, aSize);
    Inc(UsedMemory, aSize);
  end;
end;

procedure TCustomTurboModule.AddByteToData(const aValue: Byte);
var
  p: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if UsedDataSize >= DataSize then
      GrowData;
    Integer(p) := Integer(FDataMemory) + UsedDataSize;
    PByte(P)^ := aValue;
    Inc(UsedDataSize, SizeOf(Byte));
  end;
end;

procedure TCustomTurboModule.AddByteToMem(const aValue: Byte);
var
  p: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if UsedMemory >= MemorySize then
      Grow;
    Integer(p) := Integer(FMemory) + UsedMemory;
    PByte(P)^ := aValue;
    Inc(UsedMemory, SizeOf(Byte));
  end;
end;

procedure TCustomTurboModule.AddIntToData(const aValue: Integer);
var
  p: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if UsedDataSize >= DataSize then
      GrowData;
    Integer(p) := Integer(FDataMemory) + UsedDataSize;
    PInteger(P)^ := aValue;
    Inc(UsedDataSize, SizeOf(Integer));
  end;
end;

procedure TCustomTurboModule.AddIntToMem(const aValue: Integer);
var
  p: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if UsedMemory >= MemorySize then
      Grow;
    Integer(p) := Integer(FMemory) + UsedMemory;
    PInteger(P)^ := aValue;
    Inc(UsedMemory, SizeOf(Integer));
  end;
end;

procedure TCustomTurboModule.AddOpToMem(const aOpCode: TTurboVMInstruction);
var
  p: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if UsedMemory >= MemorySize then
      Grow;

    Integer(p) := Integer(FMemory) + UsedMemory;
    {
    PInteger(P)^ := Integer(aOpCode);
    Inc(PPreservedCodeMemory(FMemory).UsedMemory, SizeOf(Integer));
    }
    PTurboVMInstruction(P)^ := aOpCode;
    Inc(UsedMemory, SizeOf(TTurboVMInstruction));
  end;
end;

procedure TCustomTurboModule.AlignData;
var
  I: Integer;
begin
  I := (UsedDataSize + 3) and $FFFFFFFC;
  I := I - UsedDataSize;
  if I > 0 then AllocDataSpace(I);
end;

procedure TCustomTurboModule.AlignMem;
var
  I: Integer;
begin
  I := (UsedMemory + 3) and $FFFFFFFC;
  I := I - UsedMemory;
  if I > 0 then AllocSpace(I);
end;

procedure TCustomTurboModule.AllocDataSpace(const aSize: Integer);
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if (UsedDataSize + aSize) >= DataMemorySize then
      GrowData(aSize);

    Inc(UsedDataSize, aSize);
  end;
end;

procedure TCustomTurboModule.AllocSpace(const aSize: Integer);
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if (UsedMemory+ aSize) >= MemorySize then
      Grow(aSize);

    Inc(UsedMemory, aSize);
  end;
end;

procedure TCustomTurboModule.ClearMemory;
var
  vPreserved: Integer;
begin
  if not StoredInOwner then
  begin
    //vPreserved := SizeOf(TTurboPreservedDataMemory);
    //if vPreserved < Integer(High(TTurboVMInstruction)) then
        //vPreserved := Integer(High(TTurboVMInstruction));
    vPreserved := cDefaultDataMemSize+SizeOf(TTurboPreservedDataMemory);
    ReallocMem(FMemory, cDefaultDataMemSize);
    ReallocMem(FDataMemory, vPreserved);
    with PTurboPreservedDataMemory(FDataMemory)^ do
    begin
      MemorySize := cDefaultDataMemSize;
      UsedMemory := 0;
      Code := FMemory;
      DataSize := vPreserved;
      UsedDataSize := SizeOf(TTurboPreservedDataMemory);//SizeOf(tsInt); //preserved the first integer
      ModuleOptions := [soAssertSupport];

      InitializeProc := nil;
      FinalizeProc := nil;
      LastWordEntry := nil;
      LastVariableEntry := nil;
      LastTypeInfoEntry := nil;
      LastModuleRefEntry := nil;
      //States := [];
    end;

    IsLoaded := False;
  end;
end;

function TCustomTurboModule.FindModuleEntry(const aName: string):
        PTurboModuleRefEntry;
var
  vName: PChar;
begin
  Result := LastModuleRefEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.Module.Name;
    if Assigned(vName) then
    begin
      if psCompiling in Status then
      begin
        Integer(vName)  := Integer(FDataMemory) + Integer(vName);
      end;
      if vName = aName then
      begin
        Exit;
      end;
    end;
    Result := Result.Prior;
  end;
  Result := nil;
end;

function TCustomTurboModule.FindTypeInfoEntry(const aName: string):
        PTurboTypeInfoEntry;
var
  vName: PChar;
begin
  Result := LastTypeInfoEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.TypeInfo.Name;
    if Assigned(vName) then
    begin
      if psCompiling in Status then
      begin
        Integer(vName) := Integer(FDataMemory) + Integer(vName);
      end;
      if vName^ = aName then
      begin
        Exit;
      end;
    end;
    Result := Result.Prior;
  end;
  Result := nil;
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

function TCustomTurboModule.FindVariableEntry(const aName: string):
        PTurboVariableEntry;
var
  vName: PChar;
begin
  Result := LastVariableEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.Variable.Name;
    if Assigned(vName) then
    begin
      if psCompiling in Status then
      begin
        Integer(vName) := Integer(FDataMemory) + Integer(vName);
      end;
      if vName^ = aName then
      begin
        Exit;
      end;
    end;
    Result := Result.Prior;
  end;
  Result := nil;
end;

function TCustomTurboModule.FindWordEntry(const aName: string; const
        aCallStyle: TTurboCallStyle = csForth): PTurboMethodEntry;
var
  vName: PChar;
begin
  Result := LastWordEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.Word.Name;
    if Assigned(vName) then
    begin
      if psCompiling in Status then
      begin
        //writeln('psCompiling,', Integer(Result));
        Integer(vName) := Integer(FDataMemory) + Integer(vName);
        //break;
      end;
      {if Result.Name <> '' then
      begin
        writeln('FindWordEntry:', Result.Name);
        writeln('FindWordEntry:', Integer(Result.Options.CallStyle));
        writeln('aCallStyle:', Integer(aCallStyle));
      end;//}
      if (Result.Word.CallStyle = aCallStyle) and AnsiSameText(vName, aName) then
      begin
        Exit;
      end;
    end;
    Result := Result.Prior;
  end;
  Result := nil;
end;

function TCustomTurboModule.GetDataMemorySize: tsInt;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).DataSize;
end;

function TCustomTurboModule.GetGlobalOptions: PTurboGlobalOptions;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).GlobalOptions;
end;

function TCustomTurboModule.GetInitializeProc: Integer;
begin
  Result := Integer(PTurboPreservedDataMemory(FDataMemory)^.InitializeProc);
end;

function TCustomTurboModule.GetLastErrorCode: TTurboProcessorErrorCode;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.LastErrorCode
    else
      Result := errNone;
  end;
end;

function TCustomTurboModule.GetLastModuleRefEntry: PTurboModuleRefEntry;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).LastModuleRefEntry;
end;

function TCustomTurboModule.GetLastTypeInfoEntry: PTurboTypeInfoEntry;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).LastTypeInfoEntry;
end;

function TCustomTurboModule.GetLastVariableEntry: PTurboVariableEntry;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).LastVariableEntry;
end;

function TCustomTurboModule.GetLastWordEntry: PTurboMethodEntry;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).LastWordEntry;
end;

function TCustomTurboModule.GetMemorySize: tsInt;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).MemorySize;
end;

function TCustomTurboModule.GetModuleType: TTurboModuleType;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).ModuleType;
end;

function TCustomTurboModule.GetOptions: TTurboScriptOptions;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).ModuleOptions;
end;

function TCustomTurboModule.GetParameterStack: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.ParamStackBase
    else
      Result := nil;
end;

function TCustomTurboModule.GetParameterStackSize: Integer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.ParamStackSize
    else
      Result := 0;
end;

function TCustomTurboModule.GetReturnStack: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.ReturnStackBase
    else
      Result := nil;
end;

function TCustomTurboModule.GetReturnStackSize: Integer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.ReturnStackSize
    else
      Result := 0;
end;

function TCustomTurboModule.GetRoot: TCustomTurboModule;
begin
  Result := Owner;
  while Assigned(Result) and Assigned(Result.Owner) do
  begin
    Result := Result.Owner;
  end;

  if Result = nil then Result := Self;
end;

function TCustomTurboModule.GetStatus: TTurboProcessorStates;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.States
    else
      Result := [];
  end;
end;

function TCustomTurboModule.GetUsedDataSize: tsInt;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).UsedDataSize;
end;

function TCustomTurboModule.GetUsedMemory: tsInt;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).UsedMemory;
end;

function TCustomTurboModule.GetWordCFA(const aWord: string): Integer;
begin
  Result := Integer(FindWordEntry(aWord));
  if Result <> 0 then
  begin
    Result := PTurboMethodEntry(Result).Word.CFA;
  end
  else
    Result := -1;
  //  Result := Result + SizeOf(Pointer) + SizeOf(TTurboWordOptions) + SizeOf(LongWord) + 1
  //      + Length(aWord) - Integer(FMemory);
end;

procedure TCustomTurboModule.Grow(const aSize: Integer = 0);
var
  I: Integer;
begin
  I := MemorySize div 4;
  if I < aSize then I := aSize;
  MemorySize := MemorySize + I;
end;

procedure TCustomTurboModule.GrowData(const aSize: Integer = 0);
var
  I: Integer;
begin
  I := DataMemorySize div 4;
  if I < aSize then I := aSize;
  DataMemorySize := DataMemorySize + I;
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
  vOptions: Pointer;
  p: Pointer;
begin
  if Count <= 0 then
  begin
    Count := aStream.Size;
    aStream.Position := 0;
  end;

  if Count < (SizeOf(TTurboPreservedDataMemory)+ SizeOf(TTurboModuleStreamHeader)) then
    raise ETurboScriptError.CreateRes(@rsInvalidTurboScriptStreamError);

  aStream.ReadBuffer(vHeader, SizeOf(TTurboModuleStreamHeader));

  if vHeader.Id <> cFORTHHeaderMagicId then
    raise ETurboScriptError.CreateRes(@rsInvalidTurboScriptStreamError);
  ModuleVersion := vHeader.Revision;
  ModuleDate := vHeader.BuildDate;

  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    //backup the data in the Memory
    vOptions := GlobalOptions;
  end;

  UsedDataSize := SizeOf(TTurboPreservedDataMemory);
  aStream.ReadBuffer(FDataMemory^, SizeOf(TTurboPreservedDataMemory));
  ReallocMem(FDataMemory, DataMemorySize);
  //if UsedDataSize
  Integer(p) := Integer(FDataMemory) + SizeOf(TTurboPreservedDataMemory);
  aStream.ReadBuffer(p^, UsedDataSize-SizeOf(TTurboPreservedDataMemory));

  ReallocMem(FMemory, MemorySize);
  aStream.ReadBuffer(FMemory^, UsedMemory);

  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    //now restore the data
    GlobalOptions := vOptions;
  end;

  Reset;
  if not (psCompiling in Status) then TurboConvertAddrRelatedToAbsolute(FMemory, FDataMemory);

  if not (soLoadOnDemand in Options) then
  begin
    LoadUsedModules;
  end;

  FIsLoaded := True;
  //WriteLn('loaded ok.');
end;

procedure TCustomTurboModule.LoadUsedModules;
var
  vModuleRefEntry: PTurboModuleRefEntry;
  vModule: TCustomTurboModule;
begin
  vModuleRefEntry := LastModuleRefEntry;
  while (vModuleRefEntry <> nil) do
  begin
    if psCompiling in Status then
      Integer(vModuleRefEntry) := Integer(FDataMemory) + Integer(vModuleRefEntry);
    vModule := nil;
    case vModuleRefEntry.Module.ModuleType of
      mtLib: vModule := RequireModule(vModuleRefEntry.Module.Name);
    end; //case
    if Assigned(vModule) then
      vModuleRefEntry.Module.Handle := vModule;
    vModuleRefEntry := vModuleRefEntry.Prior;
  end;
end;

procedure TCustomTurboModule.NotifyModuleFree(Sender: TObject);
begin
  if Sender is TCustomTurboModule then
  begin
    RemoveUnloadNotification(TCustomTurboModule(Sender).NotifyModuleUnloaded);
    if Sender = FOwner then
      Self.Free;
  end;
end;

procedure TCustomTurboModule.NotifyModuleUnloaded(Sender: TObject);
var
  vModuleRef: PTurboModuleRefEntry;
begin
  if (Sender = FOwner) and StoredInOwner then
  begin
    FIsLoaded := False;
  end;

  vModuleRef := LastModuleRefEntry;
  while (vModuleRef <> nil) do
  begin
    if psCompiling in Status then
      Integer(vModuleRef) := Integer(FDataMemory) + Integer(vModuleRef);
    if vModuleRef.Module.Handle = Sender then
    begin
      vModuleRef.Module.Handle := nil;
      Exit;
    end
    else
      vModuleRef := vModuleRef.Prior;
  end;
end;

procedure TCustomTurboModule.RemoveUnloadNotification(aProc: TNotifyEvent);
var
  I: Integer;
begin
  i := FindUnloadNotification(aProc);
  if i >= 0 then
    FModuleUnloadNotifies.Delete(i);
end;

function TCustomTurboModule.RequireModule(const aModuleName: PChar):
        TCustomTurboModule;
begin
  Result := GTurboModuleManager.Require(aModuleName, TTurboModuleClass(ClassType), GlobalOptions, True);

  if Assigned(Result) then
  begin
    Result.UnloadNotification(NotifyModuleUnloaded);
    FreeNotification(Result.NotifyModuleFree);
  end
end;

procedure TCustomTurboModule.Reset;
begin
  //PPreservedCodeMemory(FMemory).ParamStackBase := FParameterStack;
  //PPreservedCodeMemory(FMemory).ParamStackSize := FParameterStackSize;

  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    //SP := Integer(GlobalOptions.ParamStackBase) + GlobalOptions.ParamStackSize;
    //RP := Integer(GlobalOptions.ReturnStackBase) + GlobalOptions.ReturnStackSize;
    GlobalOptions.LastErrorCode := errNone;
    Code := FMemory;
    //Executor := Self;
    //GlobalOptions.States := []; //不是你的东西就别动！！
  end;
  if Assigned(FOnReset) then FOnReset(Self);
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
  vOptions: Pointer;
begin
  if not FIsLoaded then
    raise ETurboScriptError.CreateRes(@rsTurboScriptNotLoadedError);

  vHeader.Id := cFORTHHeaderMagicId;
  vHeader.Revision := ModuleVersion;
  vHeader.Version  := cCurrentModuleFileForamtVersionNo;
  vHeader.BuildDate := ModuleDate;
  aStream.WriteBuffer(vHeader, SizeOf(TTurboModuleStreamHeader));
  if not (psCompiling in Status) then TurboConvertAddrAbsoluteToRelated(FMemory, FDataMemory);

  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    //backup the data in the Memory
    vOptions := GlobalOptions;
    GlobalOptions := nil;
    Code := nil;
    {vParameterStack := ParameterStack;
    vReturnStack := ReturnStack;
    vReturnStackSize := ReturnStackSize;
    vParameterStackSize := ParameterStackSize;
    ParameterStack := 0;
    ReturnStack := 0;
    ReturnStackSize := 0;
    ParameterStackSize := 0;}
    if FMaxDataMemorySize = -1 then
      DataSize := UsedDataSize;
    if FMaxMemorySize = -1 then
      MemorySize := UsedMemory;
  end;

  aStream.WriteBuffer(FDataMemory^, UsedDataSize);
  aStream.WriteBuffer(FMemory^, UsedMemory);
  //writeln('Save.UsedDataOff=',aStream.Position);
  //if UsedDataSize > 0 then
    //aStream.WriteBuffer(FDataMemory^, UsedDataSize);
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    GlobalOptions := vOptions;
    Code := FMemory;
  {  ParameterStack := vParameterStack;
    ReturnStack := vReturnStack;
    ReturnStackSize := vReturnStackSize;
    ParameterStackSize := vParameterStackSize;
  }
  end;

  if not (psCompiling in Status) then TurboConvertAddrRelatedToAbsolute(FMemory, FDataMemory);
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

procedure TCustomTurboModule.SetDataMemorySize(Value: tsInt);
var
  vOld: Pointer;
begin
  if Value = -1 then
    FMaxDataMemorySize := -1
  else
    FMaxDataMemorySize := Value;
  if (PTurboPreservedDataMemory(FDataMemory).DataSize <> Value)
    and (PTurboPreservedDataMemory(FDataMemory).UsedDataSize <= Value) then
  begin
    PTurboPreservedDataMemory(FDataMemory).DataSize := Value;
    vOld := FDataMemory;
    ReallocMem(FDataMemory, Value);
    //if Integer(vOld) <> Integer(FDataMemory) then
      //PPreservedCodeMemory(FMemory).Data := FDataMemory;
  end;
end;

procedure TCustomTurboModule.SetGlobalOptions(Value: PTurboGlobalOptions);
begin
  PTurboPreservedDataMemory(FDataMemory).GlobalOptions := Value;
end;

procedure TCustomTurboModule.SetLastModuleRefEntry(Value: PTurboModuleRefEntry);
begin
  PTurboPreservedDataMemory(FDataMemory).LastModuleRefEntry := Value;
end;

procedure TCustomTurboModule.SetLastTypeInfoEntry(const Value:
        PTurboTypeInfoEntry);
begin
  PTurboPreservedDataMemory(FDataMemory).LastTypeInfoEntry := Value;
end;

procedure TCustomTurboModule.SetLastVariableEntry(const Value:
        PTurboVariableEntry);
begin
  PTurboPreservedDataMemory(FDataMemory).LastVariableEntry := Value;
end;

procedure TCustomTurboModule.SetLastWordEntry(const Value: PTurboMethodEntry);
begin
  PTurboPreservedDataMemory(FDataMemory).LastWordEntry := Value;
end;

procedure TCustomTurboModule.SetMemorySize(Value: tsInt);
var
  vOld: Pointer;
begin
  if Value = -1 then
    FMaxMemorySize := -1
  else
    FMaxMemorySize := Value;
  if (PTurboPreservedDataMemory(FDataMemory).MemorySize <> Value) and
    (PTurboPreservedDataMemory(FDataMemory).UsedMemory <= Value) then
  begin
    PTurboPreservedDataMemory(FDataMemory).MemorySize := Value;
    vOld := FMemory;
    ReallocMem(FMemory, Value);
    if Integer(vOld) <> Integer(FMemory) then
    begin
      PTurboPreservedDataMemory(FDataMemory).Code := FMemory;
      //TODO: the base-address is changed. relocate the addresses.
    end;
  end;
end;

procedure TCustomTurboModule.SetModuleType(Value: TTurboModuleType);
begin
  PTurboPreservedDataMemory(FDataMemory).ModuleType := Value;
end;

procedure TCustomTurboModule.SetOptions(Value: TTurboScriptOptions);
begin
  PTurboPreservedDataMemory(FDataMemory).ModuleOptions := Value;
end;

procedure TCustomTurboModule.SetOwner(const Value: TCustomTurboModule);
begin
  if Value <> FOwner then
  begin
    if Assigned(FOwner) then
      FOwner.RemoveUnloadNotification(NotifyModuleUnloaded);
    FOwner := Value;
    if Assigned(FOwner) then
      FOwner.UnloadNotification(NotifyModuleUnloaded);
  end;
end;

procedure TCustomTurboModule.SetParameterStack(Value: Pointer);
begin
  PTurboPreservedDataMemory(FDataMemory).GlobalOptions.ParamStackBase := Value;
end;

procedure TCustomTurboModule.SetParameterStackSize(Value: Integer);
begin
  PTurboPreservedDataMemory(FDataMemory).GlobalOptions.ParamStackSize := Value;
end;

procedure TCustomTurboModule.SetReturnStack(Value: Pointer);
begin
  PTurboPreservedDataMemory(FDataMemory).GlobalOptions.ReturnStackBase := Value;
end;

procedure TCustomTurboModule.SetReturnStackSize(Value: Integer);
begin
  PTurboPreservedDataMemory(FDataMemory).GlobalOptions.ReturnStackSize := Value;
end;

procedure TCustomTurboModule.SetStatus(Value: TTurboProcessorStates);
var
  vChanged: Boolean;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    if Assigned(GlobalOptions) and (Value <> GlobalOptions.States) then
    begin
      vChanged := psCompiling in (Value * GlobalOptions.States);
      GlobalOptions.States := Value;
      if vChanged then
      begin
        if psCompiling in Value then
          TurboConvertAddrAbsoluteToRelated(FMemory, FDataMemory)
        else
          TurboConvertAddrRelatedToAbsolute(FMemory, FDataMemory);
      end;
    end;
  end;
end;

procedure TCustomTurboModule.SetUsedDataSize(Value: tsInt);
begin
  if Value < SizeOf(TTurboPreservedDataMemory) then
    Value  := SizeOf(TTurboPreservedDataMemory);

  if (Value > DataMemorySize) then
  begin
    if (psCompiling in Status) then
      GrowData(Value - DataMemorySize)
    else
      Raise Exception.Create('Out Of DataMemory');
  end;

  PTurboPreservedDataMemory(FDataMemory).UsedDataSize := Value;
end;

procedure TCustomTurboModule.SetUsedMemory(Value: tsInt);
begin
  //if Value < SizeOf(TTurboPreservedDataMemory) then
    //Value  := SizeOf(TTurboPreservedDataMemory);

  if Value > MemorySize then
  begin
    if (psCompiling in Status) then
      Grow(Value - MemorySize)
    else
      Raise Exception.Create('Out Of Memory');
  end;

  PTurboPreservedDataMemory(FDataMemory).UsedMemory := Value;
end;

function TCustomTurboModule.StoredInOwner: Boolean;
begin
  Result := Assigned(Owner) and
     ((Visibility <= fvPrivate)
       //or (Parent.ModuleType = mtFunction)
     );
end;

procedure TCustomTurboModule.Unload;
begin
  if FIsLoaded and not StoredInOwner then
  begin
    SendUnloadNotification;
    //if Name <> '' then RemoveModuleTypes(Name);
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

procedure TCustomTurboExecutor.DoPrintShortString(const aStr: ShortString);
begin
  DoPrintString(aStr);
end;

procedure TCustomTurboExecutor.DoPrintString(const aStr: String);
begin
  if Assigned(FOnPrintString) then
    FOnPrintString(Self, aStr);
end;

procedure TCustomTurboExecutor.DoReset(Sender: TObject);
begin
  with PTurboPreservedDataMemory(FMemory.FDataMemory)^ do
  begin
    SP := Integer(GlobalOptions.ParamStackBase) + GlobalOptions.ParamStackSize;
    RP := Integer(GlobalOptions.ReturnStackBase) + GlobalOptions.ReturnStackSize;
    Executor := Self;
  end;
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
  if Assigned(FMemory) then
  begin
    aCFA := FMemory.GetWordCFA(aWord);
    if aCFA >=0 then
      Result := ExecuteCFA(aCFA)
    else
      Result := -1;
  end
  else
    Result := -1;
end;

function TCustomTurboExecutor.ExecuteWordEntry(const aWord: PTurboMethodEntry):
        Integer;
begin
  InitExecution;

  if (aWord.Word.CallStyle = csForth) and (aWord.Word.Visibility <= fvPrivate) then
  begin
    Result := iExecuteCFA(aWord.Word.CFA);
  end
  //else begin //external word
    //  Result := iExecuteExternalWord(PTurboExteralWordCFA(aWord.CFA+Integer(FMemory))
      //  , aWord.Options.CallStyle);
  //end
  ;
  FinalizeExecution;
end;

procedure TCustomTurboExecutor.FinalizeExecution;
begin
  //Apply the SP to TProgram.SP.
end;

function TCustomTurboExecutor.GetStatus: TTurboProcessorStates;
begin
  if Assigned(FMemory) then
    Result := FMemory.Status
  else
    Result := [];
end;

function TCustomTurboExecutor.iExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := -1;
  //if (psRunning in Status) then
    //raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);
end;

function TCustomTurboExecutor.iExecuteExternalWord(const aWord:
        PTurboExteralMethodOptions; const aCallStyle: TTurboCallStyle): Integer;
begin
  {case aCallStyle of
    csForth: with aWord^ do begin
      if ProcTypeEntry = nil then
        TCustomTurboModule(ProcTypeEntry) := RequireModule(ModuleEntry.ModuleName);
      if Assigned(ProcTypeEntry) then
      with TCustomTurboExecutor(ProcTypeEntry) do
      begin
        RP := Self.RP;
        SP := Self.SP;
        ParameterStack := Self.ParameterStack;
        ReturnStack := Self.ReturnStack;
        ParameterStackSize := Self.ParameterStackSize;
        ReturnStackSize := Self.ReturnStackSize;
        Result := ExecuteCFA(Integer(ProcAddr));
      end;
    end //csForth
    else begin
    end;
  end;
  }
end;

procedure TCustomTurboExecutor.InitExecution;
begin
  if not Assigned(FMemory) then
    raise ETurboScriptError.CreateRes(@rsTurboScriptNoMemError);

  if not FMemory.FIsLoaded then
    raise ETurboScriptError.CreateRes(@rsTurboScriptNotLoadedError);

  if (psRunning in FMemory.Status) then
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

  FMemory.Status := FMemory.Status + [psRunning];
  //Include(PTurboPreservedDataMemory(FDataMemory).GlobalOptions.States, psRunning);
end;

procedure TCustomTurboExecutor.SetMemory(const Value: TCustomTurboModule);
begin
  if (Value <> FMemory) then
  begin
    if not Assigned(FMemory) or not (psRunning in Status) then
    begin
      if Assigned(FMemory) then
        FMemory.OnReset := nil;
      FMemory := Value;
      if Assigned(FMemory) then
        FMemory.OnReset := DoReset;
    end;
  end;
end;

procedure TCustomTurboExecutor.SetStatus(Value: TTurboProcessorStates);
var
  vChanged: Boolean;
begin
  if Assigned(FMemory) then
    FMemory.Status := Value;
end;

procedure TCustomTurboExecutor.Stop;
begin
  if Assigned(FMemory) then
    FMemory.Status := FMemory.Status - [psRunning];
end;

{
******************************** TTurboProgram *********************************
}
constructor TTurboProgram.Create(const aOwner: TCustomTurboModule = nil;
        aVisibility: TTurboVisibility = fvPublished);
begin
  inherited Create(aOwner, aVisibility);
  GlobalOptions := @FGlobalOptions;
  ParameterStackSize := cDefaultParamStackSize;
  ReturnStackSize := cDefaultReturnStackSize;
end;

destructor TTurboProgram.Destroy;
begin
  FreeMem(ReturnStack);
  ReturnStack := nil;
  FreeMem(ParameterStack);
  ParameterStack := nil;
  FreeAndNil(FExecutor);
  inherited Destroy;
end;

procedure TTurboProgram.Execute(aTimeOut : Integer = 0);
begin
end;

function TTurboProgram.GetExecutor: TCustomTurboExecutor;
begin
  if not Assigned(FExecutor) then
  begin
    FExecutor := ExecutorClass.Create;//(Self, fvPrivate);
    FExecutor.Memory := Self;
  end;
  Result := FExecutor;
end;

procedure TTurboProgram.SetParameterStackSize(const Value: Integer);
begin
  if not (psRunning in FGlobalOptions.States) and (ParameterStackSize <> Value) then
  begin
    ParameterStackSize := Value;
    ReallocMem(FGlobalOptions.ParamStackBase, (Value+1)*SizeOf(Pointer));
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboProgram.SetReturnStackSize(const Value: Integer);
begin
  if not (psRunning in FGlobalOptions.States) and (ReturnStackSize <> Value) then
  begin
    ReturnStackSize := Value;
    //p := ReturnStack;
    ReallocMem(FGlobalOptions.ReturnStackBase, (Value+1)*SizeOf(Pointer));
    //ReturnStack := p;
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;


type
  PPTurboEntry = ^ PTurboEntry;
  PTurboEntry  = ^ TTurboEntry;
  TTurboEntry  = packed record
    Prior: Pointer;
    MetaInfo: TTurboMetaInfo;
  end;

{//remove registered types of this module
procedure RemoveModuleTypes(const aModuleName: string);
var
  i,j: integer;
  s: string;
begin
  with GRegisteredTypes^ do
    for i := Count - 1 downto 0 do
    begin
      s := PMeType(List^[i]).Name;
      j := Pos('.', s) - 1;
      if (j = Length(aModuleName)) and CompareMem(@aModuleName[1], @s[1], j) then
      begin
        Delete(i);
      end;
    end;
end;
//}
procedure TurboConvertEntryRelatedToAbsolute(Mem: Pointer; var aEntry: PTurboEntry);
var
  vEntry: PTurboEntry; 
begin
  if Assigned(aEntry) then
  begin
    Integer(aEntry) := Integer(aEntry) + Integer(Mem);
    vEntry := aEntry;
    while Assigned(vEntry) do
    begin
      if vEntry.Prior <> nil then
        Integer(vEntry.Prior) := Integer(vEntry.Prior) + Integer(Mem);
      if Assigned(vEntry.MetaInfo.Name) then
        vEntry.MetaInfo.Name := Pointer(Integer(vEntry.MetaInfo.Name) + Integer(Mem));
      vEntry := vEntry.Prior;   
    end;
  end;
end;

procedure TurboConvertVarEntryRelatedToAbsolute(Mem: Pointer; var aEntry: PTurboVariableEntry);
var
  vEntry: PTurboVariableEntry;
begin
  if Assigned(aEntry) then
  begin
    Integer(aEntry) := Integer(aEntry) + Integer(Mem);
    vEntry := aEntry;
    while Assigned(vEntry) do
    begin
      if vEntry.Prior <> nil then
        Integer(vEntry.Prior) := Integer(vEntry.Prior) + Integer(Mem);
      if Assigned(vEntry.Variable.Name) then
        vEntry.Variable.Name := Pointer(Integer(vEntry.Variable.Name) + Integer(Mem));
      if Assigned(vEntry.Variable.TypeInfo) then
        vEntry.Variable.TypeInfo := Pointer(Integer(vEntry.Variable.TypeInfo) + Integer(Mem));  
      vEntry := vEntry.Prior;   
    end;
  end;
end;

procedure TurboConvertAddrRelatedToAbsolute(Mem: Pointer; Data:
        PTurboPreservedDataMemory);
begin
  {if Assigned(Mem.InitializeProc) then
    Inc(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Inc(Integer(Mem.FinalizeProc), Integer(Mem));
}
  //TODO: 
  TurboConvertEntryRelatedToAbsolute(Data, PTurboEntry(Data.LastTypeInfoEntry));  
  TurboConvertVarEntryRelatedToAbsolute(Data, Data.LastVariableEntry);  
  TurboConvertEntryRelatedToAbsolute(Data, PTurboEntry(Data.LastWordEntry));  
  TurboConvertEntryRelatedToAbsolute(Data, PTurboEntry(Data.LastModuleRefEntry));  
end;

procedure TurboConvertEntryAbsoluteToRelated(Mem: Pointer; var aEntry: PTurboEntry);
var
  vEntry: PTurboEntry;
  P: PTurboEntry;
begin
    if Assigned(aEntry) then
      Integer(aEntry) := Integer(aEntry) - Integer(Mem);
    vEntry := aEntry;
    while Assigned(vEntry) do
    begin
      p := vEntry.Prior;
      if vEntry.Prior <> nil then
      begin
        Integer(vEntry.Prior) := Integer(vEntry.Prior) - Integer(Mem);
      end;
      if Assigned(vEntry.MetaInfo.Name) then
        vEntry.MetaInfo.Name := Pointer(Integer(vEntry.MetaInfo.Name) - Integer(Mem));
      //aEntry.Prior := t;
      vEntry := P;   
    end;
end;

procedure TurboConvertVarEntryAbsoluteToRelated(Mem: Pointer; var aEntry: PTurboVariableEntry);
var
  vEntry: PTurboVariableEntry;
  P: PTurboVariableEntry;
begin
    if Assigned(aEntry) then
      Integer(aEntry) := Integer(aEntry) - Integer(Mem);
    vEntry := aEntry;
    while Assigned(vEntry) do
    begin
      p := vEntry.Prior;
      if vEntry.Prior <> nil then
        Integer(vEntry.Prior) := Integer(vEntry.Prior) - Integer(Mem);
      if Assigned(vEntry.Variable.Name) then
        vEntry.Variable.Name := Pointer(Integer(vEntry.Variable.Name) - Integer(Mem));
      if Assigned(vEntry.Variable.TypeInfo) then
        vEntry.Variable.TypeInfo := Pointer(Integer(vEntry.Variable.TypeInfo) - Integer(Mem));
      //aEntry.Prior := t;
      vEntry := P;   
    end;
end;

procedure TurboConvertAddrAbsoluteToRelated(Mem: Pointer; Data:
        PTurboPreservedDataMemory);
begin
{
  if Assigned(Mem.InitializeProc) then
    Dec(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Dec(Integer(Mem.FinalizeProc), Integer(Mem));
}
  TurboConvertEntryAbsoluteToRelated(Data, PTurboEntry(Data.LastWordEntry));  
  TurboConvertEntryAbsoluteToRelated(Data, PTurboEntry(Data.LastModuleRefEntry));  
  TurboConvertVarEntryAbsoluteToRelated(Data, Data.LastVariableEntry);  
  TurboConvertEntryAbsoluteToRelated(Data, PTurboEntry(Data.LastTypeInfoEntry));  
end;

{
procedure SetupSimpleTypes;
begin
  SimpleTurboTypes[ttkSByte] := GetRegisteredTypeByTypeInfo(Typeinfo(Shortint));
  SimpleTurboTypes[ttkUByte] := GetRegisteredTypeByTypeInfo(Typeinfo(Byte));
  SimpleTurboTypes[ttkSWord] := GetRegisteredTypeByTypeInfo(Typeinfo(Smallint));
  SimpleTurboTypes[ttkUWord] := GetRegisteredTypeByTypeInfo(Typeinfo(Word));
  SimpleTurboTypes[ttkSLong] := GetRegisteredTypeByTypeInfo(Typeinfo(Longint));
  SimpleTurboTypes[ttkULong] := GetRegisteredTypeByTypeInfo(Typeinfo(LongWord));
  SimpleTurboTypes[ttkInt64] := GetRegisteredTypeByTypeInfo(Typeinfo(Int64));
  SimpleTurboTypes[ttkSingle] := GetRegisteredTypeByTypeInfo(Typeinfo(Single));
  SimpleTurboTypes[ttkDouble] := GetRegisteredTypeByTypeInfo(Typeinfo(Double));
  SimpleTurboTypes[ttkExtended] := GetRegisteredTypeByTypeInfo(Typeinfo(Extended));
  SimpleTurboTypes[ttkComp] := GetRegisteredTypeByTypeInfo(Typeinfo(Comp));
  SimpleTurboTypes[ttkCurr] := GetRegisteredTypeByTypeInfo(Typeinfo(Currency));
  SimpleTurboTypes[ttkString] := GetRegisteredTypeByTypeInfo(Typeinfo(Shortstring));
  SimpleTurboTypes[ttkLString] := GetRegisteredTypeByTypeInfo(Typeinfo(AnsiString));
  SimpleTurboTypes[ttkWString] := GetRegisteredTypeByTypeInfo(Typeinfo(WideString));
  SimpleTurboTypes[ttkChar] := GetRegisteredTypeByTypeInfo(Typeinfo(Char));
  SimpleTurboTypes[ttkWChar] := GetRegisteredTypeByTypeInfo(Typeinfo(WideChar));
end;
}
initialization
finalization
end.
