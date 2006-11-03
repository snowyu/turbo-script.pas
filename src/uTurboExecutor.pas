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
  , TypInfo
  , uMeTypes
  , uMeProcType
  , uTurboConsts
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
  TTurboSimpleTypes = array [TTurboSimpleTypeKind] of PMeType;

  PPreservedCodeMemory = ^ TPreservedCodeMemory;
  PTurboVariableEntry = ^ TTurboVariableEntry;
  PTurboModuleEntry = ^ TTurboModuleEntry;
  PTurboWordEntry = ^ TTurboWordEntry;
  PTurboTypeInfoEntry = ^ TTurboTypeInfoEntry;
  PTurboExteralWordCFA = ^TTurboExteralWordCFA;

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
    function GetGlobalOptions: Pointer;
    procedure SetGlobalOptions(Value: Pointer);
  protected
    FAccessor: TObject;
    FIsLoaded: Boolean;
    {: The Code Memory }
    FMemory: Pointer;
    FModuleDate: TTimeStamp;
    FModuleUnloadNotifies: TList;
    FModuleVersion: LongWord;
    FName: string;
    FOptions: TTurboScriptOptions;
    FParent: TCustomTurboModule;
    FPC: Integer;
    FRP: Integer;
    {: the Parameter Stack(or data stack) Pointer }
    FSP: Integer;
    {: The Current TIB Index }
    { Description
    FTIBIndex : Text[FTIBIndex]
    }
    FTextIndex: Integer;
    FVisibility: TTurboVisibility;
    function GetInitializeProc: Integer;
    function GetLastErrorCode: TTurboProcessorErrorCode;
    function GetLastTypeInfoEntry: PTurboTypeInfoEntry;
    function GetLastVariableEntry: PTurboVariableEntry;
    function GetLastWordEntry: PTurboWordEntry;
    function GetMemorySize: Integer;
    function GetModuleType: TTurboModuleType;
    function GetParameterStack: Pointer;
    function GetParameterStackSize: Integer;
    function GetReturnStack: Pointer;
    function GetReturnStackSize: Integer;
    function GetRoot: TCustomTurboModule;
    function GetStatus: TTurboProcessorStates;
    function GetUsedMemory: Integer;
    procedure Grow(const aSize: Integer = 0);
    procedure SendUnloadNotification;
    procedure SetLastTypeInfoEntry(const Value: PTurboTypeInfoEntry);
    procedure SetLastVariableEntry(const Value: PTurboVariableEntry);
    procedure SetLastWordEntry(const Value: PTurboWordEntry);
    procedure SetMemorySize(Value: Integer);
    procedure SetModuleType(Value: TTurboModuleType);
    procedure SetParameterStack(Value: Pointer);
    procedure SetParameterStackSize(Value: Integer);
    procedure SetParent(const Value: TCustomTurboModule);
    procedure SetReturnStack(Value: Pointer);
    procedure SetReturnStackSize(Value: Integer);
    procedure SetStatus(Value: TTurboProcessorStates);
    procedure SetUsedMemory(Value: Integer);
  public
    constructor Create(const aParent: TCustomTurboModule = nil; aVisibility:
            TTurboVisibility = fvPublished); virtual;
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
    {: add a OpCode to the free memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddOpToMem(const aOpCode: TTurboVMInstruction);
    {: fill 0 to align the memory. }
    procedure AlignMem;
    procedure AllocSpace(const aSize: Integer);
    {: reduce the memory size to initialization state }
    procedure ClearMemory;
    {: nil means not found. }
    function FindTypeInfoEntry(const aName: string): PTurboTypeInfoEntry;
    function FindUnloadNotification(aProc: TNotifyEvent): Integer;
    {: nil means not found. }
    function FindVariableEntry(const aName: string): PTurboVariableEntry;
    {: nil means not found. }
    function FindWordEntry(const aName: string): PTurboWordEntry;
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
    property GlobalOptions: Pointer read GetGlobalOptions write
            SetGlobalOptions;
    property InitializeProc: Integer read GetInitializeProc;
    { Description
    if not loaded, then only the name and some options loaded but the body(
    Memory).
    }
    property IsLoaded: Boolean read FIsLoaded write FIsLoaded;
    property LastErrorCode: TTurboProcessorErrorCode read GetLastErrorCode;
    property LastTypeInfoEntry: PTurboTypeInfoEntry read GetLastTypeInfoEntry
            write SetLastTypeInfoEntry;
    property LastVariableEntry: PTurboVariableEntry read GetLastVariableEntry
            write SetLastVariableEntry;
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
    property ParameterStack: Pointer read GetParameterStack write
            SetParameterStack;
    {: : the Parameter Stack }
    { Description
    指向栈底： @Stack[0]
    压入减小
    }
    property ParameterStackSize: Integer read GetParameterStackSize write
            SetParameterStackSize;
    {: 它的父亲:被 Parser 或Tree使用. nil means root. }
    { Description
    如果该模块是私有的,那么内存就是使用的父亲的内存.
    }
    property Parent: TCustomTurboModule read FParent write SetParent;
    {: : program counter. }
    { Description
    program counter, which contains the address 
    in memory of the instruction that is the next 
    to be executed. 
    }
    property PC: Integer read FPC write FPC;
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
  private
    FOnPrintString: TTurboPrintStringEvent;
  protected
    FOnPrintChar: TTurboPrintCharEvent;
    procedure DoPrintChar(aChar: Char); virtual;
    procedure DoPrintShortString(const aStr: ShortString);
    procedure DoPrintString(const aStr: String); virtual;
    {: Finalize after the execution. }
    procedure FinalizeExecution; virtual;
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
    function iExecuteExternalWord(const aWord: PTurboExteralWordCFA; const
            aCallStyle: TTurboCallStyle): Integer;
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
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function ExecuteWordEntry(const aWord: PTurboWordEntry): Integer;
    procedure Stop;
    property OnPrintChar: TTurboPrintCharEvent read FOnPrintChar write
            FOnPrintChar;
    property OnPrintString: TTurboPrintStringEvent read FOnPrintString write
            FOnPrintString;
  end;

  TTurboProgram = class(TCustomTurboExecutor)
  private
    FExecutorClass: TTurboExecutorClass;
    function GetExecutor: TCustomTurboExecutor;
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetReturnStackSize(const Value: Integer);
  protected
    FExecutor: TCustomTurboExecutor;
    procedure DoPrintChar(aChar: Char); override;
    procedure DoPrintString(const aStr: String); override;
    {: Finalize after the execution. }
    procedure FinalizeExecution; override;
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
    }
    function iExecuteCFA(const aCFA: Integer): Integer; override;
    {: Init before the Execution. }
    procedure InitExecution; override;
    property Executor: TCustomTurboExecutor read GetExecutor;
  public
    { Description
    为了在多个 executor 中管理共享数据栈＆返回栈以及运行参数。
    }
    constructor Create(const aParent: TCustomTurboModule = nil; aVisibility:
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



  //For type-cast the Mem
  TTurboWordEntry = packed record
    Prior: PTurboWordEntry; //前一个单词 0 means 为最前面。

    Options: TTurboWordOptions;
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord;
    CFA: tsInt;//the offset address of the memory.
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
  //cfsHostFunction, cfsDLLFunction
  TTurboExteralWordCFA = packed record
    ProcAddr: Pointer; //if it csForth then it is the CFA.
    ModuleEntry: PTurboModuleEntry;
    ProcTypeEntry: Pointer; //if it csForth then it is Module instance else it is ProcTypeInfo offset addr.
    Index: tsInt; //-1 means non-index visits.
    Name: ShortString; //packed the function name in the DLL/Host.
  end;

  TTurboVariableEntry = packed record
    Prior: PTurboVariableEntry; 
    Size: tsInt;
    Addr: Pointer; //offset address of the FMemory.
    TypeInfo: PTurboTypeInfoEntry; //TODO: relocate it.
    Name: ShortString;//packed
    //Value: ....
  end;
  
  //the import module for uses.
  TTurboModuleEntry = packed record
    Prior: PTurboModuleEntry; //nil means no more
    ModuleType: TTurboModuleType;
    {
      DLL Module(mtDLL): it's the DLL handle.
      Host Module(mtHost):
      ForthLib module(mtLib): loaded the instance of TCustomTurboModule.
      nil means not assigned(or loaded).
    }
    Module: Pointer; 
    ModuleName: ShortString; //packed string, the full module name with path.
  end;

  TTurboTypeInfoEntry = packed record
    Prior: PTurboTypeInfoEntry; //nil means no more
    //## abondoned following fields are TypeInfo: PMeType
    //## MeType: Pointer; //PMeType(@TTurboSymbolEntry.MeType) 
    Kind: TMeTypeKind;
    Name: ShortString; //packed ; maybe nil.
   {TypeData: TTypeData} 
   {Note: all PPTypeInfo are integer and the offset address of the PTurboTypeInfoEntry}
  end;
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


  PTurboGlobalOptions = ^TTurboGlobalOptions;
  TTurboGlobalOptions = record  //do not use the packed.
    //States: TTurboProcessorStates; //如果放在这里，速度会下降
    LastErrorCode: TTurboProcessorErrorCode;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ParamStackBottom: Pointer;
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    ReturnStackBottom: Pointer;
  end;

  //the typecast for code memory area to get the parameters
  TPreservedCodeMemory = packed record
    States: TTurboProcessorStates;
    GlobalOptions: PTurboGlobalOptions;
    Executor: TCustomTurboModule;
    //##abondoned:this Module unique Index in this program, allocated by compiler.
    //##ModuleIndex: Integer;
    ModuleType: TTurboModuleType;
    {ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ParamStackBottom: Pointer;
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    ReturnStackBottom: Pointer;
    }
    UsedMemory: tsInt;
    MemorySize: tsInt; 
    //ToIn: Integer; //>IN the text buffer current index
    //TIBLength: Integer; //#TIB the text buffer length
    //TIB: array [0..cMAXTIBCount-1] of char; //'TIB
    //LastErrorCode: TTurboProcessorErrorCode;
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
    //reserved: array [SizeOf() ] of byte; 
  end;
  
  TTurboModuleStreamHeader = packed record
    Id: array [0..cFORTHHeaderMagicIdLen-1] of char;
    Version: LongWord;
    BuildDate: TTimeStamp;
  end;
  
procedure TurboConvertAddrRelatedToAbsolute(Mem: PPreservedCodeMemory);
procedure TurboConvertAddrAbsoluteToRelated(Mem: PPreservedCodeMemory);
//remove registered types of this module
procedure RemoveModuleTypes(const aModuleName: string);

var 
  SimpleTurboTypes: TTurboSimpleTypes;


implementation

uses
  uTurboAccessor;

{
****************************** TCustomTurboModule ******************************
}
constructor TCustomTurboModule.Create(const aParent: TCustomTurboModule = nil;
        aVisibility: TTurboVisibility = fvPublished);
begin
  inherited Create;
  FModuleUnloadNotifies := TList.Create;
  FParent := aParent;
  FVisibility := aVisibility;
  ClearMemory;
end;

destructor TCustomTurboModule.Destroy;
begin
  Unload;

  if not StoredInParent then
  begin
    FreeMem(FMemory);
    FMemory := nil;
  end;
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

procedure TCustomTurboModule.AddOpToMem(const aOpCode: TTurboVMInstruction);
var
  p: Pointer;
begin
  if PPreservedCodeMemory(FMemory).UsedMemory >= MemorySize then
    Grow;
  Integer(p) := Integer(FMemory) + PPreservedCodeMemory(FMemory).UsedMemory;
  PInteger(P)^ := Integer(aOpCode);
  Inc(PPreservedCodeMemory(FMemory).UsedMemory, SizeOf(Integer));
end;

procedure TCustomTurboModule.AlignMem;
var
  I: Integer;
begin
  I := UsedMemory mod SizeOf(Integer);
  if I <> 0 then AllocSpace(I);
end;

procedure TCustomTurboModule.AllocSpace(const aSize: Integer);
begin
  if (PPreservedCodeMemory(FMemory).UsedMemory+ aSize) >= MemorySize then
    Grow(aSize);

  Inc(PPreservedCodeMemory(FMemory).UsedMemory, aSize);
end;

procedure TCustomTurboModule.ClearMemory;
var
  vPreserved: Integer;
begin
  if not StoredInParent then
  begin
    vPreserved := SizeOf(TPreservedCodeMemory);
    if vPreserved < Integer(High(TTurboVMInstruction)) then
        vPreserved := Integer(High(TTurboVMInstruction));
    ReallocMem(FMemory, vPreserved);
    with PPreservedCodeMemory(FMemory)^ do
    begin
      MemorySize := vPreserved;
      UsedMemory := vPreserved;
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
end;

function TCustomTurboModule.FindTypeInfoEntry(const aName: string):
        PTurboTypeInfoEntry;
begin
  Result := LastTypeInfoEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
      Integer(Result) := Integer(FMemory) + Integer(Result);
    if Result.Name = aName then
    begin
      Exit;
    end
    else
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
begin
  Result := LastVariableEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
      Integer(Result) := Integer(FMemory) + Integer(Result);
    if Result.Name = aName then
    begin
      Exit;
    end
    else
      Result := Result.Prior;
  end;
  Result := nil;
end;

function TCustomTurboModule.FindWordEntry(const aName: string): PTurboWordEntry;
begin
  Result := LastWordEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
    begin
      //writeln('psCompiling,', Integer(Result));
      Integer(Result) := Integer(FMemory) + Integer(Result);
    end;
    //writeln('FindWordEntry:', Result.Name);
    if Result.Name = aName then
    begin
      Exit;
    end
    else
      Result := Result.Prior;
  end;
  Result := nil;
end;

function TCustomTurboModule.GetGlobalOptions: Pointer;
begin
  Result := PPreservedCodeMemory(FMemory).GlobalOptions;
end;

function TCustomTurboModule.GetInitializeProc: Integer;
begin
  Result := Integer(PPreservedCodeMemory(FMemory)^.InitializeProc);
end;

function TCustomTurboModule.GetLastErrorCode: TTurboProcessorErrorCode;
begin
  with PPreservedCodeMemory(FMemory)^ do
  begin
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.LastErrorCode
    else
      Result := errNone;
  end;
end;

function TCustomTurboModule.GetLastTypeInfoEntry: PTurboTypeInfoEntry;
begin
  Result := PPreservedCodeMemory(FMemory).LastTypeInfoEntry;
end;

function TCustomTurboModule.GetLastVariableEntry: PTurboVariableEntry;
begin
  Result := PPreservedCodeMemory(FMemory).LastVariableEntry;
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

function TCustomTurboModule.GetParameterStack: Pointer;
begin
  with PPreservedCodeMemory(FMemory)^ do
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.ParamStackBase
    else
      Result := nil;
end;

function TCustomTurboModule.GetParameterStackSize: Integer;
begin
  with PPreservedCodeMemory(FMemory)^ do
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.ParamStackSize
    else
      Result := 0;
end;

function TCustomTurboModule.GetReturnStack: Pointer;
begin
  with PPreservedCodeMemory(FMemory)^ do
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.ReturnStackBase
    else
      Result := nil;
end;

function TCustomTurboModule.GetReturnStackSize: Integer;
begin
  with PPreservedCodeMemory(FMemory)^ do
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.ReturnStackSize
    else
      Result := 0;
end;

function TCustomTurboModule.GetRoot: TCustomTurboModule;
begin
  Result := Parent;
  while Assigned(Result) and Assigned(Result.Parent) do
  begin
    Result := Result.Parent;
  end;

  if Result = nil then Result := Self;
end;

function TCustomTurboModule.GetStatus: TTurboProcessorStates;
begin
  Result := PPreservedCodeMemory(FMemory).States;
end;

function TCustomTurboModule.GetUsedMemory: Integer;
begin
  Result := PPreservedCodeMemory(FMemory).UsedMemory;
end;

function TCustomTurboModule.GetWordCFA(const aWord: string): Integer;
begin
  Result := Integer(FindWordEntry(aWord));
  if Result <> 0 then
    Result := PTurboWordEntry(Result).CFA;
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

  with PPreservedCodeMemory(FMemory)^ do
  begin
    //backup the data in the Memory
    vOptions := GlobalOptions;
    {vParameterStack := ParameterStack;
    vReturnStack := ReturnStack;
    vReturnStackSize := ReturnStackSize;
    vParameterStackSize := ParameterStackSize;}
  end;

  MemorySize := Count-SizeOf(TTurboModuleStreamHeader);
  aStream.ReadBuffer(FMemory^, MemorySize);

  with PPreservedCodeMemory(FMemory)^ do
  begin
    //now restore the data
    GlobalOptions := vOptions;
    {ParameterStack := vParameterStack;
    ReturnStack := vReturnStack;
    ReturnStackSize := vReturnStackSize;
    ParameterStackSize := vParameterStackSize; //}
  end;

  Reset;
  if not (psCompiling in Status) then TurboConvertAddrRelatedToAbsolute(FMemory);

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
  if (Sender = FParent) and StoredInParent then
  begin
    FIsLoaded := False;
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
  //PPreservedCodeMemory(FMemory).ParamStackBase := FParameterStack;
  //PPreservedCodeMemory(FMemory).ParamStackSize := FParameterStackSize;

  with PPreservedCodeMemory(FMemory)^ do
  begin
    SP := Integer(GlobalOptions.ParamStackBase) + GlobalOptions.ParamStackSize;
    RP := Integer(GlobalOptions.ReturnStackBase) + GlobalOptions.ReturnStackSize;
    GlobalOptions.LastErrorCode := errNone;
    Executor := Self;
    States := [];
  end;
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
  vHeader.Version := ModuleVersion;
  vHeader.BuildDate := ModuleDate;
  aStream.WriteBuffer(vHeader, SizeOf(TTurboModuleStreamHeader));
  if not (psCompiling in Status) then TurboConvertAddrAbsoluteToRelated(FMemory);

  with PPreservedCodeMemory(FMemory)^ do
  begin
    //backup the data in the Memory
    vOptions := GlobalOptions;
    GlobalOptions := nil;
    {vParameterStack := ParameterStack;
    vReturnStack := ReturnStack;
    vReturnStackSize := ReturnStackSize;
    vParameterStackSize := ParameterStackSize;
    ParameterStack := 0;
    ReturnStack := 0;
    ReturnStackSize := 0;
    ParameterStackSize := 0;}
  end;

  aStream.WriteBuffer(FMemory^, UsedMemory);

  with PPreservedCodeMemory(FMemory)^ do
  begin
    GlobalOptions := vOptions;
  {  ParameterStack := vParameterStack;
    ReturnStack := vReturnStack;
    ReturnStackSize := vReturnStackSize;
    ParameterStackSize := vParameterStackSize;
  }
  end;

  if not (psCompiling in Status) then TurboConvertAddrRelatedToAbsolute(FMemory);
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

procedure TCustomTurboModule.SetGlobalOptions(Value: Pointer);
begin
  PPreservedCodeMemory(FMemory).GlobalOptions := Value;
end;

procedure TCustomTurboModule.SetLastTypeInfoEntry(const Value:
        PTurboTypeInfoEntry);
begin
  PPreservedCodeMemory(FMemory).LastTypeInfoEntry := Value;
end;

procedure TCustomTurboModule.SetLastVariableEntry(const Value:
        PTurboVariableEntry);
begin
  PPreservedCodeMemory(FMemory).LastVariableEntry := Value;
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
      //TODO: the base-address is changed. relocate the addresses.
  end;
end;

procedure TCustomTurboModule.SetModuleType(Value: TTurboModuleType);
begin
  PPreservedCodeMemory(FMemory).ModuleType := Value;
end;

procedure TCustomTurboModule.SetParameterStack(Value: Pointer);
begin
  PPreservedCodeMemory(FMemory).GlobalOptions.ParamStackBase := Value;
end;

procedure TCustomTurboModule.SetParameterStackSize(Value: Integer);
begin
  PPreservedCodeMemory(FMemory).GlobalOptions.ParamStackSize := Value;
end;

procedure TCustomTurboModule.SetParent(const Value: TCustomTurboModule);
begin
  if Value <> FParent then
  begin
    if Assigned(FParent) then
      FParent.RemoveUnloadNotification(NotifyModuleUnloaded);
    FParent := Value;
    if Assigned(FParent) then
      FParent.UnloadNotification(NotifyModuleUnloaded);
  end;
end;

procedure TCustomTurboModule.SetReturnStack(Value: Pointer);
begin
  PPreservedCodeMemory(FMemory).GlobalOptions.ReturnStackBase := Value;
end;

procedure TCustomTurboModule.SetReturnStackSize(Value: Integer);
begin
  PPreservedCodeMemory(FMemory).GlobalOptions.ReturnStackSize := Value;
end;

procedure TCustomTurboModule.SetStatus(Value: TTurboProcessorStates);
var
  vChanged: Boolean;
begin
  if Value <> PPreservedCodeMemory(FMemory).States then
  begin
    vChanged := psCompiling in (Value * PPreservedCodeMemory(FMemory).States);
    PPreservedCodeMemory(FMemory).States := Value;
    if vChanged then
    begin
      if psCompiling in Value then
        TurboConvertAddrAbsoluteToRelated(FMemory)
      else
        TurboConvertAddrRelatedToAbsolute(FMemory);
    end;

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
     ((Visibility <= fvPrivate) or (Parent.ModuleType = mtFunction));
end;

procedure TCustomTurboModule.Unload;
begin
  if FIsLoaded and not StoredInParent then
  begin
    SendUnloadNotification;
    if Name <> '' then RemoveModuleTypes(Name);
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

function TCustomTurboExecutor.ExecuteWordEntry(const aWord: PTurboWordEntry):
        Integer;
begin
  InitExecution;

  if (aWord.Options.CallStyle = csForth) and (aWord.Options.Visibility <= fvPrivate) then
        Result := iExecuteCFA(aWord.CFA)
  else begin //external word
      Result := iExecuteExternalWord(PTurboExteralWordCFA(aWord.CFA+Integer(FMemory))
        , aWord.Options.CallStyle);
  end;
  FinalizeExecution;
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

function TCustomTurboExecutor.iExecuteExternalWord(const aWord:
        PTurboExteralWordCFA; const aCallStyle: TTurboCallStyle): Integer;
begin
  case aCallStyle of
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
constructor TTurboProgram.Create(const aParent: TCustomTurboModule = nil;
        aVisibility: TTurboVisibility = fvPublished);
begin
  inherited Create(aParent, aVisibility);
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

procedure TTurboProgram.DoPrintChar(aChar: Char);
begin
  FExecutor.DoPrintChar(aChar);
end;

procedure TTurboProgram.DoPrintString(const aStr: String);
begin
  FExecutor.DoPrintString(aStr);
end;

procedure TTurboProgram.Execute(aTimeOut : Integer = 0);
begin
end;

procedure TTurboProgram.FinalizeExecution;
begin
  Executor.FinalizeExecution;
end;

function TTurboProgram.GetExecutor: TCustomTurboExecutor;
begin
  if not Assigned(FExecutor) then
  begin
    FExecutor := ExecutorClass.Create(Self, fvPrivate);
  end;
  Result := FExecutor;
end;

function TTurboProgram.iExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := Executor.iExecuteCFA(aCFA);
end;

procedure TTurboProgram.InitExecution;
begin
  Executor.InitExecution;
end;

procedure TTurboProgram.SetParameterStackSize(const Value: Integer);
begin
  if not (psRunning in Status) and (ParameterStackSize <> Value) then
  begin
    ParameterStackSize := Value;
    ReallocMem(PPreservedCodeMemory(FMemory).GlobalOptions.ParamStackBase, (Value+1)*SizeOf(Pointer));
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboProgram.SetReturnStackSize(const Value: Integer);
begin
  if not (psRunning in Status) and (ReturnStackSize <> Value) then
  begin
    ReturnStackSize := Value;
    //p := ReturnStack;
    ReallocMem(PPreservedCodeMemory(FMemory).GlobalOptions.ReturnStackBase, (Value+1)*SizeOf(Pointer));
    //ReturnStack := p;
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;


type
  PTurboEntry = ^ TTurboEntry;
  TTurboEntry = packed record
    Prior: Pointer;
  end;

//remove registered types of this module
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

initialization
finalization
end.
