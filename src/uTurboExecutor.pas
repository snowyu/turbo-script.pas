{: The abstract super script executor module. }
{ Description

��ס����Ķ�ջ(����X86�Ķ�ջ����)�� ѹ�����ǵ�ַ���٣��������ǵ�ַ����,
��ջ�����ڶ��������������ġ�
When an item is pushed onto the stack, the processor decrements the ESP
register, then writes the item at the new top of stack. When an item is popped
off the stack, the
processor reads the item from the top of stack, then increments the ESP register


Ҳ�������� TTurboAppDomain
���У����������������ջ������ջ���ڴ棬����GlobalOptions��
  ParameterStackSize := 4096;
  GetMem(ParameterStack, ParameterStackSize);
  ReturnStackSize := 4096;
  GetMem(ReturnStack, ReturnStackSize);
}
unit uTurboExecutor;

interface

{$I TurboScript.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeTypes
  //, uMeProcType
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboTypes
  ;

type
  {##: the basic simple turbo types. }
  {## Now use TMeTypeKind
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
{##  TTurboSimpleTypeKind = (
    ttkUnknown, ttkSByte, ttkUByte, ttkSWord, ttkUWord
    , ttkSLong, ttkULong, ttkQWord, ttkInt64
    , ttkSingle, ttkDouble, ttkExtended, ttkComp, ttkCurr
    , ttkVariant, ttkRecord, ttkArray, ttkDynArray, ttkClass, ttkObject 
    , ttkString, ttkLString, ttkMeString, ttkWString, ttkChar, ttkWChar
    , ttkMethod, ttkProcedure, ttkInterface, ttkPointer, ttkParam
    , ttkSet, ttkEnumeration
  );
  //TTurboSimpleTypes = array [TTurboSimpleTypeKind] of PMeType;}


  //PTurboPreservedCodeMemory = ^ TTurboPreservedCodeMemory;
  PTurboPreservedDataMemory = ^ TTurboPreservedDataMemory;
  PTurboGlobalOptions = ^TTurboGlobalOptions;
  PTurboRelocatedAddresses = ^TTurboRelocatedAddresses;
  PTurboRelocatedTypes = ^TTurboRelocatedTypes;

  TCustomTurboPEFormat = class;
  TCustomTurboModule = class;
  TCustomTurboExecutor = class;
  TTurboAppDomain = class;
  TTurboGlobalOptions = object  //do not use the packed.
  private
    function GetIsRunning(): Boolean;
    procedure SetIsRunning(const Value: Boolean);
  public
    States: Byte; //TTurboProcessorStates; modified by FPC
    LastErrorCode: TTurboProcessorErrorCode;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ParamStackBottom: tsInt;
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    //if halt with errHalt then the ReturnStackBottom will be the Old RSP.   
    ReturnStackBottom: tsInt;
    _PC: tsInt; //program counter address pointer: ��PCֻ�е����н���ǰ��TODO,��λ��ʱ�򣩺��˳���Żᱻ���á�ֻ����ʱ���档
    _SP: tsInt; //the Parameter Stack(or data stack) Pointer, ͬ�ϣ���ʱ����
    _RP: tsInt; //return stack pointer(TOS). ͬ�ϣ���ʱ����
    _Mem: PTurboPreservedDataMemory; //current engine used memory. temp(only some executor class used.)
    Executor: TCustomTurboExecutor;
    //{:  �ŵ�ϵͳ��Ԫ���У� �ô�����ֲ�Ծ٣�ȱ�����ٶȱ����ɡ�
    ErrorAddr: Pointer;
    ErrorProc: Pointer; 
    AssertErrorProc: Pointer;
    //The built-in I/O routines use InOutResult to store the value that 
    //the next call to the IOResult standard function will return.
    IOResult: tsInt;
    {: the States in [psRunning, psStepping]}
    property IsRunning: Boolean read GetIsRunning write SetIsRunning;   
  end;

  TTurboModuleClass = class of TCustomTurboModule;
  TTurboExecutorClass = class of TCustomTurboExecutor;
  TTurboPrintCharEvent = procedure(Sender: TCustomTurboExecutor; aChar: Char) of object;
  TTurboPrintStringEvent = procedure(Sender: TCustomTurboExecutor; const aStr: String) of object;
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

  {: the abstract turbo script Module. }
  { Description
  û��ִ�л���/

  Load the script into memory

  the TCustomTurboModule is the compiled script in the memory.
  }
  TCustomTurboModule = class(TCustomTurboObject)
  private
    FOnReset: TNotifyEvent;
    FRegisteredTypes: PTurboRegisteredTypes;
    FRelocatedDataAddresses: PTurboRelocatedAddresses;
    FRelocatedDataTypes: PTurboRelocatedTypes;
    function GetIsAddrResolved: Boolean;
    function GetLastModuleRefEntry: PTurboModuleRefEntry;
    function GetOptions: TTurboScriptOptions;
    procedure SetIsAddrResolved(const Value: Boolean);
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
    procedure FreeDLLLibs;
    function GetDataMemorySize: tsInt;
    function GetInitializeProc: Integer;
    function GetLastTypeInfoEntry: PTurboTypeInfoEntry;
    function GetLastTypeRefEntry: PTurboTypeRefEntry;
    function GetLastVariableEntry: PTurboStaticFieldEntry;
    function GetLastWordEntry: PTurboMethodEntry;
    function GetMemorySize: tsInt;
    function GetModuleType: TTurboModuleType;
    function GetRoot: TCustomTurboModule;
    function GetUsedDataSize: tsInt;
    function GetUsedMemory: tsInt;
    procedure Grow(const aSize: Integer = 0);
    procedure GrowData(const aSize: Integer = 0);
    {: reset the stack pointers. }
    procedure iReset; virtual;
    procedure LoadUsedModules;
    {: clear all TurboType to nil for the aModuleRef }
    procedure ResetTypeRefsBy(const aModuleRef: PTurboModuleRefInfo);
    {: convert the module memory related address to absolute. }
    procedure ResolveAddress;
    procedure SendUnloadNotification;
    procedure SetDataMemorySize(Value: tsInt);
    procedure SetLastTypeInfoEntry(const Value: PTurboTypeInfoEntry);
    procedure SetLastTypeRefEntry(const Value: PTurboTypeRefEntry);
    procedure SetLastVariableEntry(const Value: PTurboStaticFieldEntry);
    procedure SetLastWordEntry(const Value: PTurboMethodEntry);
    procedure SetMemorySize(Value: tsInt);
    procedure SetModuleType(Value: TTurboModuleType);
    procedure SetOwner(const Value: TCustomTurboModule);
    procedure SetUsedDataSize(Value: tsInt);
    procedure SetUsedMemory(Value: tsInt);
    {: convert the module memory related address to absolute. }
    procedure UnResolveAddress;
  public
    constructor Create(const aOwner: TCustomTurboModule = nil; aVisibility:
            TTurboVisibility = tvPublicVisibilty); virtual;
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
    {: add a PChar to the free data-memory, and add UsedMemory }
    { Description
    Note: you must InitExecution first.
    }
    procedure AddPCharToData(const aValue: string);
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
    function FindModuleRefEntry(const aName: string): PTurboModuleRefEntry;
    {: find used module name entry. }
    { Description
    nil means not found.
    }
    function FindModuleRefEntryBy(const aModule: Pointer): PTurboModuleRefEntry;
    {: nil means not found. }
    function FindTypeInfoEntry(const aName: string): PTurboTypeInfoEntry;
    {: nil means not found. }
    function FindTypeRefEntry(const aName: string): PTurboTypeRefEntry;
    {: nil means not found. }
    function FindVariableEntry(const aName: string): PTurboStaticFieldEntry;
    {: nil means not found. }
    function FindWordEntry(const aName: string; aCodeFieldStyle:
            TTurboCodeFieldStyle = cfsFunction; const aCallStyle:
            TCallingConvention = ccUnknown): PTurboMethodEntry;
    {: Get the Local forth word entry. }
    { Description
    Note: 0 means not found.
    }
    function GetWordCFA(const aWord: string): Integer;
    function IndexOfUnloadNotification(aProc: TNotifyEvent): Integer;
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
    {: �Ƿ񱻴���� Owner ��. }
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
    property InitializeProc: Integer read GetInitializeProc;
    property IsAddrResolved: Boolean read GetIsAddrResolved write
            SetIsAddrResolved;
    { Description
    if not loaded, then only the name and some options loaded but the body(
    Memory).
    }
    property IsLoaded: Boolean read FIsLoaded write FIsLoaded;
    property LastModuleRefEntry: PTurboModuleRefEntry read
            GetLastModuleRefEntry write SetLastModuleRefEntry;
    property LastTypeInfoEntry: PTurboTypeInfoEntry read GetLastTypeInfoEntry
            write SetLastTypeInfoEntry;
    property LastTypeRefEntry: PTurboTypeRefEntry read GetLastTypeRefEntry
            write SetLastTypeRefEntry;
    property LastVariableEntry: PTurboStaticFieldEntry read
            GetLastVariableEntry write SetLastVariableEntry;
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
    {: ���ĸ���:�� Parser ��Treeʹ��. nil means root. }
    { Description
    �����ģ����˽�е�,��ô�ڴ����ʹ�õĸ��׵��ڴ�.
    }
    property Owner: TCustomTurboModule read FOwner write SetOwner;
    {: here are all published defined types! }
    { Description
    �������ڣ����������������û���������Ͷ������
    }
    property RegisteredTypes: PTurboRegisteredTypes read FRegisteredTypes;
    property RelocatedDataAddresses: PTurboRelocatedAddresses read
            FRelocatedDataAddresses;
    property RelocatedDataTypes: PTurboRelocatedTypes read FRelocatedDataTypes;
    property Root: TCustomTurboModule read GetRoot;
    {: �Ѿ�ʹ�õ��ڴ� }
    { Description
    Ҳ����ָ�����Ŀ����ڴ棺
    �Ӹõ�ַ����ڴ�δ�ã�FMemory[UsedMemory] 
    }
    property UsedDataSize: tsInt read GetUsedDataSize write SetUsedDataSize;
    {: �Ѿ�ʹ�õ��ڴ� }
    { Description
    Ҳ����ָ�����Ŀ����ڴ棺
    �Ӹõ�ַ����ڴ�δ�ã�FMemory[UsedMemory] 
    }
    property UsedMemory: tsInt read GetUsedMemory write SetUsedMemory;
    property Visibility: TTurboVisibility read FVisibility write FVisibility;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
  end;

  {: the abstract turbo script executor. }
  { Description
  û��ִ�л���/

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
  TCustomTurboExecutor = class(TCustomTurboObject)
  private
    FOnPrintString: TTurboPrintStringEvent;
    function GetOptions: TTurboScriptOptions;
    function GetPC: Pointer;
    function GetRP: Pointer;
    function GetSP: Pointer;
    procedure SetGlobalOptions(Value: PTurboGlobalOptions);
    procedure SetMemory(const Value: TCustomTurboModule);
    procedure SetOptions(Value: TTurboScriptOptions);
    procedure SetPC(Value: Pointer);
    procedure SetRP(Value: Pointer);
    procedure SetSP(Value: Pointer);
  protected
    FGlobalOptions: PTurboGlobalOptions;
    FMemory: TCustomTurboModule;
    FOnPrintChar: TTurboPrintCharEvent;
    procedure DoPrintChar(aChar: Char); virtual;
    procedure DoPrintShortString(const aStr: ShortString);
    procedure DoPrintString(const aStr: String); virtual;
    procedure DoReset(Sender: TObject);
    {: Finalize after the execution. }
    procedure FinalizeExecution; virtual;
    function GetStates: TTurboProcessorStates;
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function iExecuteCFA(const aCFA: Integer): Integer; virtual;
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function iExecuteExternalWord(const aWord: PTurboExteralMethodOptions;
            const aCallStyle: TCallingConvention): Integer;
    {: Init before the Execution. }
    procedure InitExecution; virtual;
    function IsGlobalOptionsExists: Boolean;
    procedure SetStates(Value: TTurboProcessorStates);
  public
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function ExecuteCFA(const aCFA: Integer): Integer;
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function ExecuteWord(const aWord: string): Integer;
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function ExecuteWordEntry(const aWord: PTurboMethodEntry): Integer;
    procedure Stop;
    property GlobalOptions: PTurboGlobalOptions read FGlobalOptions write
            SetGlobalOptions;
    property Memory: TCustomTurboModule read FMemory write SetMemory;
    property Options: TTurboScriptOptions read GetOptions write SetOptions;
    {: : program counter. }
    { Description
    program counter, which contains the address 
    in memory of the instruction that is the next 
    to be executed. 
    }
    property PC: Pointer read GetPC write SetPC;
    {: : return stack pointer(TOS). }
    { Description
    stack pointer, a register that points to the area 
    in memory utilized as the main return stack.

    the RP0-StackSize <= the stack memory < RP0.
    }
    property RP: Pointer read GetRP write SetRP;
    {: the Parameter Stack(or data stack) Pointer }
    property SP: Pointer read GetSP write SetSP;
    {: the current status of the script. }
    { Description
    the Memory is related address when the status is in the psConpiling
    until the status is not in the psConpiling.
    }
    property States: TTurboProcessorStates read GetStates write SetStates;
    property OnPrintChar: TTurboPrintCharEvent read FOnPrintChar write
            FOnPrintChar;
    property OnPrintString: TTurboPrintStringEvent read FOnPrintString write
            FOnPrintString;
  end;

  { Description
  The TTurboAppDomain is derived from the TCustomTurboModule.
  The TTurboAppDomain should be a Main Application Module too.

  allocate the memory to return stack and param stack.
  }
  TTurboAppDomain = class(TCustomTurboObject)
  private
    FExecutorClass: TTurboExecutorClass;
    FFreeMemory: Boolean;
    function GetExecutor: TCustomTurboExecutor;
    function GetParameterStackSize: Integer;
    function GetReturnStackSize: Integer;
    procedure SetExecutorClass(Value: TTurboExecutorClass);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetReturnStackSize(const Value: Integer);
  protected
    FExecutor: TCustomTurboExecutor;
    FGlobalOptions: TTurboGlobalOptions;
    FMemory: TCustomTurboModule;
    procedure DoModuleReset(Sender: TObject);
    function GetLastErrorCode: TTurboProcessorErrorCode;
    function GetParameterStack: Pointer;
    function GetReturnStack: Pointer;
    function GetStates: TTurboProcessorStates;
    procedure SetParameterStack(Value: Pointer);
    procedure SetReturnStack(Value: Pointer);
    procedure SetStates(Value: TTurboProcessorStates);
  public
    { Description
    Ϊ���ڶ�� executor �й���������ջ������ջ�Լ����в�����
    }
    constructor Create(const aMemory: TCustomTurboModule = nil);
    destructor Destroy; override;
    { Description
    }
    procedure Execute(aTimeOut : Integer = 0);
    property Executor: TCustomTurboExecutor read GetExecutor;
    property ExecutorClass: TTurboExecutorClass read FExecutorClass write
            SetExecutorClass;
    property GlobalOptions: TTurboGlobalOptions read FGlobalOptions write
            FGlobalOptions;
    property LastErrorCode: TTurboProcessorErrorCode read GetLastErrorCode;
    property Memory: TCustomTurboModule read FMemory;
    {: : the Parameter Stack }
    { Description
    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ParameterStack: Pointer read GetParameterStack write
            SetParameterStack;
    property ParameterStackSize: Integer read GetParameterStackSize write
            SetParameterStackSize;
    {: : Return(Proc) Stack }
    { Description
    ���ض�ջ

    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ReturnStack: Pointer read GetReturnStack write SetReturnStack;
    {: : Return Stack Size }
    property ReturnStackSize: Integer read GetReturnStackSize write
            SetReturnStackSize;
    {: the current status of the script. }
    { Description
    the Memory is related address when the status is in the psConpiling
    until the status is not in the psConpiling.
    }
    property States: TTurboProcessorStates read GetStates write SetStates;
  end;



  //the typecast for data memory area to get the parameters
  TTurboPreservedDataMemory = packed record
    Code: Pointer; //need relocate addr. point to the FMemory
    //GlobalOptions: PTurboGlobalOptions; //ready move to AppDomain
    Flags: Byte; //TTurboModuleFlags; modified for FPC
    //Executor: TCustomTurboExecutor; //put to the GlobalOptions
    //��ִ��������󣬿��Էŵ�ȫ�ֲ����У���ʾ��Appʹ�õ�ִ����������
    ModuleHandle: TCustomTurboModule;

    UsedMemory: tsInt;//ʵ��ʹ�õĴ�С
    MemorySize: tsInt;//����������Ĵ�С
    UsedDataSize: tsInt;
    DataSize: tsInt; 

    ModuleType: TTurboModuleType;
    ModuleOptions: LongWord; //TTurboScriptOptions; for FPC
    ModuleName: PChar;
    //if Module is Class then the ModuleParent is classParent
    //in LastModuleEntry ������
    ModuleParent: PTurboModuleRefInfo;

    //���ModuleType��ģ�飬��ô����װ�����и�ģ��ǰִ�еĳ�ʼ�����̣���ڵ�ַ
    //����Ǻ��������Ǹú�������ڵ�ַ
    InitializeProc: Pointer; //it is the offset address of the FMemory
    FinalizeProc: Pointer; //�����ģ��Ļ�
    //last Used(import) module entry.
    LastModuleRefEntry: PTurboModuleRefEntry;
    //�����ֵĺ�������ָ�����һ��������ڡ�
    LastWordEntry: PTurboMethodEntry;
    //�����ֵı�������
    LastVariableEntry: PTurboStaticFieldEntry;
    //RTTI TypeInfo ����
    LastTypeInfoEntry: PTurboTypeInfoEntry;
    LastTypeRefEntry: PTurboTypeRefEntry;
    //VMT: TTurboVirtualMethodTable; ʵ���Ͽ��Խ� LastWordEntry ����VMT! ��ʱ���� 
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
  
  TMeListEx = object(TMeList)
  public
    function Add(const aValue: Integer): Integer;
    procedure LoadFromStream(const aStream: TStream);
    procedure SaveToStream(const aStream: TStream);
  end;

  TTurboRelocatedAddresses = object(TMeListEx)
  public
    procedure Relocate(BaseAddress: Pointer; 
      const aRelocated: Boolean);
  end;
  
  TTurboRelocatedTypes = object(TMeListEx)
  protected
    Module: TCustomTurboModule; 
  protected
    function Resolve(const I: Integer): Integer;
    function UnResolve(const I: Integer): Integer;
  public
    procedure Relocate(BaseAddress: Pointer;
      const aRelocated: Boolean);
  end;
  
type
  PPTurboEntry = ^ PTurboEntry;
  PTurboEntry  = ^ TTurboEntry;
  TTurboEntry  = packed record
    Prior: Pointer;
    MetaInfo: TTurboMetaInfo;
  end;
  //aMem ���� ��ʾUnResolve(AbsolutedToRelated)
  TTurboEntryItemEventProc = procedure(const aEntry: PTurboEntry
    ; const aMem: Pointer
    //; const IsResolved: Boolean
  );
  
procedure ConvertTurboMetaInfoEntryAddr(MetaData: PTurboPreservedDataMemory;
        const IsResolved: Boolean);

//remove registered types of this module
//procedure RemoveModuleTypes(const aModuleName: string);

//var 
  //SimpleTurboTypes: TTurboSimpleTypes;


implementation

uses
  uTurboAccessor;

const
  cCurrentModuleFileForamtVersionNo = 1; 

{: walk-throughout all the entry}
{
  @param aDoOnItem  the (un)resolve address item prcoedure.
  @param Mem        the base Mem
                     Mem < 0 means UnResolve all the addresses
                     Mem > 0 means Resolve all the addresses
}
procedure ForEachTurboEntry(aEntry: PTurboEntry; const aDoOnItem:
        TTurboEntryItemEventProc; const Mem:
        Pointer 
        //; const IsResolved: Boolean = True
);
begin
  if Assigned(aEntry) and Assigned(aDoOnItem) then
  begin
    if Integer(Mem) < 0 then //restore the absoulte addr.
      Integer(aEntry) := Integer(aEntry) - Integer(Mem);
    while Assigned(aEntry) do
    begin
      aDoOnItem(aEntry, Mem);
      aEntry := aEntry.Prior;
      if Assigned(aEntry) and (Integer(Mem) < 0) then 
        Integer(aEntry) := Integer(aEntry) - Integer(Mem);
    end;
  end;
end;

function TTurboGlobalOptions.GetIsRunning(): Boolean;
begin
  {$IFDEF FPC}
  Result := TTurboProcessorStates(LongWord(States)) * [psRunning, psStepping] <> [] ;
  {$ELSE Borland}
  Result := TTurboProcessorStates(States) * [psRunning, psStepping] <> [] ;
  {$ENDIF}
end;

procedure TTurboGlobalOptions.SetIsRunning(const Value: Boolean);
{$IFDEF FPC}
var
  vStates: TTurboProcessorStates;
{$ENDIF}

begin
  {$IFDEF FPC}
  vStates := TTurboProcessorStates(LongWord(States));
  {$ENDIF}

  if Value then
    {$IFDEF FPC}
    Include(vStates, psRunning)
    {$ELSE Borland}
    Include(TTurboProcessorStates(States), psRunning)
    {$ENDIF}
  else
    {$IFDEF FPC}
    Exclude(vStates, psRunning);
    {$ELSE Borland}
    Exclude(TTurboProcessorStates(States), psRunning);
    {$ENDIF}

  {$IFDEF FPC}
  States := Byte(LongWord(vStates));
  {$ENDIF}
end;
  
{
****************************** TCustomTurboModule ******************************
}
constructor TCustomTurboModule.Create(const aOwner: TCustomTurboModule = nil;
        aVisibility: TTurboVisibility = tvPublicVisibilty);
begin
  inherited Create;
  FModuleUnloadNotifies := TList.Create;
  FOwner := aOwner;
  FVisibility := aVisibility;
  //FOptions := [soLoadOnDemand];
  New(FRegisteredTypes, Create);
  New(FRelocatedDataAddresses, Create);
  New(FRelocatedDataTypes, Create);
  FRelocatedDataTypes.Module := Self;
  ClearMemory;
end;

destructor TCustomTurboModule.Destroy;
begin
  Unload;

  FreeAndNil(FModuleUnloadNotifies);
  MeFreeAndNil(FRegisteredTypes);
  MeFreeAndNil(FRelocatedDataAddresses);
  MeFreeAndNil(FRelocatedDataTypes);
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

procedure TCustomTurboModule.AddPCharToData(const aValue: string);
var
  p: Pointer;
begin
  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    Integer(p) := Length(aValue)+1;
    if (UsedDataSize+Integer(p)) > DataMemorySize then
      GrowData(Integer(p));
    Integer(p) := Integer(FDataMemory) + UsedDataSize;
    if aValue <> '' then
    begin
      Move(aValue[1], p^, Length(aValue));
      Inc(Integer(p), Length(aValue));
    end;
    PByte(p)^ := 0;
    Inc(UsedDataSize, Length(aValue) + 1);
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
    if Assigned(FDataMemory) then FreeDLLLibs;
    //vPreserved := SizeOf(TTurboPreservedDataMemory);
    //if vPreserved < Integer(High(TTurboVMInstruction)) then
        //vPreserved := Integer(High(TTurboVMInstruction));
    vPreserved := cDefaultDataMemSize+SizeOf(TTurboPreservedDataMemory);
    ReallocMem(FMemory, cDefaultDataMemSize);
    ReallocMem(FDataMemory, vPreserved);
    FRelocatedDataAddresses.Clear;
    FRelocatedDataTypes.Clear;
    //RegisteredTypes.Clear; // i wish keep it in the mem even unload.
    with PTurboPreservedDataMemory(FDataMemory)^ do
    begin
      MemorySize := cDefaultDataMemSize;
      UsedMemory := 0;
      Flags := 0;
      Code := FMemory;
      DataSize := vPreserved;
      UsedDataSize := SizeOf(TTurboPreservedDataMemory);//SizeOf(tsInt); //preserved the first integer
      TTurboScriptOptions(ModuleOptions) := [soAssertSupport];

      InitializeProc := nil;
      FinalizeProc := nil;
      LastWordEntry := nil;
      LastVariableEntry := nil;
      LastTypeInfoEntry := nil;
      LastModuleRefEntry := nil;
      LastTypeRefEntry := nil;
      //States := [];
    end;

    IsLoaded := False;
  end;
end;

function TCustomTurboModule.FindModuleRefEntry(const aName: string):
        PTurboModuleRefEntry;
var
  vName: PChar;
begin
  Result := LastModuleRefEntry;
  while (Result <> nil) do
  begin
    if not IsAddrResolved then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.Module.Name;
    if Assigned(vName) then
    begin
      if not IsAddrResolved then
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

function TCustomTurboModule.FindModuleRefEntryBy(const aModule: Pointer):
        PTurboModuleRefEntry;
begin
  Result := LastModuleRefEntry;
  while (Result <> nil) do
  begin
    if not IsAddrResolved then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    if Result.Module.Handle = aModule then exit;
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
    if not IsAddrResolved then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.TypeInfo.Name;
    if Assigned(vName) then
    begin
      if not IsAddrResolved then
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

function TCustomTurboModule.FindTypeRefEntry(const aName: string):
        PTurboTypeRefEntry;
var
  vName: PChar;
begin
  Result := LastTypeRefEntry;
  while (Result <> nil) do
  begin
    if not IsAddrResolved then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.TypeRef.Name;
    if Assigned(vName) then
    begin
      if not IsAddrResolved then
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

function TCustomTurboModule.FindVariableEntry(const aName: string):
        PTurboStaticFieldEntry;
var
  vName: PChar;
begin
  Result := LastVariableEntry;
  while (Result <> nil) do
  begin
    if not IsAddrResolved then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.Variable.Name;
    if Assigned(vName) then
    begin
      if not IsAddrResolved then
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

function TCustomTurboModule.FindWordEntry(const aName: string; aCodeFieldStyle:
        TTurboCodeFieldStyle = cfsFunction; const aCallStyle:
        TCallingConvention = ccUnknown): PTurboMethodEntry;
var
  vName: PChar;
begin
  Result := LastWordEntry;
  while (Result <> nil) do
  begin
    if not IsAddrResolved then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    vName := Result.Word.Name;
    if Assigned(vName) then
    begin
      if not IsAddrResolved then
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
      if (aCodeFieldStyle = cfsFunction) or (aCallStyle = ccUnknown) then
      begin
        if AnsiSameText(vName, aName) then exit;
      end
      else if (Result.Word.CallStyle = aCallStyle) and (Result.Word.CodeFieldStyle = aCodeFieldStyle) and AnsiSameText(vName, aName) then
      begin
        Exit;
      end;
    end;
    Result := Result.Prior;
  end;
  Result := nil;
end;

procedure TCustomTurboModule.FreeDLLLibs;
var
  vModuleRefEntry: PTurboModuleRefEntry;
begin
  vModuleRefEntry := LastModuleRefEntry;
  while (vModuleRefEntry <> nil) do
  begin
    if not IsAddrResolved then
    begin
      Integer(vModuleRefEntry) := Integer(FDataMemory) + Integer(vModuleRefEntry);
    end;
    vModuleRefEntry.Module.FreeDLLHandle;
    vModuleRefEntry := vModuleRefEntry.Prior;
  end;
end;

function TCustomTurboModule.GetDataMemorySize: tsInt;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).DataSize;
end;

function TCustomTurboModule.GetInitializeProc: Integer;
begin
  Result := Integer(PTurboPreservedDataMemory(FDataMemory)^.InitializeProc);
end;

function TCustomTurboModule.GetIsAddrResolved: Boolean;
begin
  {$IFDEF FPC}
  Result := tfAddrResolved in TTurboModuleFlags(LongWord(PTurboPreservedDataMemory(FDataMemory).Flags));
  {$ELSE Borland}
  Result := tfAddrResolved in TTurboModuleFlags(PTurboPreservedDataMemory(FDataMemory).Flags);
  {$ENDIF}
end;

function TCustomTurboModule.GetLastModuleRefEntry: PTurboModuleRefEntry;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).LastModuleRefEntry;
end;

function TCustomTurboModule.GetLastTypeInfoEntry: PTurboTypeInfoEntry;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).LastTypeInfoEntry;
end;

function TCustomTurboModule.GetLastTypeRefEntry: PTurboTypeRefEntry;
begin
  Result := PTurboPreservedDataMemory(FDataMemory).LastTypeRefEntry;
end;

function TCustomTurboModule.GetLastVariableEntry: PTurboStaticFieldEntry;
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
  Result := TTurboScriptOptions(PTurboPreservedDataMemory(FDataMemory).ModuleOptions);
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
    Result := PTurboMethodEntry(Result).Word.MethodAddr;
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

function TCustomTurboModule.IndexOfUnloadNotification(aProc: TNotifyEvent):
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

procedure TCustomTurboModule.iReset;
begin
  //PPreservedCodeMemory(FMemory).ParamStackBase := FParameterStack;
  //PPreservedCodeMemory(FMemory).ParamStackSize := FParameterStackSize;

  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    //SP := Integer(GlobalOptions.ParamStackBase) + GlobalOptions.ParamStackSize;
    //RP := Integer(GlobalOptions.ReturnStackBase) + GlobalOptions.ReturnStackSize;
    //GlobalOptions.LastErrorCode := errNone;
    Code := FMemory;
    ModuleHandle := Self;
    //Executor := Self;
    //GlobalOptions.States := []; //������Ķ����ͱ𶯣���
  end;
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
  p: PChar;
  I: Integer;
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

  FRelocatedDataAddresses.LoadFromStream(aStream);
  FRelocatedDataTypes.LoadFromStream(aStream);

  //read the FRegisteredTypes size+1 in the stream
  aStream.ReadBuffer(I, SizeOf(I));
  if FRegisteredTypes.Count > 0 then
    //already loaded in mem, so skip
     aStream.Position := I
  else
    FRegisteredTypes.LoadFromStream(aStream);

  UsedDataSize := SizeOf(TTurboPreservedDataMemory);
  aStream.ReadBuffer(FDataMemory^, SizeOf(TTurboPreservedDataMemory));
  ReallocMem(FDataMemory, DataMemorySize);
  //if UsedDataSize
  Integer(p) := Integer(FDataMemory) + SizeOf(TTurboPreservedDataMemory);
  aStream.ReadBuffer(p^, UsedDataSize-SizeOf(TTurboPreservedDataMemory));

  ReallocMem(FMemory, MemorySize);
  aStream.ReadBuffer(FMemory^, UsedMemory);

  if Assigned(PTurboPreservedDataMemory(FDataMemory).ModuleName) then
  begin
    Integer(p) := Integer(FDataMemory) + Integer(PTurboPreservedDataMemory(FDataMemory).ModuleName);
    Name := p;
  end
  else
    Name := '';

  Reset;
  //if not (psCompiling in Status) then TurboConvertAddrRelatedToAbsolute(FMemory, FDataMemory);

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
  vName: PChar;
begin
  vModuleRefEntry := LastModuleRefEntry;
  while (vModuleRefEntry <> nil) do
  begin
    vName := vModuleRefEntry.Module.Name;
    if not IsAddrResolved then
    begin
      Integer(vModuleRefEntry) := Integer(FDataMemory) + Integer(vModuleRefEntry);
      if Assigned(vName) then
        Integer(vName) := Integer(FDataMemory) + Integer(vName);
    end;
    vModule := nil;
    case vModuleRefEntry.Module.ModuleType of
      mtLib, mtProgram: vModule := RequireModule(vName);
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

  vModuleRef := FindModuleRefEntryBy(Sender);
  if Assigned(vModuleRef) then
  begin
    vModuleRef.Module.Handle := nil;

    ResetTypeRefsBy(@vModuleRef.Module);
  end;
end;

procedure TCustomTurboModule.RemoveUnloadNotification(aProc: TNotifyEvent);
var
  I: Integer;
begin
  i := IndexOfUnloadNotification(aProc);
  if i >= 0 then
    FModuleUnloadNotifies.Delete(i);
end;

function TCustomTurboModule.RequireModule(const aModuleName: PChar):
        TCustomTurboModule;
begin
  //Bug here: this should make the AppDomain module, and memory leak here!!!
  //Result := GTurboModuleManager.Require(aModuleName, TTurboModuleClass(ClassType), GlobalOptions, True);
  //now, the requiered module is always the TCustomTurboModule
  Result := GTurboModuleManager.Require(aModuleName, TCustomTurboModule, True);

  if Assigned(Result) then
  begin
    Result.UnloadNotification(NotifyModuleUnloaded);
    FreeNotification(Result.NotifyModuleFree);
    Result.IsAddrResolved := IsAddrResolved;
  end
end;

procedure TCustomTurboModule.Reset;
begin
  iReset;
  if Assigned(FOnReset) then FOnReset(Self);
end;

procedure TCustomTurboModule.ResetTypeRefsBy(const aModuleRef:
        PTurboModuleRefInfo);
var
  vEntry: PTurboTypeRefEntry;
begin
  vEntry := LastTypeRefEntry;
  while (vEntry <> nil) do
  begin
    if not IsAddrResolved then
      Integer(vEntry) := Integer(FDataMemory) + Integer(vEntry);
       if Assigned(vEntry.TypeRef.TurboType)
          and (vEntry.TypeRef.ModuleRef = aModuleRef)
       then
         vEntry.TypeRef.TurboType := nil;
    vEntry := vEntry.Prior;
  end;
end;

procedure TCustomTurboModule.ResolveAddress;
var
  I: Integer;
  vTypeInfoEntry: PTurboTypeInfoEntry;
begin
  ConvertTurboMetaInfoEntryAddr(FDataMemory, True);
  vTypeInfoEntry := LastTypeInfoEntry;
  while assigned(vTypeInfoEntry) do
  begin
    I := Integer(vTypeInfoEntry.TypeInfo.TurboType);
    Assert(I >= 0, 'vTypeInfoEntry.TypeInfo.TurboType index Too small.');
    Assert(I < RegisteredTypes.Count, 'vTypeInfoEntry.TypeInfo.TurboType index Too big.');
    vTypeInfoEntry.TypeInfo.TurboType := RegisteredTypes.Items[I];
    vTypeInfoEntry := vTypeInfoEntry.Prior;
  end;

  {PTurboMethodEntry(vTypeInfoEntry) := LastWordEntry;
  while assigned(vTypeInfoEntry) do
  begin
    I := Integer(PTurboMethodEntry(vTypeInfoEntry).Word.TurboType);
    if I < 0 then
    begin
      I := -I;
      Assert(I < GRegisteredTypes.Count, 'vMethodInfoEntry.TurboType index is too big for the GRegisteredTypes.');
      PTurboMethodEntry(vTypeInfoEntry).Word.TurboType := GRegisteredTypes.Items[I];
    end
    else if I > 0 then
    begin
      Dec(I);
      Assert(I < RegisteredTypes.Count, 'vMethodInfoEntry.TurboType index is too big for the RegisteredTypes.');
      PTurboMethodEntry(vTypeInfoEntry).Word.TurboType := RegisteredTypes.Items[I];
    end;
    vTypeInfoEntry := vTypeInfoEntry.Prior;
  end;
  }

  FRelocatedDataAddresses.Relocate(FDataMemory, True);
  FRelocatedDataTypes.Relocate(FDataMemory, True);
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
  vFlags: TTurboModuleFlags;
  I: Integer;
  vPos: Integer;
begin
  if not FIsLoaded then
    raise ETurboScriptError.CreateRes(@rsTurboScriptNotLoadedError);

  vHeader.Id := cFORTHHeaderMagicId;
  vHeader.Revision := ModuleVersion;
  vHeader.Version  := cCurrentModuleFileForamtVersionNo;
  vHeader.BuildDate := ModuleDate;
  aStream.WriteBuffer(vHeader, SizeOf(TTurboModuleStreamHeader));

  if IsAddrResolved then UnResolveAddress;

  FRelocatedDataAddresses.SaveToStream(aStream);
  FRelocatedDataTypes.SaveToStream(aStream);

  vPos := aStream.Position;
  I := 0;
  aStream.WriteBuffer(I, SizeOf(I));
  FRegisteredTypes.SaveToStream(aStream);
  I := aStream.Position;
  aStream.Position := vPos;
  aStream.WriteBuffer(I, SizeOf(I));
  aStream.Position := I;

  with PTurboPreservedDataMemory(FDataMemory)^ do
  begin
    //backup the data in the Memory
    {$IFDEF FPC}
    vFlags := TTurboModuleFlags(LongWord(Flags));
    {$ELSE Borland}
    vFlags := TTurboModuleFlags(Flags);
    {$ENDIF}
    Flags := 0;
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
    {$IFDEF FPC}
    Flags := Byte(LongWord(vFlags));
    {$ELSE Borland}
    Flags := Byte(vFlags);
    {$ENDIF}
    Code := FMemory;
  {  ParameterStack := vParameterStack;
    ReturnStack := vReturnStack;
    ReturnStackSize := vReturnStackSize;
    ParameterStackSize := vParameterStackSize;
  }
  end;

  if IsAddrResolved then ResolveAddress;
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

procedure TCustomTurboModule.SetIsAddrResolved(const Value: Boolean);
var
  vFlags: TTurboModuleFlags;
begin
  {$IFDEF FPC}
  vFlags := TTurboModuleFlags(LongWord(PTurboPreservedDataMemory(FDataMemory).Flags));
  {$ELSE Borland}
  vFlags := TTurboModuleFlags(PTurboPreservedDataMemory(FDataMemory).Flags);
  {$ENDIF}

  if (tfAddrResolved in vFlags) <> Value then
  begin
    if Value then
    begin
      ResolveAddress;
      Include(vFlags, tfAddrResolved);
    end
    else
    begin
      UnResolveAddress;
      Exclude(vFlags, tfAddrResolved);
    end;
    {$IFDEF FPC}
    PTurboPreservedDataMemory(FDataMemory).Flags := Byte(LongWord(vFlags));
    {$ELSE Borland}
    PTurboPreservedDataMemory(FDataMemory).Flags := Byte(vFlags);
    {$ENDIF}
  end;
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

procedure TCustomTurboModule.SetLastTypeRefEntry(const Value:
        PTurboTypeRefEntry);
begin
  PTurboPreservedDataMemory(FDataMemory).LastTypeRefEntry := Value;
end;

procedure TCustomTurboModule.SetLastVariableEntry(const Value:
        PTurboStaticFieldEntry);
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
  PTurboPreservedDataMemory(FDataMemory).ModuleOptions := LongWord(Value);
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

procedure TCustomTurboModule.SetUsedDataSize(Value: tsInt);
begin
  if Value < SizeOf(TTurboPreservedDataMemory) then
    Value  := SizeOf(TTurboPreservedDataMemory);

  if (Value > DataMemorySize) then
  begin
    if not IsAddrResolved then
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
    if not IsAddrResolved then
      Grow(Value - MemorySize)
    else
      Raise Exception.Create('Out Of Memory');
  end;

  PTurboPreservedDataMemory(FDataMemory).UsedMemory := Value;
end;

function TCustomTurboModule.StoredInOwner: Boolean;
begin
  Result := Assigned(Owner) and
     ((Visibility = tvPrivateVisibilty)
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
  if IndexOfUnloadNotification(aProc) < 0 then
  begin
    FModuleUnloadNotifies.Insert(0, Pointer(TMethod(aProc).Data));
    FModuleUnloadNotifies.Insert(0, Pointer(TMethod(aProc).Code));
  end;
end;

procedure TCustomTurboModule.UnResolveAddress;
var
  I: Integer;
  vTypeInfoEntry: PTurboTypeInfoEntry;
begin
  FRelocatedDataAddresses.Relocate(FDataMemory, False);
  FRelocatedDataTypes.Relocate(FDataMemory, False);

  //TurboConvertAddrAbsoluteToRelated(FMemory, FDataMemory);
  vTypeInfoEntry := LastTypeInfoEntry;
  while assigned(vTypeInfoEntry) do
  begin
    I := RegisteredTypes.IndexOf(vTypeInfoEntry.TypeInfo.TurboType);
    Assert(I >= 0, 'vTypeInfoEntry.TurboType Not found in the RegisteredTypes.');
    Integer(vTypeInfoEntry.TypeInfo.TurboType) := I;
    vTypeInfoEntry := vTypeInfoEntry.Prior;
  end;

  {PTurboMethodEntry(vTypeInfoEntry) := LastWordEntry;
  while assigned(vTypeInfoEntry) do
  begin
    if Assigned(PTurboMethodEntry(vTypeInfoEntry).Word.TurboType) then
    begin
      I := RegisteredTypes.IndexOf(PTurboMethodEntry(vTypeInfoEntry).Word.TurboType);
      if I < 0 then
      begin
        I := GRegisteredTypes.IndexOf(PTurboMethodEntry(vTypeInfoEntry).Word.TurboType);
        Assert(I >= 0, 'vMethodInfoEntry.TurboType Not found in the GRegisteredTypes.');
        I := -I;
      end
      else
        Inc(I);
      Integer(PTurboMethodEntry(vTypeInfoEntry).Word.TurboType) := I;
    end;
    vTypeInfoEntry := vTypeInfoEntry.Prior;
  end; //}


  ConvertTurboMetaInfoEntryAddr(FDataMemory, False);
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
  //put into the TurboAppDomain now.
  {if IsGlobalOptionsExists then
  with PTurboPreservedDataMemory(FMemory.FDataMemory).GlobalOptions^ do
  begin
    Integer(_SP) := Integer(ParamStackBase) + ParamStackSize;
    Integer(_RP) := Integer(ReturnStackBase) + ReturnStackSize;
    Executor := Self;
  end;
  //}
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

  if (aWord.Word.CallStyle = ccForth) and (aWord.Word.Visibility = tvPrivateVisibilty) then
  begin
    Result := iExecuteCFA(aWord.Word.MethodAddr);
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

function TCustomTurboExecutor.GetOptions: TTurboScriptOptions;
begin
  if Assigned(FMemory) then
    Result := TTurboScriptOptions(PTurboPreservedDataMemory(FMemory.FDataMemory).ModuleOptions)
  else
    //Result := [];
    Raise ETurboScriptError.Create(rsTurboScriptNoMemError);
end;

function TCustomTurboExecutor.GetPC: Pointer;
begin
  if IsGlobalOptionsExists then
    Result := Pointer(FGlobalOptions._PC)
  else
    Result := nil;
end;

function TCustomTurboExecutor.GetRP: Pointer;
begin
  if IsGlobalOptionsExists then
    Result := Pointer(FGlobalOptions._RP)
  else
    Result := nil;
end;

function TCustomTurboExecutor.GetSP: Pointer;
begin
  if IsGlobalOptionsExists then
    Result := Pointer(FGlobalOptions._SP)
  else
    Result := nil;
end;

function TCustomTurboExecutor.GetStates: TTurboProcessorStates;
begin
  if Assigned(FGlobalOptions) then
    {$IFDEF FPC}
    Result := TTurboProcessorStates(LongWord(FGlobalOptions.States))
    {$ELSE Borland}
    Result := TTurboProcessorStates(FGlobalOptions.States)
    {$ENDIF}
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
        PTurboExteralMethodOptions; const aCallStyle: TCallingConvention):
        Integer;
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

  if not Assigned(FGlobalOptions) then
    Raise ETurboScriptError.Create(rsTurboScriptNoGlobalOptionsError);

  if (FGlobalOptions.IsRunning) then
    raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);


  FGlobalOptions.IsRunning := True;
  //TTurboProcessorStates(FGlobalOptions.States) := TTurboProcessorStates(FGlobalOptions.States) + [psRunning];
  FMemory.IsAddrResolved := True;
  //Include(PTurboPreservedDataMemory(FDataMemory).GlobalOptions.States, psRunning);
end;

function TCustomTurboExecutor.IsGlobalOptionsExists: Boolean;
begin
  Result := Assigned(FMemory);
  if not Result then
    Raise ETurboScriptError.Create(rsTurboScriptNoMemError);

  if Result then
    Result := Assigned(FGlobalOptions);

  if not Result then
    Raise ETurboScriptError.Create(rsTurboScriptNoGlobalOptionsError);
end;

procedure TCustomTurboExecutor.SetGlobalOptions(Value: PTurboGlobalOptions);
begin
  if FGlobalOptions <> Value then
  begin
    if Assigned(FGlobalOptions) then
      FGlobalOptions.Executor := nil;

    FGlobalOptions := Value;

    if Assigned(FGlobalOptions) then
      FGlobalOptions.Executor := Self;
  end;
end;

procedure TCustomTurboExecutor.SetMemory(const Value: TCustomTurboModule);
begin
  if (Value <> FMemory) then
  begin
    if not Assigned(FMemory) or not (psRunning in States) then
    begin
      if Assigned(FMemory) then
      begin
        FMemory.OnReset := nil;
        FGlobalOptions := nil;
      end;
      FMemory := Value;
      //if Assigned(FMemory) then
        //FMemory.OnReset := DoReset;
    end;
  end;
end;

procedure TCustomTurboExecutor.SetOptions(Value: TTurboScriptOptions);
begin
  if Assigned(FMemory) then
    PTurboPreservedDataMemory(FMemory.FDataMemory).ModuleOptions := LongWord(Value);
end;

procedure TCustomTurboExecutor.SetPC(Value: Pointer);
begin
  if IsGlobalOptionsExists then
    FGlobalOptions._PC := tsInt(Value);
end;

procedure TCustomTurboExecutor.SetRP(Value: Pointer);
begin
  if IsGlobalOptionsExists then
    FGlobalOptions._RP := tsInt(Value);
end;

procedure TCustomTurboExecutor.SetSP(Value: Pointer);
begin
  if IsGlobalOptionsExists then
    FGlobalOptions._SP := tsInt(Value);
end;

procedure TCustomTurboExecutor.SetStates(Value: TTurboProcessorStates);
begin
  if Assigned(FGlobalOptions) then
    {$IFDEF FPC}
    FGlobalOptions.States := Byte(LongWord(Value));
    {$ELSE Borland}
    FGlobalOptions.States := Byte(Value);
    {$ENDIF}
end;

procedure TCustomTurboExecutor.Stop;
begin
  if Assigned(FGlobalOptions) then
    FGlobalOptions.IsRunning := False;
end;

{
******************************* TTurboAppDomain ********************************
}
constructor TTurboAppDomain.Create(const aMemory: TCustomTurboModule = nil);
begin
  inherited Create;
  //GlobalOptions := @FGlobalOptions;
  if not Assigned(aMemory) then
  begin
    FMemory := TCustomTurboModule.Create();
    FMemory.OnReset := DoModuleReset;
    FFreeMemory := True;
  end;
  ParameterStackSize := cDefaultParamStackSize;
  ReturnStackSize := cDefaultReturnStackSize;
end;

destructor TTurboAppDomain.Destroy;
begin
  FreeMem(ReturnStack);
  ReturnStack := nil;
  FreeMem(ParameterStack);
  ParameterStack := nil;
  FreeAndNil(FExecutor);
  if FFreeMemory then FreeAndNil(FMemory);
  inherited Destroy;
end;

procedure TTurboAppDomain.DoModuleReset(Sender: TObject);
begin
  with FGlobalOptions do
  begin
    Integer(_SP) := Integer(ParamStackBase) + ParamStackSize * SizeOf(tsInt);
    Integer(_RP) := Integer(ReturnStackBase) + ReturnStackSize * SizeOf(Pointer);
    LastErrorCode := errNone;
  end;
  if not (psCompiling in States) then
    FMemory.IsAddrResolved := True;

  GTurboModuleManager.Add(FMemory);
end;

procedure TTurboAppDomain.Execute(aTimeOut : Integer = 0);

  {$IFDEF FPC}
  var
    vFlags: TTurboModuleFlags;
  {$ENDIF}

begin
  if Assigned(Executor) then
  begin
    if States * [psRunning, psStepping] <> [] then
      Raise ETurboScriptError.Create(rsTurboScriptAlreayRunningError);
    //IsAddrResolved := True; //done in executor
    {$IFDEF FPC}
    vFlags := TTurboModuleFlags(LongWord(PTurboPreservedDataMemory(FMemory.FDataMemory).Flags));
    Include(vFlags, tfInited);
    PTurboPreservedDataMemory(FMemory.FDataMemory).Flags := Byte(LongWord(vFlags));
    {$ELSE Borland}
    Include(TTurboModuleFlags(PTurboPreservedDataMemory(FMemory.FDataMemory).Flags), tfInited);
    {$ENDIF}
    FExecutor.ExecuteCFA(FMemory.InitializeProc);
  end;
end;

function TTurboAppDomain.GetExecutor: TCustomTurboExecutor;
begin
  if not Assigned(FExecutor) then
  begin
    FExecutor := ExecutorClass.Create;//(Self, fvPrivate);
    FExecutor.Memory := FMemory;
    FExecutor.GlobalOptions := @FGlobalOptions;
    //FGlobalOptions.Executor := FExecutor;
  end;
  Result := FExecutor;
end;

function TTurboAppDomain.GetLastErrorCode: TTurboProcessorErrorCode;
begin
  Result := FGlobalOptions.LastErrorCode;
end;

function TTurboAppDomain.GetParameterStack: Pointer;
begin
  Result := FGlobalOptions.ParamStackBase;
end;

function TTurboAppDomain.GetParameterStackSize: Integer;
begin
  Result := FGlobalOptions.ParamStackSize;
end;

function TTurboAppDomain.GetReturnStack: Pointer;
begin
  Result := FGlobalOptions.ReturnStackBase;
end;

function TTurboAppDomain.GetReturnStackSize: Integer;
begin
  Result := FGlobalOptions.ReturnStackSize;
end;

function TTurboAppDomain.GetStates: TTurboProcessorStates;
begin
  {$IFDEF FPC}
  Result := TTurboProcessorStates(LongWord(FGlobalOptions.States));
  {$ELSE Borland}
  Result := TTurboProcessorStates(FGlobalOptions.States);
  {$ENDIF}
end;

procedure TTurboAppDomain.SetExecutorClass(Value: TTurboExecutorClass);
begin
  if FExecutorClass <> Value then
  begin
    if Assigned(FExecutor) then
    begin
      if FGlobalOptions.IsRunning then
        Raise ETurboScriptError.Create(rsTurboScriptAlreayRunningError);
      FreeAndNil(FExecutor);
      FGlobalOptions.Executor := nil;
    end;
    FExecutorClass := Value;
  end;
end;

procedure TTurboAppDomain.SetParameterStack(Value: Pointer);
begin
  FGlobalOptions.ParamStackBase := Value;
end;

procedure TTurboAppDomain.SetParameterStackSize(const Value: Integer);
begin
  if not FGlobalOptions.IsRunning and (ParameterStackSize <> Value) then
  begin
    FGlobalOptions.ParamStackSize := Value;
    ReallocMem(FGlobalOptions.ParamStackBase, (Value+1)*SizeOf(tsInt));
    with FGlobalOptions do
      Integer(_SP) := Integer(ParamStackBase) + Value * SizeOf(tsInt);
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboAppDomain.SetReturnStack(Value: Pointer);
begin
  FGlobalOptions.ReturnStackBase := Value;
end;

procedure TTurboAppDomain.SetReturnStackSize(const Value: Integer);
begin
  if not FGlobalOptions.IsRunning and (ReturnStackSize <> Value) then
  begin
    FGlobalOptions.ReturnStackSize := Value;
    ReallocMem(FGlobalOptions.ReturnStackBase, (Value+1)*SizeOf(Pointer));
    with FGlobalOptions do
      Integer(_RP) := Integer(ReturnStackBase) + Value*SizeOf(Pointer);
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboAppDomain.SetStates(Value: TTurboProcessorStates);
var
  vChanged: Boolean;
begin
  with FGlobalOptions do
  {$IFDEF FPC}
  if (Value <> TTurboProcessorStates(LongWord(States))) then
  {$ELSE Borland}
  if (Value <> TTurboProcessorStates(States)) then
  {$ENDIF}
  begin
  {$IFDEF FPC}
    vChanged := psCompiling in (Value * TTurboProcessorStates(LongWord(States)));
    States := Byte(LongWord(Value));
  {$ELSE Borland}
    vChanged := psCompiling in (Value * TTurboProcessorStates(States));
    TTurboProcessorStates(States) := Value;
  {$ENDIF}
    if vChanged then
    begin
      if psCompiling in Value then
        FMemory.UnResolveAddress
      else
        FMemory.ResolveAddress;
    end;
  end;
end;


procedure DoTurboEntryItemProc(const aEntry: PTurboEntry
    ; const aMem: Pointer
    //; const IsResolved: Boolean
  );
begin
  with aEntry^ do
  begin
    if Prior <> nil then
      Integer(Prior) := Integer(Prior) + Integer(aMem);
    if Assigned(MetaInfo.Name) then
        MetaInfo.Name := Pointer(Integer(MetaInfo.Name) + Integer(aMem));
  end;
end;

procedure DoStaticFieldEntryItemProc(const aEntry: PTurboEntry
    ; const aMem: Pointer
    //; const IsResolved: Boolean
  );
begin
  with PTurboStaticFieldEntry(aEntry)^ do
  begin
    if Prior <> nil then
      Integer(Prior) := Integer(Prior) + Integer(aMem);
    if Assigned(Variable.Name) then
        Variable.Name := Pointer(Integer(Variable.Name) + Integer(aMem));
    //if Assigned(Variable.Addr) then //because the private var is not here!!
        //Variable.Addr := Pointer(Integer(Variable.Addr) + Integer(aMem));  
    if Assigned(Variable.TypeInfo) then
        Variable.TypeInfo := Pointer(Integer(Variable.TypeInfo) + Integer(aMem));  
  end;
end;

procedure DoMethodEntryItemProc(const aEntry: PTurboEntry
    ; const aMem: Pointer
    //; const IsResolved: Boolean
  );
begin
  with PTurboMethodEntry(aEntry)^ do
  begin
    if Prior <> nil then
      Integer(Prior) := Integer(Prior) + Integer(aMem);
    if Assigned(Word.Name) then
        Word.Name := Pointer(Integer(Word.Name) + Integer(aMem));
    //if Assigned(Word.TypeInfo) then
        //Word.TypeInfo := Pointer(Integer(Word.TypeInfo) + Integer(aMem));
    {//ExternalOptions.ModuleRef �ĵ�ַ�ض�λ�ŵ�RelocatedDataAddresses���У�      
    if Word.IsExternal then 
    begin
      with Word.GetExternalOptionsAddr^ do
        if Assigned(ModuleRef) then
          ModuleRef := Pointer(Integer(ModuleRef) + Integer(aMem));
    end; //}  
  end;
end;

procedure DoTypeRefEntryItemProc(const aEntry: PTurboEntry
    ; const aMem: Pointer
    //; const IsResolved: Boolean
  );
begin
  with PTurboTypeRefEntry(aEntry)^ do
  begin
    if Prior <> nil then
      Integer(Prior) := Integer(Prior) + Integer(aMem);
    if Assigned(TypeRef.Name) then
        TypeRef.Name := Pointer(Integer(TypeRef.Name) + Integer(aMem));
    if Assigned(TypeRef.ModuleRef) then
    begin
      TypeRef.ModuleRef := Pointer(Integer(TypeRef.ModuleRef) + Integer(aMem));
    end;  
  end;
end;

procedure ConvertTurboEntry(const Mem: Pointer; var aEntry: PTurboEntry
  //; const IsResolved: Boolean
);
begin
  if Assigned(aEntry) then
  begin
    Integer(aEntry) := Integer(aEntry) + Integer(Mem);
    ForEachTurboEntry(aEntry, DoTurboEntryItemProc, Mem);
  end;
end;

procedure ConvertTurboStaticFieldEntry(const Mem: Pointer ; var aEntry:
        PTurboStaticFieldEntry 
        //; const IsResolved: Boolean
);
begin
  if Assigned(aEntry) then
  begin
    Integer(aEntry) := Integer(aEntry) + Integer(Mem);
    ForEachTurboEntry(PTurboEntry(aEntry), DoStaticFieldEntryItemProc, Mem);
  end;
end;

procedure ConvertTurboMethodEntry(const Mem: Pointer ; var aEntry:
        PTurboMethodEntry 
        //; const IsResolved: Boolean
);
begin
  if Assigned(aEntry) then
  begin
    Integer(aEntry) := Integer(aEntry) + Integer(Mem);
    ForEachTurboEntry(PTurboEntry(aEntry), DoMethodEntryItemProc, Mem);
  end;
end;

procedure ConvertTurboTypeRefEntry(const Mem: Pointer ; var aEntry:
        PTurboTypeRefEntry 
        //; const IsResolved: Boolean
);
begin
  if Assigned(aEntry) then
  begin
    Integer(aEntry) := Integer(aEntry) + Integer(Mem);
    ForEachTurboEntry(PTurboEntry(aEntry), DoTypeRefEntryItemProc, Mem);
  end;
end;

procedure ConvertTurboMetaInfoEntryAddr(MetaData: PTurboPreservedDataMemory;
        const IsResolved: Boolean);
var
  vMem: Pointer;
  vEntry: PTurboEntry;
begin
  vMem := MetaData;
  if not IsResolved then
     Integer(vMem) := - Integer(vMem);
  if Assigned(MetaData.ModuleName) then
    Integer(MetaData.ModuleName) := Integer(MetaData.ModuleName) + Integer(vMem);

  ConvertTurboEntry(vMem, PTurboEntry(MetaData.LastModuleRefEntry));  
  ConvertTurboEntry(vMem, PTurboEntry(MetaData.LastTypeInfoEntry));  
  ConvertTurboTypeRefEntry(vMem, MetaData.LastTypeRefEntry);  
  ConvertTurboStaticFieldEntry(MetaData, MetaData.LastVariableEntry);  
  ConvertTurboMethodEntry(vMem, MetaData.LastWordEntry);  
end;

{TMeListEx}
function TMeListEx.Add(const aValue: Integer): Integer;
begin
  Result := IndexOf(Pointer(aValue));
  if Result < 0 then
    Result := inherited Add(Pointer(aValue));
end;

procedure TMeListEx.LoadFromStream(const aStream: TStream);
var
  i: Integer;
begin
  Clear;
  aStream.ReadBuffer(i, SizeOf(i));
  Count := i;
  for i := 0 to Count -1 do
     aStream.ReadBuffer(List^[i], SizeOf(Pointer));
end;

procedure TMeListEx.SaveToStream(const aStream: TStream);
var
  i: Integer;
begin
  i := Count;
  aStream.WriteBuffer(i, SizeOf(i));
  for i := 0 to Count -1 do
     aStream.WriteBuffer(List^[i], SizeOf(Pointer));
end;

{TTurboRelocatedAddresses}
procedure TTurboRelocatedAddresses.Relocate(BaseAddress: Pointer; 
      const aRelocated: Boolean);
var
  i: Integer;
  //v: PInteger;
begin
  for I := 0 to Count -1 do
  begin
    if aRelocated then
    begin
      Integer(List^[I]) := Integer(List^[I]) + Integer(BaseAddress);
      if PInteger(List^[I])^ <> 0 then
        PInteger(List^[I])^ := PInteger(List^[I])^ + Integer(BaseAddress); 
    end
    else
    begin
      if PInteger(List^[I])^ <> 0 then
        PInteger(List^[I])^ := PInteger(List^[I])^ - Integer(BaseAddress); 
      Integer(List^[I]) := Integer(List^[I]) - Integer(BaseAddress);
    end;
  end;
end;

{TTurboRelocatedTypes}
function TTurboRelocatedTypes.Resolve(const I: Integer): Integer;
begin
  Result := I;
  if Result < 0 then
  begin
    Result := -Result;
    Assert(Result < GRegisteredTypes.Count, 'vMethodInfoEntry.TurboType index is too big for the GRegisteredTypes.');
    Result := Integer(GRegisteredTypes.Items[Result]);
  end
  else if Result > 0 then
  begin
    Dec(Result);
    Assert(Result < Module.RegisteredTypes.Count, 'vMethodInfoEntry.TurboType index is too big for the RegisteredTypes.');
    Result := Integer(Module.RegisteredTypes.Items[Result]);
  end;
end;

function TTurboRelocatedTypes.UnResolve(const I: Integer): Integer;
begin
  Result := I;
  if Result <> 0 then
  begin
    Result := Module.RegisteredTypes.IndexOf(PMeType(Result));
    if Result < 0 then
    begin
      Result := GRegisteredTypes.IndexOf(PMeType(Result));
      Assert(Result >= 0, 'vMethodInfoEntry.TurboType Not found in the GRegisteredTypes.');
      Result := -Result;
    end
    else
      Inc(Result);
  end;
end;

procedure TTurboRelocatedTypes.Relocate(BaseAddress: Pointer;
  const aRelocated: Boolean);
var
  i: Integer;
  //v: PInteger;
begin
  for I := 0 to Count -1 do
  begin
    if aRelocated then
    begin
      Integer(List^[I]) := Integer(List^[I]) + Integer(BaseAddress);
      if PInteger(List^[I])^ <> 0 then
        PInteger(List^[I])^ := Resolve(PInteger(List^[I])^); 
    end
    else
    begin
      if PInteger(List^[I])^ <> 0 then
        PInteger(List^[I])^ := UnResolve(PInteger(List^[I])^); 
      Integer(List^[I]) := Integer(List^[I]) - Integer(BaseAddress);
    end;
  end;
end;

initialization
finalization
end.
