{: The abstract super script executor module. }
{ Description

��ס����Ķ�ջ(����X86�Ķ�ջ����)�� ѹ�����ǵ�ַ���٣��������ǵ�ַ����,
��ջ�����ڶ��������������ġ�
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
  }
  TCustomTurboModule = class(TCustomTurboObject)
  private
    FDataMemory: Pointer;
    function GetGlobalOptions: PTurboGlobalOptions;
    function GetLastModuleEntry: PTurboModuleEntry;
    procedure SetGlobalOptions(Value: PTurboGlobalOptions);
    procedure SetLastModuleEntry(Value: PTurboModuleEntry);
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
    function GetDataMemorySize: Integer;
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
    function GetUsedDataSize: Integer;
    function GetUsedMemory: Integer;
    procedure Grow(const aSize: Integer = 0);
    procedure GrowData(const aSize: Integer = 0);
    procedure LoadUsedModules;
    procedure SendUnloadNotification;
    procedure SetDataMemorySize(Value: Integer);
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
    procedure SetUsedDataSize(Value: Integer);
    procedure SetUsedMemory(Value: Integer);
  public
    constructor Create(const aParent: TCustomTurboModule = nil; aVisibility:
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
    function FindModuleEntry(const aName: string): PTurboModuleEntry;
    {: nil means not found. }
    function FindTypeInfoEntry(const aName: string): PTurboTypeInfoEntry;
    function FindUnloadNotification(aProc: TNotifyEvent): Integer;
    {: nil means not found. }
    function FindVariableEntry(const aName: string): PTurboVariableEntry;
    {: nil means not found. }
    function FindWordEntry(const aName: string; const aCallStyle:
            TTurboCallStyle = csForth): PTurboWordEntry;
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
    function RequireModule(const aModuleName: ShortString): TCustomTurboModule;
    {: reset the stack pointers. }
    procedure Reset;
    procedure SaveToFile(const aFileName: String);
    {: save FMemory to stream }
    procedure SaveToStream(const aStream: TStream);
    {: �Ƿ񱻴����Parent ��. }
    function StoredInParent: Boolean;
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
    property DataMemorySize: Integer read GetDataMemorySize write
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
    property LastModuleEntry: PTurboModuleEntry read GetLastModuleEntry write
            SetLastModuleEntry;
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
    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ParameterStack: Pointer read GetParameterStack write
            SetParameterStack;
    {: : the Parameter Stack }
    { Description
    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ParameterStackSize: Integer read GetParameterStackSize write
            SetParameterStackSize;
    {: ���ĸ���:�� Parser ��Treeʹ��. nil means root. }
    { Description
    �����ģ����˽�е�,��ô�ڴ����ʹ�õĸ��׵��ڴ�.
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
    ���ض�ջ

    ָ��ջ�ף� @Stack[0]
    ѹ���С
    }
    property ReturnStack: Pointer read GetReturnStack write SetReturnStack;
    {: : Return(Proc) Stack }
    { Description
    ���ض�ջ

    ָ��ջ�ף� @Stack[0]
    ѹ���С
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
    {: �Ѿ�ʹ�õ��ڴ� }
    { Description
    Ҳ����ָ�����Ŀ����ڴ棺
    �Ӹõ�ַ����ڴ�δ�ã�FMemory[UsedMemory] 
    }
    property UsedDataSize: Integer read GetUsedDataSize write SetUsedDataSize;
    {: �Ѿ�ʹ�õ��ڴ� }
    { Description
    Ҳ����ָ�����Ŀ����ڴ棺
    �Ӹõ�ַ����ڴ�δ�ã�FMemory[UsedMemory] 
    }
    property UsedMemory: Integer read GetUsedMemory write SetUsedMemory;
    property Visibility: TTurboVisibility read FVisibility write FVisibility;
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
    �����FMemory��ƫ������
    }
    function iExecuteCFA(const aCFA: Integer): Integer; virtual;
    {: : Run the Virtual Machine. }
    { Description
    Run the Virtual Machine from the PC adress.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
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
    �����FMemory��ƫ������
    }
    function iExecuteCFA(const aCFA: Integer): Integer; override;
    {: Init before the Execution. }
    procedure InitExecution; override;
    property Executor: TCustomTurboExecutor read GetExecutor;
  public
    { Description
    Ϊ���ڶ�� executor �й���������ջ������ջ�Լ����в�����
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
  TTurboWordEntry = object
    Prior: PTurboWordEntry; //ǰһ������ 0 means Ϊ��ǰ�档

    Options: TTurboWordOptions;
    //the Param Field Length
    //�ú�������ĳ��� 
    ParamFieldLength: LongWord;
    CFA: tsInt;//the offset address of the memory.
    Name: ShortString; //packed
    {if CallStyle <> csForth then //external procedure
      //PFA: TTurboExteralWordPFA
      //ForStack ��IntegerΪ��λ�ģ��������������ϵĲ�������
      //��Щ������������ջ����
      ParamCount: Integer; 
      TypeInfo: PTuroboTypeInfoEntry;  //nil means no RTTI info. the address is related.
    }
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //��ʵ����ֱ��ָ���ĳ�����ʵ�PFA�������Ǹ����ʵ�PFA����ֱ��ִ�еĻ�������ѡ�
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //������������PForthWord��������������������ݻ�VM Codes
  end;
  //cfsHostFunction, cfsDLLFunction
  TTurboExteralWordCFA = packed record
    ProcAddr: Pointer; //if it csForth then it is the CFA.
    ModuleEntry: PTurboModuleEntry;
    ProcTypeEntry: Pointer; //if it csForth then it is Module instance else it is ProcTypeInfo offset addr.
    //-1 means non-index visits.
    Index: tsInt;  
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
    Revision: LongWord; //the file version
    BuildDate: TTimeStamp;
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


  TTurboGlobalOptions = record  //do not use the packed.
    States: TTurboProcessorStates; //�����������ٶȻ��½�
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
    Data: Pointer; //point to the data memory. Or its the offset of the data in File .
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
    UsedMemory: tsInt;//ʵ��ʹ�õĴ�С
    MemorySize: tsInt;//����������Ĵ�С
    UsedDataSize: tsInt;
    DataSize: tsInt; 
    //ToIn: Integer; //>IN the text buffer current index
    //TIBLength: Integer; //#TIB the text buffer length
    //TIB: array [0..cMAXTIBCount-1] of char; //'TIB
    //LastErrorCode: TTurboProcessorErrorCode;
    //���ModuleType��ģ�飬��ô����װ�����и�ģ��ǰִ�еĳ�ʼ�����̣���ڵ�ַ
    //����Ǻ��������Ǹú�������ڵ�ַ
    InitializeProc: Pointer; //it is the offset address of the FMemory
    FinalizeProc: Pointer; //�����ģ��Ļ�
    //last Used(import) module entry.
    LastModuleEntry: PTurboModuleEntry;
    //�����ֵĺ�������ָ�����һ��������ڡ�
    LastWordEntry: PTurboWordEntry;
    //�����ֵı�������
    LastVariableEntry: PTurboVariableEntry;
    //RTTI TypeInfo ����
    LastTypeInfoEntry: PTurboTypeInfoEntry;
    //reserved: array [SizeOf() ] of byte; 
  end;
  
  TTurboModuleStreamHeader = packed record
    Id: array [0..cFORTHHeaderMagicIdLen-1] of char;
    Version: LongWord; //the file format version
    Revision: LongWord; //the file version
    BuildDate: TTimeStamp;
  end;
  
procedure TurboConvertAddrRelatedToAbsolute(Mem: PPreservedCodeMemory; Data: Pointer);
procedure TurboConvertAddrAbsoluteToRelated(Mem: PPreservedCodeMemory; Data: Pointer);
//remove registered types of this module
procedure RemoveModuleTypes(const aModuleName: string);

var 
  SimpleTurboTypes: TTurboSimpleTypes;


implementation

uses
  uTurboAccessor;

const
  cCurrentModuleFileForamtVersionNo = 1; 
  
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
  //FOptions := [soLoadOnDemand];
  ClearMemory;
end;

destructor TCustomTurboModule.Destroy;
begin
  Unload;

  if not StoredInParent then
  begin
    FreeMem(FMemory);
    FMemory := nil;
    FreeMem(FDataMemory);
    FDataMemory := nil;
  end;
  inherited Destroy;
end;

procedure TCustomTurboModule.AddBufferToData(const aValue; aSize: Integer);
var
  p: Pointer;
begin
  if (PPreservedCodeMemory(FMemory).UsedDataSize+aSize) > DataMemorySize then
    GrowData(aSize);
  Integer(p) := Integer(FDataMemory) + PPreservedCodeMemory(FMemory).UsedDataSize;
  Move(aValue, p^, aSize);
  Inc(PPreservedCodeMemory(FMemory).UsedDataSize, aSize);
end;

procedure TCustomTurboModule.AddBufferToMem(const aValue; aSize: Integer);
var
  p: Pointer;
begin
  if (PPreservedCodeMemory(FMemory).UsedMemory+aSize) > MemorySize then
    Grow(aSize);
  Integer(p) := Integer(FMemory) + PPreservedCodeMemory(FMemory).UsedMemory;
  Move(aValue, p^, aSize);
  Inc(PPreservedCodeMemory(FMemory).UsedMemory, aSize);
end;

procedure TCustomTurboModule.AddByteToData(const aValue: Byte);
var
  p: Pointer;
begin
  with PPreservedCodeMemory(FMemory)^ do
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
  if PPreservedCodeMemory(FMemory).UsedMemory >= MemorySize then
    Grow;
  Integer(p) := Integer(FMemory) + PPreservedCodeMemory(FMemory).UsedMemory;
  PByte(P)^ := aValue;
  Inc(PPreservedCodeMemory(FMemory).UsedMemory, SizeOf(Byte));
end;

procedure TCustomTurboModule.AddIntToData(const aValue: Integer);
var
  p: Pointer;
begin
  with PPreservedCodeMemory(FMemory)^ do
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
  {
  PInteger(P)^ := Integer(aOpCode);
  Inc(PPreservedCodeMemory(FMemory).UsedMemory, SizeOf(Integer));
  }
  PTurboVMInstruction(P)^ := aOpCode;
  Inc(PPreservedCodeMemory(FMemory).UsedMemory, SizeOf(TTurboVMInstruction));
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
  if (PPreservedCodeMemory(FMemory).UsedDataSize + aSize) >= DataMemorySize then
    GrowData(aSize);

  Inc(PPreservedCodeMemory(FMemory).UsedDataSize, aSize);
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
    ReallocMem(FDataMemory, cDefaultDataMemSize);
    with PPreservedCodeMemory(FMemory)^ do
    begin
      MemorySize := vPreserved;
      UsedMemory := vPreserved;
      Data := FDataMemory;
      DataSize := cDefaultDataMemSize;
      UsedDataSize := SizeOf(tsInt); //preserved the first integer
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
      //States := [];
    end;

    IsLoaded := False;
  end;
end;

function TCustomTurboModule.FindModuleEntry(const aName: string):
        PTurboModuleEntry;
begin
  Result := LastModuleEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    if Result.ModuleName = aName then
    begin
      Exit;
    end
    else
      Result := Result.Prior;
  end;
  Result := nil;
end;

function TCustomTurboModule.FindTypeInfoEntry(const aName: string):
        PTurboTypeInfoEntry;
begin
  Result := LastTypeInfoEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
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
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
    if Result.Name = aName then
    begin
      Exit;
    end
    else
      Result := Result.Prior;
  end;
  Result := nil;
end;

function TCustomTurboModule.FindWordEntry(const aName: string; const
        aCallStyle: TTurboCallStyle = csForth): PTurboWordEntry;
begin
  Result := LastWordEntry;
  while (Result <> nil) do
  begin
    if psCompiling in Status then
    begin
      //writeln('psCompiling,', Integer(Result));
      Integer(Result) := Integer(FDataMemory) + Integer(Result);
      //break;
    end;
    {if Result.Name <> '' then
    begin
      writeln('FindWordEntry:', Result.Name);
      writeln('FindWordEntry:', Integer(Result.Options.CallStyle));
      writeln('aCallStyle:', Integer(aCallStyle));
    end;//}
    if (Result.Options.CallStyle = aCallStyle) and AnsiSameText(Result.Name, aName) then
    begin
      Exit;
    end
    else
      Result := Result.Prior;
  end;
  Result := nil;
end;

function TCustomTurboModule.GetDataMemorySize: Integer;
begin
  Result := PPreservedCodeMemory(FMemory).DataSize;
end;

function TCustomTurboModule.GetGlobalOptions: PTurboGlobalOptions;
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

function TCustomTurboModule.GetLastModuleEntry: PTurboModuleEntry;
begin
  Result := PPreservedCodeMemory(FMemory).LastModuleEntry;
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
  with PPreservedCodeMemory(FMemory)^ do
  begin
    if Assigned(GlobalOptions) then
      Result := GlobalOptions.States
    else
      Result := [];
  end;
end;

function TCustomTurboModule.GetUsedDataSize: Integer;
begin
  Result := PPreservedCodeMemory(FMemory).UsedDataSize;
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

  if Count < (SizeOf(TPreservedCodeMemory)+ SizeOf(TTurboModuleStreamHeader)) then
    raise ETurboScriptError.CreateRes(@rsInvalidTurboScriptStreamError);

  aStream.ReadBuffer(vHeader, SizeOf(TTurboModuleStreamHeader));

  if vHeader.Id <> cFORTHHeaderMagicId then
    raise ETurboScriptError.CreateRes(@rsInvalidTurboScriptStreamError);
  ModuleVersion := vHeader.Revision;
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

  UsedMemory := SizeOf(TPreservedCodeMemory);
  aStream.ReadBuffer(FMemory^, SizeOf(TPreservedCodeMemory));
  ReallocMem(FMemory, MemorySize);
  Integer(p) := Integer(FMemory) + SizeOf(TPreservedCodeMemory);
  aStream.ReadBuffer(p^, UsedMemory-SizeOf(TPreservedCodeMemory));

  ReallocMem(FDataMemory, DataMemorySize);
  //writeln('Load.UsedDataOff=',aStream.Position);
  aStream.ReadBuffer(FDataMemory^, UsedDataSize);
  //MemorySize := Count-SizeOf(TTurboModuleStreamHeader);
  //aStream.ReadBuffer(FMemory^, MemorySize);

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
  vModuleEntry: PTurboModuleEntry;
  vModule: TCustomTurboModule;
begin
  vModuleEntry := LastModuleEntry;
  while (vModuleEntry <> nil) do
  begin
    if psCompiling in Status then
      Integer(vModuleEntry) := Integer(FDataMemory) + Integer(vModuleEntry);
    vModule := nil;
    case vModuleEntry.ModuleType of
      mtLib: vModule := RequireModule(vModuleEntry.ModuleName);
    end; //case
    if Assigned(vModule) then
      vModuleEntry.Module := vModule;
    vModuleEntry := vModuleEntry.Prior;
  end;
end;

procedure TCustomTurboModule.NotifyModuleFree(Sender: TObject);
begin
  RemoveUnloadNotification(TCustomTurboModule(Sender).NotifyModuleUnloaded);
end;

procedure TCustomTurboModule.NotifyModuleUnloaded(Sender: TObject);
var
  vModule: PTurboModuleEntry;
begin
  if (Sender = FParent) and StoredInParent then
  begin
    FIsLoaded := False;
  end;

  vModule := LastModuleEntry;
  while (vModule <> nil) do
  begin
    if psCompiling in Status then
      Integer(vModule) := Integer(FMemory) + Integer(vModule);
    if vModule.Module = Sender then
    begin
      vModule.Module := nil;
      Exit;
    end
    else
      vModule := vModule.Prior;
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

  with PPreservedCodeMemory(FMemory)^ do
  begin
    SP := Integer(GlobalOptions.ParamStackBase) + GlobalOptions.ParamStackSize;
    RP := Integer(GlobalOptions.ReturnStackBase) + GlobalOptions.ReturnStackSize;
    GlobalOptions.LastErrorCode := errNone;
    Data := FDataMemory;
    Executor := Self;
    //GlobalOptions.States := []; //������Ķ����ͱ𶯣���
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
  vHeader.Revision := ModuleVersion;
  vHeader.Version  := cCurrentModuleFileForamtVersionNo;
  vHeader.BuildDate := ModuleDate;
  aStream.WriteBuffer(vHeader, SizeOf(TTurboModuleStreamHeader));
  if not (psCompiling in Status) then TurboConvertAddrAbsoluteToRelated(FMemory, FDataMemory);

  with PPreservedCodeMemory(FMemory)^ do
  begin
    //backup the data in the Memory
    vOptions := GlobalOptions;
    GlobalOptions := nil;
    Data := nil;
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
  //writeln('Save.UsedDataOff=',aStream.Position);
  if UsedDataSize > 0 then
    aStream.WriteBuffer(FDataMemory^, UsedDataSize);
  with PPreservedCodeMemory(FMemory)^ do
  begin
    GlobalOptions := vOptions;
    Data := FDataMemory;
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

procedure TCustomTurboModule.SetDataMemorySize(Value: Integer);
var
  vOld: Pointer;
begin
  if PPreservedCodeMemory(FMemory).DataSize <> Value then
  begin
    PPreservedCodeMemory(FMemory).DataSize := Value;
    vOld := FDataMemory;
    ReallocMem(FDataMemory, Value);
    if Integer(vOld) <> Integer(FDataMemory) then
      PPreservedCodeMemory(FMemory).Data := FDataMemory;
  end;
end;

procedure TCustomTurboModule.SetGlobalOptions(Value: PTurboGlobalOptions);
begin
  PPreservedCodeMemory(FMemory).GlobalOptions := Value;
end;

procedure TCustomTurboModule.SetLastModuleEntry(Value: PTurboModuleEntry);
begin
  PPreservedCodeMemory(FMemory).LastModuleEntry := Value;
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
  with PPreservedCodeMemory(FMemory)^ do
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

procedure TCustomTurboModule.SetUsedDataSize(Value: Integer);
begin
  if Value < SizeOf(TPreservedCodeMemory) then
    Value  := SizeOf(TPreservedCodeMemory);
  if Value > DataMemorySize then
    GrowData(Value - DataMemorySize);
  PPreservedCodeMemory(FMemory).UsedDataSize := Value;
end;

procedure TCustomTurboModule.SetUsedMemory(Value: Integer);
begin
  if Value < SizeOf(TPreservedCodeMemory) then
    Value  := SizeOf(TPreservedCodeMemory);

  if Value > MemorySize then
    GrowData(Value - MemorySize);

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

  Include(PPreservedCodeMemory(FMemory).GlobalOptions.States, psRunning);
end;

procedure TCustomTurboExecutor.Stop;
begin
  with PPreservedCodeMemory(FMemory)^ do
  begin
    if Assigned(GlobalOptions) then Exclude(GlobalOptions.States, psRunning);
  end;
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
  PPTurboEntry = ^ PTurboEntry;
  PTurboEntry  = ^ TTurboEntry;
  TTurboEntry  = packed record
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

procedure TurboConvertEntryRelatedToAbsolute(Mem: Pointer; var aEntry: PTurboEntry);
begin
  if Assigned(aEntry) then
  begin
    Integer(aEntry) := Integer(aEntry) + Integer(Mem);
    while Assigned(aEntry) and (aEntry.Prior <> nil) do
    begin
      Integer(aEntry.Prior) := Integer(aEntry.Prior) + Integer(Mem);
      aEntry := aEntry.Prior;   
    end;
  end;
end;

procedure TurboConvertVarEntryRelatedToAbsolute(Mem: Pointer; var aEntry: PTurboVariableEntry);
begin
  if Assigned(aEntry) then
  begin
    Integer(aEntry) := Integer(aEntry) + Integer(Mem);
    while Assigned(aEntry) and (aEntry.Prior <> nil) do
    begin
      Integer(aEntry.Prior) := Integer(aEntry.Prior) + Integer(Mem);
      if Assigned(aEntry.TypeInfo) then
        Integer(aEntry.TypeInfo) := Integer(aEntry.TypeInfo) + Integer(Mem);  
      aEntry := aEntry.Prior;   
    end;
  end;
end;

procedure TurboConvertAddrRelatedToAbsolute(Mem: PPreservedCodeMemory; Data: Pointer);
begin
  {if Assigned(Mem.InitializeProc) then
    Inc(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Inc(Integer(Mem.FinalizeProc), Integer(Mem));
}
  //TODO: 
  TurboConvertEntryRelatedToAbsolute(Data, PTurboEntry(Mem.LastTypeInfoEntry));  
  TurboConvertVarEntryRelatedToAbsolute(Data, Mem.LastVariableEntry);  
  TurboConvertEntryRelatedToAbsolute(Data, PTurboEntry(Mem.LastWordEntry));  
  TurboConvertEntryRelatedToAbsolute(Data, PTurboEntry(Mem.LastModuleEntry));  
end;

procedure TurboConvertEntryAbsoluteToRelated(Mem: Pointer; var aEntry: PTurboEntry);
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

procedure TurboConvertVarEntryAbsoluteToRelated(Mem: Pointer; var aEntry: PTurboVariableEntry);
var
  P: PTurboVariableEntry;
begin
    while Assigned(aEntry) and (aEntry.Prior <> nil) do
    begin
      p := aEntry.Prior;
      Integer(aEntry.Prior) := Integer(aEntry.Prior) - Integer(Mem);
      if Assigned(aEntry.TypeInfo) then
        Integer(aEntry.TypeInfo) := Integer(aEntry.TypeInfo) - Integer(Mem);
      //aEntry.Prior := t;
      aEntry := P.Prior;   
    end;
end;

procedure TurboConvertAddrAbsoluteToRelated(Mem: PPreservedCodeMemory; Data: Pointer);
begin
{
  if Assigned(Mem.InitializeProc) then
    Dec(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Dec(Integer(Mem.FinalizeProc), Integer(Mem));
}
  TurboConvertEntryAbsoluteToRelated(Data, PTurboEntry(Mem.LastWordEntry));  
  TurboConvertEntryAbsoluteToRelated(Data, PTurboEntry(Mem.LastModuleEntry));  
  TurboConvertVarEntryAbsoluteToRelated(Data, Mem.LastVariableEntry);  
  TurboConvertEntryAbsoluteToRelated(Data, PTurboEntry(Mem.LastTypeInfoEntry));  
end;

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
