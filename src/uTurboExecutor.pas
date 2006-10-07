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

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uMeTypes
  , uTurboScriptConsts
  ;

type
  TCustomTurboPEFormat = class;
  TCustomTurboModule = class;
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

  {: the abstract turbo script Module. }
  { Description
  û��ִ�л���/

  Load the script into memory
  }
  TCustomTurboModule = class(TCustomTurboObject)
  private
    FAccessor: TObject;
    FIsLoaded: Boolean;
    function GetLastErrorCode: TTurboForthProcessorErrorCode;
    function GetModuleType: TTurboModuleType;
    function GetStatus: TTurboProcessorStates;
    function GetTIB: string;
    procedure SetMemorySize(Value: Integer);
    procedure SetModuleType(Value: TTurboModuleType);
    procedure SetStatus(Value: TTurboProcessorStates);
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
    {: Finalize after the execution. }
    procedure FinalizeExecution; virtual;
    procedure Grow;
    {: : Run the CFA word. }
    { Description
    internal proc, not init.

    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
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
    function RequireModule(const aModuleName: ShortString): TCustomTurboModule;
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
    {: the TurboModuleAccessor }
    property Accessor: TObject read FAccessor write FAccessor;
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
    property Status: TTurboProcessorStates read GetStatus write SetStatus;
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
  end;

  TTurboProgram = class(TObject)
  private
    FOptions: TTurboScriptOptions;
    FParameterStack: Pointer;
    FParameterStackSize: Integer;
    FReturnStack: Pointer;
    FReturnStackSize: Integer;
    FStatus: TTurboForthProcessorStates;
    procedure SetExecutor(const Value: TCustomTurboModule);
    procedure SetParameterStackSize(const Value: Integer);
    procedure SetReturnStackSize(const Value: Integer);
  protected
    FExecutor: TCustomTurboModule;
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
    property Status: TTurboForthProcessorStates read FStatus write FStatus;
  end;


  PPreservedCodeMemory = ^ TPreservedCodeMemory;
  PTurboVariableEntry = ^ TTurboVariableEntry;
  PTurboModuleEntry = ^ TTurboModuleEntry;
  PTurboWordEntry = ^ TTurboWordEntry;
  PTurboTypeInfoEntry = ^ TTurboTypeInfoEntry;

  //For type-cast the Mem
  TTurboWordEntry = packed record
    Prior: PTurboWordEntry; //ǰһ������ 0 means Ϊ��ǰ�档

    Options: TTurboWordOptions;
    //the Param Field Length
    //�ú�������ĳ��� 
    ParamFieldLength: LongWord;
    Name: ShortString; //packed
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //��ʵ����ֱ��ָ���ĳ�����ʵ�PFA�������Ǹ����ʵ�PFA����ֱ��ִ�еĻ�������ѡ�
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //������������PForthWord��������������������ݻ�VM Codes
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
    Prior: PTurboSymbolEntry; //nil means no more
    //## abondoned following fields are TypeInfo: PMeType
    //## MeType: Pointer; //PMeType(@TTurboSymbolEntry.MeType) 
    TypeKind: TMeTypeKind;
    Name: ShortString; //packed ; maybe nil.
    //other additional fields}
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
    ToIn: Integer; //>IN the text buffer current index
    TIBLength: Integer; //#TIB the text buffer length
    TIB: array [0..cMAXTIBCount-1] of char; //'TIB
    LastErrorCode: TTurboProcessorErrorCode;
    //���ModuleType��ģ�飬��ô����װ�����и�ģ��ǰִ�еĳ�ʼ�����̣���ڵ�ַ
    //����Ǻ��������Ǹú�������ڵ�ַ
    InitializeProc: Pointer; 
    FinalizeProc: Pointer; //�����ģ��Ļ�
    //last Used(import) module entry.
    LastModuleEntry: PTurboModuleEntry;
    //�����ֵĺ�������ָ�����һ��������ڡ�
    LastWordEntry: PTurboWordEntry;
    //�����ֵı�������
    LastVariableEntry: PTurboVariableEntry;
    //RTTI TypeInfo ����
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
  uTurboScriptAccessor;

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
  FMemorySize := 0;
  inherited Destroy;
end;

procedure TCustomTurboModule.AddIntToMem(const aInt: Integer);
var
  p: Pointer;
begin
  if FUsedMemory >= FMemorySize then
    Grow;
  Integer(p) := Integer(FMemory) + FUsedMemory;
  PInteger(P)^ := aInt;
  Inc(FUsedMemory, SizeOf(Integer));
end;

procedure TCustomTurboModule.ClearMemory;
begin
  MemorySize := SizeOf(TPreservedCodeMemory); //+ cDefaultFreeMemSize;
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

function TCustomTurboModule.ExecuteCFA(const aCFA: Integer): Integer;
begin
  InitExecution;
  Result := iExecuteCFA(aCFA);
  FinalizeExecution;
end;

function TCustomTurboModule.ExecuteWord(const aWord: string): Integer;
var
  aCFA: Integer;
begin
  aCFA := GetWordCFA(aWord);
  if aCFA >=0 then
    Result := ExecuteCFA(aCFA)
  else
    Result := -1;
end;

procedure TCustomTurboModule.FinalizeExecution;
begin
  //Apply the SP to TProgram.SP.
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

function TCustomTurboModule.GetLastErrorCode: TTurboForthProcessorErrorCode;
begin
  Result := PPreservedCodeMemory(FMemory).LastErrorCode;
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

function TCustomTurboModule.GetWordCFA(const aWord: string): Integer;
begin
  Result := -1;
end;

procedure TCustomTurboModule.Grow;
begin
  MemorySize := FMemorySize + FMemorySize div 4;
end;

function TCustomTurboModule.iExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := -1;
  //if (psRunning in Status) then
    //raise ETurboScriptError.CreateRes(@rsTurboScriptAlreayRunningError);
end;

procedure TCustomTurboModule.InitExecution;
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

procedure TCustomTurboModule.Load;
begin
  if not IsLoaded and Assigned(FAccessor) then
  begin
    TTurboModuleAccessor(FAccessor).LoadModule(Self);
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

  MemorySize := Count;
  aStream.ReadBuffer(FMemory^, Count);
  Reset;
  TurboConvertAddrRelatedToAbsolute(FMemory);

  FIsLoaded := True;
end;

procedure TCustomTurboModule.NotifyModuleFree(Sender: TObject);
var
  I: Integer;
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
  aStream.WriteBuffer(FMemory^, FUsedMemory);
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

procedure TCustomTurboModule.SetMemorySize(Value: Integer);
begin
  if FMemorySize <> Value then
  begin
    FMemorySize := Value;
    ReallocMem(FMemory, FMemorySize);
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

procedure TCustomTurboModule.Stop;
begin
  Exclude(PPreservedCodeMemory(FMemory).States, psRunning);
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
  if Assigned(aEntry) then
    while aEntry.Prior <> nil do
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
  MOV  EDX, [ECX].TTurboEntry.Prior
  JMP  @@Loop
@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TurboConvertVarEntryRelatedToAbsolute(Mem: Pointer; aEntry: PTurboVariableEntry);
{$IFDEF PUREPASCAL}
begin
  if Assigned(aEntry) then
    while aEntry.Prior <> nil do
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

  //TODO: 
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastTypeInfoEntry);  
  TurboConvertVarEntryRelatedToAbsolute(Mem, Mem.LastVariableEntry);  
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastWordEntry);  
  TurboConvertEntryRelatedToAbsolute(Mem, Mem.LastModuleEntry);  
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
  if Assigned(aEntry) then
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
  if Assigned(aEntry) then
    while aEntry.Prior <> nil do
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
  if Assigned(Mem.InitializeProc) then
    Dec(Integer(Mem.InitializeProc), Integer(Mem));
  if Assigned(Mem.FinalizeProc) then
    Dec(Integer(Mem.FinalizeProc), Integer(Mem));
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastWordEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastModuleEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastVariableEntry);  
  TurboConvertEntryAbsoluteToRelated(Mem, Mem.LastTypeInfoEntry);  
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

  LEA  EDX, [EAX].TPreservedCodeMemory.LastTypeInfoEntry
  CALL TurboConvertEntryAbsoluteToRelated 
end;
{$ENDIF PUREPASCAL}

end.
