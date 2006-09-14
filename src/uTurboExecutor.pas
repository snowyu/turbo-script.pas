{1 The abstract super script executor module. }
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
  �ض�λ��ַ:
   1.Import���е�ģ��(DLL��ForthDLLģ��)�����ĵ�ַ
   2.Relocation�����Ե�ַ�����Ҫ���¼���ģ�
  }
  TCustomTurboPEFormat = class(TObject)
  private
    FImageBase: Integer;
  protected
    ExportTable: Integer;
    {1 ������ʹ�õ�ģ���еĺ��� }
    {{
    ����Import���е�ģ��(DLL��ForthDLLģ��)
    }
    ImportTable: Integer;
    {1 �ض�λ��ַ�� }
    {{
    �������ļ��еľ��Ի���ַ����ImageBase($0000)
    So �µľ��Ե�ַӦ����: OrgAddress - ImageBase + @CodeArea[0]
    }
    RelocationTable: Pointer;
  public
    {1 Ԥ��Ĵ������ַ }
    property ImageBase: Integer read FImageBase write FImageBase;
  end;
  
  {1 the abstract turbo script executor. }
  {{
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
    FMemory: Pointer;
    FModuleDate: TTimeStamp;
    FModuleType: TTurboScriptModuleType;
    FModuleVersion: LongWord;
    FName: string;
    FOptions: TTurboScriptOptions;
    FParameterStack: Pointer;
    FProgram: TTurboProgram;
    FStack: Pointer;
    FStatus: TTurboScriptStatus;
    procedure SetMemorySize(Value: Integer);
  protected
    FMemorySize: Integer;
    FUsedMemory: Integer;
    {1 : Run the CFA word. }
    {{
    internal proc, not init.
    
    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function ExecuteCFA(const aCFA: Integer): Integer; virtual;
    {1 Init the Virtual Machine. }
    procedure Init; virtual;
  public
    destructor Destroy; override;
    {1 : Run the Virtual Machine. }
    {{
    Run the Virtual Machine from the PC adress.
    
    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function ExecuteWord(const aWord: string): Integer;
    function GetWordCFA(const aWord: string): Integer; virtual;
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
    property ParameterStack: Pointer read FParameterStack write FParameterStack;
    property Program: TTurboProgram read FProgram write FProgram;
    {1 : Return(Proc) Stack }
    {{
    ���ض�ջ
    }
    property Stack: Pointer read FStack write FStack;
    {1 the current status of the script. }
    property Status: TTurboScriptStatus read FStatus write FStatus;
    {1 �Ѿ�ʹ�õ��ڴ� }
    {{
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
    {1 return stack. }
    property Stack: Pointer read FStack write FStack;
    property StackSize: Integer read FStackSize write SetStackSize;
    {1 the current status of the script. }
    property Status: TTurboScriptStatus read FStatus write FStatus;
  end;
  

implementation

{
***************************** TCustomTurboExecutor *****************************
}
destructor TCustomTurboExecutor.Destroy;
begin
  FreeMem(FMemory);
  FMemory := nil;
  inherited Destroy;
end;

function TCustomTurboExecutor.ExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := -1;
end;

function TCustomTurboExecutor.ExecuteWord(const aWord: string): Integer;
var
  aCFA: Integer;
begin
  Init;
  aCFA := GetWordCFA(aWord);
  if aCFA >=0 then
    Result := ExecuteCFA(aCFA)
  else
    Result := -1;
end;

function TCustomTurboExecutor.GetWordCFA(const aWord: string): Integer;
begin
  Result := -1;
end;

procedure TCustomTurboExecutor.Init;
begin
  //PC := 0;
  //SP := StackSize;
end;

procedure TCustomTurboExecutor.SetMemorySize(Value: Integer);
begin
  if FMemorySize <> Value then
  begin
    FMemorySize := Value;
    ReallocMem(FMemory, FMemorySize);
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
    ReallocMem(FParameterStack, FParameterStackSize);
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboProgram.SetStackSize(const Value: Integer);
begin
  if (Status < ssRunning) and (FStackSize <> Value) then
  begin
    FStackSize := Value;
    ReallocMem(FStack, FStackSize);
    //if FSP > FParameterStackSize then FSP := FParameterStackSize;
  end;
end;

procedure TTurboProgram.Stop;
begin
  //sF
end;


end.
