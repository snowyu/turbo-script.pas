{1 The abstract super script executor module. }
unit uTurboExecutor;

interface

uses
  SysUtils, Classes
  , uTurboScriptConsts
  ;

type
  TTurboScriptOption = (soLoadOnDemand, soBindingRuntime);
  TTurboScriptOptions = set of TTurboScriptOption;
  {: The current status of the turbo script }
  {
    @PARAM ssNotLoaded only the name and soem options loaded but the body(Memory) in not loaded yet.
  }
  TTurboScriptStatus = (ssNotLoaded, ssLoaded, ssRunning, ssPaused);

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
    FFileDate: LongWord;
    FFileVersion: LongWord;
    FModuleType: TTurboScriptModuleType;
    FName: string;
    FOptions: TTurboScriptOptions;
    FStatus: TTurboScriptStatus;
  protected
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
    {1 : Run the Virtual Machine. }
    {{
    Run the Virtual Machine from the PC adress.
    
    @param aCFA the Code Field Address(related to FMemory).
    �����FMemory��ƫ������
    }
    function ExecuteWord(const aWord: string): Integer;
    function GetWordCFA(const aWord: string): Integer; virtual;
    {1 : Load the Virtual Machine Codes from the File. }
    procedure LoadFromFile(const aFileName: String);
    {1 : Load the Virtual Machine Codes from the Stream. }
    procedure LoadFromStream(const aStream: TStream); virtual;
    {1 : save the Virtual Machine Codes to file. }
    procedure SaveToFile(const aFileName: String);
    {1 : save the Virtual Machine Codes to Stream. }
    procedure SaveToStream(const aStream: TStream); virtual;
    {{
    (The Data field indicates the number of calendar days since the start of
    the calendar (the number of days since 1/1/0001 plus one).)
    See Also TTimeStamp
    }
    property FileDate: LongWord read FFileDate write FFileDate;
    property FileVersion: LongWord read FFileVersion write FFileVersion;
    property ModuleType: TTurboScriptModuleType read FModuleType write
            FModuleType;
    property Name: string read FName write FName;
    property Options: TTurboScriptOptions read FOptions write FOptions;
    {1 the current status of the script. }
    property Status: TTurboScriptStatus read FStatus write FStatus;
  end;
  

implementation

{
***************************** TCustomTurboExecutor *****************************
}
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

procedure TCustomTurboExecutor.LoadFromFile(const aFileName: String);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

procedure TCustomTurboExecutor.LoadFromStream(const aStream: TStream);
var
  Lstr: string;
  L: Byte;
begin
  L := Length(cFORTHHeaderMagicWord);
  SetLength(Lstr, L);
  with aStream do
  begin
    Read(Lstr[1], L);
    if Lstr <> cFORTHHeaderMagicWord then
      Raise ESuperScriptError.Create(rsMissFileHeaderError);
  
    //Get the Unit(Program) Name.
    Read(L, SizeOf(L));
    SetLength(FName, L);
    if L<>0 then
      Read(FName[1], L);
  
    //Read(FModuleType, SizeOf(FModuleType)); //abondon
    Read(FFileVersion, SizeOf(FFileVersion));
    Read(FFileDate, SizeOf(FFileDate));
  end;
end;

procedure TCustomTurboExecutor.SaveToFile(const aFileName: String);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(aFileStream);
  finally
    aFileStream.Free;
  end;
end;

procedure TCustomTurboExecutor.SaveToStream(const aStream: TStream);
var
  L: Byte;
begin
  aStream.Write(cFORTHHeaderMagicWord[1], Length(cFORTHHeaderMagicWord));
  //Get the Unit(Program) Name.
  L := Length(FName);
  aStream.Write(L, SizeOf(L));
  if L<>0 then
    aStream.Write(PChar(FName)^, L);
  
  //aStream.Write(FModuleType, SizeOf(FModuleType));
  aStream.Write(FFileVersion, SizeOf(FFileVersion));
  aStream.Write(FFileDate, SizeOf(FFileDate));
end;


end.
