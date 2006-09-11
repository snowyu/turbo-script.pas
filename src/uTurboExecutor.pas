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
    相对于FMemory的偏移量。
    }
    function ExecuteCFA(const aCFA: Integer): Integer; virtual;
    {1 Init the Virtual Machine. }
    procedure Init; virtual;
  public
    {1 : Run the Virtual Machine. }
    {{
    Run the Virtual Machine from the PC adress.
    
    @param aCFA the Code Field Address(related to FMemory).
    相对于FMemory的偏移量。
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
