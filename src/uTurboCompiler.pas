{: The script compiler module. }
{ Description
the abstract parser, compiler class, and the compiler factory here.
}
unit uTurboCompiler;

interface

uses
  SysUtils, Classes
  , uMeTypes
  , uTurboConsts
  , uTurboExecutor
  ;

type
  TCustomTurboWord = class;
  TTurboWord = class;
  TTurboVariableList = class;
  TTurboWordList = class;
  TTurboModuleList = class;
  TTTurboModule = class;
  TCustomTurboCompiler = class;
  TTurboTypeSymbol = class;
  TAbstractTurboWordSymbol = class;
  TTurboOpCodeSymbol = class;
  TCustomTurboWordSymbol = class;
  TTurboWordSymbol = class;
  TCustomTurboWord = class(TCustomTurboModule)
  private
    FSymbols: TList;
    FUsedModules: TList;
    FVariables: TList;
    FVisibility: TTurboVisibility;
    FWords: TTurboWordList;
  protected
    procedure DoCompile; virtual;
    class procedure InitModuleType; virtual;
  public
    constructor Create(const aParent: TCustomTurboModule = nil; aVisibility:
            TTurboVisibility = fvPublished); override;
    destructor Destroy; override;
    { Description
    ���ݱ��뿪�ؽ���Щ�б����������Memory.
    ���������ģ���AccessorΪDBAccessor,��ô��ֻ��ʹ�ú��ڰ󶨵���ʽ����.
    ���������ģ��ı��뿪��Ҫ�����ֱ�

    See Also
      Options
    }
    procedure Compile; virtual;
    property Symbols: TList read FSymbols write FSymbols;
    property UsedModules: TList read FUsedModules write FUsedModules;
    property Variables: TList read FVariables write FVariables;
    property Visibility: TTurboVisibility read FVisibility write FVisibility;
    {: ���е�WordsΪ˽��word.Ҳ���Ǹ�word��Ƕ�׹���. }
    property Words: TTurboWordList read FWords write FWords;
  end;

  TTurboWord = class(TCustomTurboWord)
  protected
    procedure DoCompile; override;
    class procedure InitModuleType; override;
  end;

  TTurboVariableList = class(TTurboSymbolList)
  private
    function GetItems(Index: Integer): TTurboVariableSymbol;
    function GetOwner: TCustomTurboWord;
  public
    function Add: TTurboVariableSymbol;
    property Items[Index: Integer]: TTurboVariableSymbol read GetItems; default;
    property Owner: TCustomTurboWord read GetOwner;
  end;

  TTurboWordList = class(TTurboSymbolList)
  private
    function GetItems(Index: Integer): TCustomTurboWord;
    function GetOwner: TCustomTurboWord;
  public
    function Add: TCustomTurboWord;
    property Items[Index: Integer]: TCustomTurboWord read GetItems; default;
    property Owner: TCustomTurboWord read GetOwner;
  end;

  TTurboModuleList = class(TTurboSymbolList)
  private
    function GetItems(Index: Integer): TTurboModule;
    function GetOwner: TTurboModule;
  public
    function Add: TTurboModule;
    property Items[Index: Integer]: TTurboModule read GetItems; default;
    property Owner: TTurboModule read GetOwner;
  end;

  { Description
  ��Options ֻ�� soBindingRunTime, ��ô���ķ�˽�� words Ϊ���������.
  ���ܱ����� memory.
  ��ĳ����˽�� TurboWord.Option ֻ�� soBindingRunTime, ��ô�� word Ϊ���������.
  ���ܱ����� memory.
  ע��˽�е� word ���ܶ�������!!
  }
  TTTurboModule = class(TCustomTurboWord)
  private
    FChilds: TTurboModuleList;
  protected
    procedure DoCompile; override;
    class procedure InitModuleType; override;
  public
    constructor Create(const aParent: TCustomTurboModule = nil; aVisibility:
            TTurboVisibility = fvPublished); override;
    destructor Destroy; override;
    {: ������ģ���б�:�� Parser ��Treeʹ�� }
    property Childs: TTurboModuleList read FChilds write FChilds;
  end;

  TCustomTurboCompiler = class(TCustomTurboObject)
  end;

  TTurboTypeSymbol = class(TCustomTurboSymbol)
  end;

  {: abstract word symbol. }
  { Description
  include OpCode, word, module symbol.
  }
  TAbstractTurboWordSymbol = class(TCustomTurboSymbol)
  end;

  {: the turbo script virtual machine code symbol. }
  TTurboOpCodeSymbol = class(TAbstractTurboWordSymbol)
  protected
    FOpCode: TTurboVMInstruction;
    FOperand1: Integer;
    FOperand2: Integer;
  public
    property OpCode: TTurboVMInstruction read FOpCode write FOpCode;
    property Operand1: Integer read FOperand1 write FOperand1;
    property Operand2: Integer read FOperand2 write FOperand2;
  end;

  {: the turbo script word(module) symbol. }
  TCustomTurboWordSymbol = class(TAbstractTurboWordSymbol)
  private
    FCode: TCustomTurboModule;
  public
    constructor Create;
    destructor Destroy; override;
    property Code: TCustomTurboModule read FCode;
  end;

  {: the turbo script virtual machine code symbol. }
  TTurboWordSymbol = class(TCustomTurboWordSymbol)
  private
    FBody: TTurboWordSymbolList;
  public
    constructor Create;
    destructor Destroy; override;
    property Body: TTurboWordSymbolList read FBody;
  end;

  {: keep the function body symbol. }
  TTurboWordSymbolList = class(TTurboSymbolList)
  private
    function GetItems(Index: Integer): TAbstractTurboWordSymbol;
    function GetOwner: TCustomTurboWordSymbol;
  public
    function Add(const aWord: TAbstractTurboWordSymbol): Integer;
    property Items[Index: Integer]: TAbstractTurboWordSymbol read GetItems;
            default;
    property Owner: TCustomTurboWordSymbol read GetOwner;
  end;


implementation

{
******************************* TCustomTurboWord *******************************
}
constructor TCustomTurboWord.Create(const aParent: TCustomTurboModule = nil;
        aVisibility: TTurboVisibility = fvPublished);
begin
  inherited Create(aParent, aVisibility);
  InitModuleType;
  FSymbols := TList.Create;
  FUsedModules := TList.Create;
  FVariables := TList.Create;
  FWords := TTurboWordList.Create(Self);
end;

destructor TCustomTurboWord.Destroy;
begin
  FreeAndNil(FWords);
  FreeAndNil(FVariables);
  FreeAndNil(FUsedModules);
  FreeAndNil(FSymbols);
  inherited Destroy;
end;

procedure TCustomTurboWord.Compile;
var
  vStoredInParent: Boolean;
begin
  //����Module ˽�� ��ô��Module ʵ�����ǲ����ڱ��ϲ���Parent
  vStoredInParent := StoredInParent;
  try
    if vStoredInParent then
    begin
      FreeMem(FMemory);
      FMemory := Parent.FMemory;
      //FMemorySize := Parent.FMemorySize;
      //FUsedMemory := Parent.FUsedMemory;
    end
    else
    begin
      ClearMemory;
    end;

    DoCompile;
  finally
    if vStoredInParent then
    begin
      FMemory := nil;
      ClearMemory;
    end;
  end;
end;

procedure TCustomTurboWord.DoCompile;
begin
  //Process UsedModules
  //process TypeInfos
  //process Variables
  //Process words
end;

class procedure TCustomTurboWord.InitModuleType;
begin
  FModuleType := mtUnknown;
end;

{
********************************** TTurboWord **********************************
}
procedure TTurboWord.DoCompile;
begin
  inherited DoCompile;
  //Process func-body.
end;

class procedure TTurboWord.InitModuleType;
begin
  FModuleType := mtFunction;
end;

{
****************************** TTurboVariableList ******************************
}
function TTurboVariableList.Add: TTurboVariableSymbol;
begin
  Result := TTurboModule.Create;
  try
    Result.Parent := TCustomTurboWord(FOwner);
    inherited Add(Result);
  except
    FreeAndNil(Result);
  end;
end;

function TTurboVariableList.GetItems(Index: Integer): TTurboVariableSymbol;
begin
  Result := TTurboModule(inherited Get(Index));
end;

function TTurboVariableList.GetOwner: TCustomTurboWord;
begin
  Result := TCustomTurboWord(FOwner);
end;

{
******************************** TTurboWordList ********************************
}
function TTurboWordList.Add: TCustomTurboWord;
begin
  Result := TCustomTurboWord.Create;
  try
    Result.Parent := TCustomTurboWord(FOwner);
    inherited Add(Result);
  except
    FreeAndNil(Result);
  end;
end;

function TTurboWordList.GetItems(Index: Integer): TCustomTurboWord;
begin
  Result := TCustomTurboWord(inherited Get(Index));
end;

function TTurboWordList.GetOwner: TCustomTurboWord;
begin
  Result := TCustomTurboWord(FOwner);
end;

{
******************************* TTurboModuleList *******************************
}
function TTurboModuleList.Add: TTurboModule;
begin
  Result := TTurboModule.Create;
  try
    Result.Parent := TCustomTurboWord(FOwner);
    inherited Add(Result);
  except
    FreeAndNil(Result);
  end;
end;

function TTurboModuleList.GetItems(Index: Integer): TTurboModule;
begin
  Result := TTurboModule(inherited Get(Index));
end;

function TTurboModuleList.GetOwner: TTurboModule;
begin
  Result := TTurboModule(FOwner);
end;

{
******************************** TTTurboModule *********************************
}
constructor TTTurboModule.Create(const aParent: TCustomTurboModule = nil;
        aVisibility: TTurboVisibility = fvPublished);
begin
  inherited Create(aParent, aVisibility);
  FChilds := TTurboModuleList.Create(Self);
end;

destructor TTTurboModule.Destroy;
begin
  FreeAndNil(FChilds);
  inherited Destroy;
end;

procedure TTTurboModule.DoCompile;
begin
  inherited DoCompile;
  //Do Process Childs.
end;

class procedure TTTurboModule.InitModuleType;
begin
  FModuleType := mtLib;
end;

{
**************************** TCustomTurboWordSymbol ****************************
}
constructor TCustomTurboWordSymbol.Create;
begin
  inherited Create;
  FCode := TCustomTurboModule.Create;
end;

destructor TCustomTurboWordSymbol.Destroy;
begin
  FreeAndNil(FCode);
  inherited Destroy;
end;

{
******************************* TTurboWordSymbol *******************************
}
constructor TTurboWordSymbol.Create;
begin
  inherited Create;
  FBody := TCustomTurboWordSymbol.Create(Self);
end;

destructor TTurboWordSymbol.Destroy;
begin
  FreeAndNil(FBody);
  inherited Destroy;
end;

{
***************************** TTurboWordSymbolList *****************************
}
function TTurboWordSymbolList.Add(const aWord: TAbstractTurboWordSymbol):
        Integer;
begin
  aWord.Parent := TCustomTurboWordSymbol(FOwner);
  Result :=  inherited Add(aWord);
end;

function TTurboWordSymbolList.GetItems(Index: Integer):
        TAbstractTurboWordSymbol;
begin
  Result := TAbstractTurboWordSymbol(inherited Get(Index));
end;

function TTurboWordSymbolList.GetOwner: TCustomTurboWordSymbol;
begin
  Result := TCustomTurboWordSymbol(FOwner);
end;


end.
