{: the module variable. }
unit uTurboSymbols;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboExecutor 
  ;

type
  TTurboSymbol = class;
  TTurboTypeSymbol = class;
  TTurboModuleSymbol = class;
  TTurboSymbolList = class;
  TTurboConstSymbol = class;
  TTurboVariableSymbol = class;
  TTurboConstSymbolList = class;
  TTurboWordSymbol = class;
  TTurboVariableSymbolList = class;
  {: the abstract symbol class. }
  TTurboSymbol = class(TObject)
  private
    FCaption: string;
    FDescription: string;
    FName: string;
    FParent: TObject;
    FRefs: LongInt;
  protected
    {: the abstract metainfo object. }
    FMetaInfo: PTurboMetaInfo;
    {: :Compile the index symbol content to memory. }
    { Description
    for compile, maybe i should rename to CompileTo?
    }
    procedure iCompileTo(const aModule: TCustomTurboModule); virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {: :Compile the index symbol content to memory. }
    { Description
    for compile, maybe i should rename to CompileTo?
    }
    procedure CompileTo(const aModule: TCustomTurboModule);
    {: the short comment(one line) for the symbol. }
    property Caption: string read FCaption write FCaption;
    {: the long comment(multi-line) for the symbol. }
    property Description: string read FDescription write FDescription;
    {: the symbol name }
    property Name: string read FName write FName;
    property Parent: TObject read FParent write FParent;
    {: Indicates how many times this symbol is refered in the parsed code for
            compiling only. }
    { Description
    }
    property Refs: LongInt read FRefs write FRefs;
  end;

  TTurboTypeSymbol = class(TTurboSymbol)
  private
    FTypeKind: TTurboTypeKind;
  public
    property TypeKind: TTurboTypeKind read FTypeKind write FTypeKind;
  end;

  TTurboModuleSymbol = class(TTurboSymbol)
  private
    FConstants: Integer;
    FVariables: Integer;
    FWords: Integer;
  public
    property Constants: Integer read FConstants write FConstants;
    property Variables: Integer read FVariables write FVariables;
    property Words: Integer read FWords write FWords;
  end;

  TTurboSymbolList = class(TList)
  private
    function GetItems(Index: Integer): TTurboSymbol;
  protected
    FOwner: TTurboSymbol;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(aOwner: TTurboSymbol); reintroduce;
    function Find(const aName: String): Integer;
    property Items[Index: Integer]: TTurboSymbol read GetItems; default;
    property Owner: TTurboSymbol read FOwner write FOwner;
  end;

  {: the module constant symbol. }
  TTurboConstSymbol = class(TTurboSymbol)
  private
    FTypeKind: TTurboTypeSymbol;
    FValue: string;
    procedure SetValue(const aValue: string);
  protected
    procedure iCompileTo(const aModule: TCustomTurboModule); override;
  public
    property TypeKind: TTurboTypeSymbol read FTypeKind write FTypeKind;
    property Value: string read FValue write SetValue;
  end;

  {: the module variable symbol. }
  TTurboVariableSymbol = class(TTurboConstSymbol)
  private
    FAddr: Integer;
  public
    property Addr: Integer read FAddr write FAddr;
  end;

  TTurboConstSymbolList = class(TTurboSymbolList)
  private
    function GetItems(Index: Integer): TTurboConstSymbol;
  public
    {: if successful then return the index number, else return -1 means failed.
            }
    function Add(const aName, aValue: String): Integer;
    procedure AddToMem(Index: Integer); override;
    function CreateConst(const aName, aValue: String): TTurboConstSymbol;
    property Items[Index: Integer]: TTurboConstSymbol read GetItems; default;
  end;

  {: the turbo script virtual machine code symbol. }
  TTurboWordSymbol = class(TCustomTurboWordSymbol)
  private
    FBody: TTurboWordSymbolList;
  protected
    procedure iCompileTo(const aModule: TCustomTurboModule); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Body: TTurboWordSymbolList read FBody;
  end;

  TTurboVariableSymbolList = class(TTurboSymbolList)
  private
    function GetItems(Index: Integer): TTurboVariableSymbol;
    function GetOwner: TTurboSymbol;
  public
    function Add: TTurboVariableSymbol;
    property Items[Index: Integer]: TTurboVariableSymbol read GetItems; default;
    property Owner: TTurboSymbol read GetOwner;
  end;


implementation

{
********************************* TTurboSymbol *********************************
}
constructor TTurboSymbol.Create;
begin
  inherited Create;
end;

destructor TTurboSymbol.Destroy;
begin
  if Assigned(FMetaInfo) then
  begin
    Dispose(FMetaInfo);
    FMetaInfo := nil;
  end;
  inherited Destroy;
end;

procedure TTurboSymbol.CompileTo(const aModule: TCustomTurboModule);
begin
  if FRefs > 0 then iCompileTo(aModule);
end;

{
******************************* TTurboSymbolList *******************************
}
constructor TTurboSymbolList.Create(aOwner: TTurboSymbol);
begin
  inherited Create;
  FOwner := aOwner;
end;

function TTurboSymbolList.Find(const aName: String): Integer;
begin
  For Result := 0 to Count - 1 do
  begin
    if aName = Items[Result].Name then
      exit;
  end;
  Result := -1;
end;

function TTurboSymbolList.GetItems(Index: Integer): TTurboSymbol;
begin
  Result := TTurboSymbol(Get(Index));
end;

procedure TTurboSymbolList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (TObject(Ptr) is TObject) then
  begin
    TObject(Ptr).Free;
    Ptr := nil;
  end;
end;

{
****************************** TTurboConstSymbol *******************************
}
procedure TTurboConstSymbol.iCompileTo(const aModule: TCustomTurboModule);
var
  vTypeSize: Integer;
  vValue: Pointer;
begin
  vTypeSize := GetTurboTypeSize(aType, Value);
  Integer(vValue) := Integer(aModule.Memory)  + aModule.UsedMemory;
  aModule.AllocSpace(vTypeSize);

  case TypeKind of
      ttUnknown: begin
        //may be Identifier
      end
      ;
      {ttShortString: begin
        //
        PShortString(vValue)^[0] := Length(Value);
        Move(Value[1], vValue^, Length(Value));
      end;//}
      ttSByte: PShortInt(vValue)^  := StrToInt(Value);
      ttUByte, ttChar, ttSet: PByte(vValue)^ := StrToInt(Value);
      ttSWord:PSmallInt(vValue)^ := StrToInt(Value);
      ttUWord: PWord(vValue)^ := StrToInt(Value);
      ttSLong: PInteger(vValue)^ := StrToInt(Value);
      ttULong: PLongWord(vValue)^ := StrToInt(Value);
      ttQWord, ttInt64: PInt64(vValue)^:= StrToInt(Value);
  end;
  inherited iCompileTo(aModule);
end;

procedure TTurboConstSymbol.SetValue(const aValue: string);
var
  I: Int64;
begin
  if aValue[1] = '''' then
  begin
    TypeKind := ttShortString;
    FValue := AnsiDequotedStr(aValue, '''');
  end
  else
    try
       FValue := aValue;
       I := StrToInt(aValue);
       if (i >= Low(ShortInt)) and (i<=High(ShortInt)) then
         TypeKind := ttSByte
       else if (i >= Low(Byte)) and (i<=High(Byte)) then
         TypeKind := ttUByte
       else if (i >= Low(SmallInt)) and (i<=High(SmallInt)) then
         TypeKind := ttSWord
       else if (i >= Low(Word)) and (i<=High(Word)) then
         TypeKind := ttUWord
       else if (i >= Low(LongInt)) and (i<=High(LongInt)) then
         TypeKind := ttSLong
       else if (i >= Low(LongWord)) and (i<=High(LongWord)) then
         TypeKind := ttULong
       else //if (i >= Low(Int64)) and (i<=High(Int64)) then
         TypeKind := ttInt64;
    except
      TypeKind := ttUnknown;
    end;
end;

{
**************************** TTurboConstSymbolList *****************************
}
function TTurboConstSymbolList.Add(const aName, aValue: String): Integer;
begin
  Result := Find(aName);
  if Result = -1 then
  begin
    Result := inherited Add(CreateConst(aName, aValue));
  end
  else
    Result := -1;
end;

procedure TTurboConstSymbolList.AddToMem(Index: Integer);
begin
  inherited AddToMem(Index);
end;

function TTurboConstSymbolList.CreateConst(const aName, aValue: String):
        TTurboConstSymbol;
begin
  Result := TTurboConstSymbol.Create;
  Result.Parent := Owner;
  Result.Name  := aName;
  Result.Value := aValue;
end;

function TTurboConstSymbolList.GetItems(Index: Integer): TTurboConstSymbol;
begin
  Result := TTurboSymbol(Get(Index));
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

procedure TTurboWordSymbol.iCompileTo(const aModule: TCustomTurboModule);
begin
end;

{
*************************** TTurboVariableSymbolList ***************************
}
function TTurboVariableSymbolList.Add: TTurboVariableSymbol;
begin
  Result := TTurboVariableSymbol.Create;
  try
    Result.Parent := FOwner;
    inherited Add(Result);
  except
    FreeAndNil(Result);
  end;
end;

function TTurboVariableSymbolList.GetItems(Index: Integer):
        TTurboVariableSymbol;
begin
  Result := TTurboModule(inherited Get(Index));
end;

function TTurboVariableSymbolList.GetOwner: TTurboSymbol;
begin
  Result := TTurboSymbol(FOwner);
end;



end.
