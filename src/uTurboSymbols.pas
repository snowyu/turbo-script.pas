unit uTurboSymbols;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uTurboConsts
  , uTurboExecutor 
  ;

type
  TTurboSymbol = class;
  TTurboTypeSymbol = class;
  TTurboSymbolList = class;
  TTurboConstSymbol = class;
  TTurboVariableSymbol = class;
  TTurboConstList = class;
  TTurboWordSymbol = class;
  TTurboVariableList = class;
  {: the abstract metadata class for symbol. }
  TTurboSymbol = class(TObject)
  private
    FCaption: string;
    FDescription: string;
    FName: string;
    FParent: TObject;
    FRefs: LongInt;
  public
    {: :add the index symbol content to memory. }
    { Description
    for compile, maybe i should rename to CompileTo?
    }
    procedure CompileTo(const aModule: TCustomTurboModule; Index: Integer = -1);
            virtual;
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

  TTurboSymbolList = class(TList)
  private
    function GetItems(Index: Integer): TTurboSymbol;
  protected
    FOwner: TCustomTurboModule;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(aOwner: TObject); reintroduce;
    function Find(const aName: String): Integer;
    property Items[Index: Integer]: TTurboSymbol read GetItems; default;
    property Owner: TCustomTurboModule read FOwner write FOwner;
  end;

  TTurboConstSymbol = class(TTurboSymbol)
  private
    FTypeKind: TTurboTypeSymbol;
    FValue: string;
    procedure SetValue(const aValue: string);
  public
    procedure CompileTo(const aModule: TCustomTurboModule; Index: Integer = -1);
            override;
    property TypeKind: TTurboTypeSymbol read FTypeKind write FTypeKind;
    property Value: string read FValue write SetValue;
  end;

  TTurboVariableSymbol = class(TTurboConstSymbol)
  private
    FAddr: Integer;
  public
    property Addr: Integer read FAddr write FAddr;
  end;

  TTurboConstList = class(TTurboSymbolList)
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
  public
    constructor Create;
    destructor Destroy; override;
    property Body: TTurboWordSymbolList read FBody;
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


implementation

{
********************************* TTurboSymbol *********************************
}
procedure TTurboSymbol.CompileTo(const aModule: TCustomTurboModule; Index:
        Integer = -1);
begin
  Inc(FRefs);
end;

{
******************************* TTurboSymbolList *******************************
}
constructor TTurboSymbolList.Create(aOwner: TObject);
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
procedure TTurboConstSymbol.CompileTo(const aModule: TCustomTurboModule; Index:
        Integer = -1);
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
  inherited CompileTo(aModule, Index);
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
******************************* TTurboConstList ********************************
}
function TTurboConstList.Add(const aName, aValue: String): Integer;
begin
  Result := Find(aName);
  if Result = -1 then
  begin
    Result := inherited Add(CreateConst(aName, aValue));
  end
  else
    Result := -1;
end;

procedure TTurboConstList.AddToMem(Index: Integer);
begin
  inherited AddToMem(Index);
end;

function TTurboConstList.CreateConst(const aName, aValue: String):
        TTurboConstSymbol;
begin
  Result := TTurboConstSymbol.Create;
  Result.Parent := Owner;
  Result.Name  := aName;
  Result.Value := aValue;
end;

function TTurboConstList.GetItems(Index: Integer): TTurboConstSymbol;
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



end.
