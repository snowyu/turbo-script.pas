unit uTurboCompilerUtils;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  //, TypInfo
  , uMeObject
  , uMeTypes
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboExecutor
  ;


Type
  //the Compiler option state
  TTurboCompilerOptionState = (cosDefault, cosEnable, cosDisable);

  PTurboSymbols = ^ TTurboSymbols;

  PTurboCustomSymbol = ^ TTurboCustomSymbol;
  TTurboCustomSymbol = Object(TMeDynamicObject)
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aObj: PTurboCustomSymbol); virtual;
  public
    Name: String;
  end;

  PTurboSymbol = ^ TTurboSymbol;
  TTurboSymbol = Object(TTurboCustomSymbol)
  public
    procedure Assign(const aObj: PTurboCustomSymbol); virtual;
  public
    Visibility: TTurboVisibility;
  end;

  PTurboLabelSymbol = ^ TTurboLabelSymbol;
  TTurboLabelSymbol = object(TTurboCustomSymbol)
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aObj: PTurboCustomSymbol); virtual;
  public
    WordName: String;
    Addr: Integer;
  end;
  
  PTurboConstSymbol = ^TTurboConstSymbol;
  TTurboConstSymbol = object(TTurboSymbol)
  protected
    FTurboType: PMeType;
    FValue: TMeVarRec;
    FValueStr: String;
    FSize: Integer;
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aObj: PTurboCustomSymbol); virtual;
    procedure SetTurboType(const aValue: PMeType);
    {: assign value to mem! }
    {
      note: you must allocate the space first!
    }
    function AssignValueTo(const Source: Pointer): Boolean;
    //根据aValue 如果aTypeKind is mtkUnknown 那么会自动判断其类型
    function AssignValue(const aValue: string; aType: PMeType = nil): Boolean;
    //if the value is string then save the string to the module's dataMemory.
    //and put the offset of the dataMemory to Value.VInteger
    procedure SaveString(const aModule: TCustomTurboModule);
  public
    property TurboType: PMeType read FTurboType write SetTurboType;
    property Value: TMeVarRec read FValue write FValue;
    property ValueStr: string read FValueStr write FValueStr;
    property Size: Integer read FSize write FSize;
  end;

  PTurboVarSymbol = ^ TTurboVarSymbol;
  TTurboVarSymbol = object(TTurboConstSymbol)
  public
    procedure Assign(const aObj: PTurboCustomSymbol); virtual;
  public
    Addr: Integer;
  end;

  PTurboMethodSymbol = ^ TTurboMethodSymbol;
  TTurboMethodSymbol = object(TTurboSymbol)
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aObj: PTurboCustomSymbol); virtual;
  public
    //Options: TTurboWordOptions;
    CallStyle: TTurboCallStyle;
    CodeFieldStyle: TTurboCodeFieldStyle;
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord;
    CFA: tsInt;//the offset address of the memory.
    ModuleName: String;  //for external word
    ModuleType: TTurboModuleType;  //for external word
    ModuleIndex: Integer; //for external word
    Module: TCustomTurboModule; //for external forth word
    ExternalOptions: TTurboExteralMethodOptions; //Exteral Word Options
  end;

  PTurboModuleSymbol = ^ TTurboModuleSymbol;
  TTurboModuleSymbol = object(TTurboSymbol)
  protected
    //collects the published methods in the Module.
    FMethods: PTurboSymbols;
  protected
    function GetMethods: PTurboSymbols;
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aObj: PTurboCustomSymbol); virtual;
    function FindMethod(const aName: string): PTurboMethodSymbol;
  public
    Entry: PTurboModuleRefEntry;
    ModuleType: TTurboModuleType;
    Module: TCustomTurboModule;
    property Methods: PTurboSymbols read GetMethods;
  end;

  TTurboSymbols = object(TMeList)
  protected
    function GetItem(Index: Integer): PTurboSymbol;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
    function IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer;
    function Find(const aName: string): PTurboSymbol;
  public
    property Items[Index: Integer]: PTurboSymbol read GetItem; default;
  end;

//convert the unit string(100KB, 100MB) to int(byte).
function UnitStrToInt(s: string): integer;
//function GetSimpleTurboTypeSize(const aTypeKind: TMeTypeKind): Integer;

implementation

function UnitStrToInt(s: string): integer;
var
  vUnit: integer;
begin
  vUnit := 1;
  if Length(s) >= 3 then
  begin
    if UpCase(s[Length(s)]) = 'B' then
    begin
      if UpCase(s[Length(s)-1]) = 'K' then
      begin
        vUnit := 1024;
        Delete(s, Length(s)-2, 2);
      end
      else if UpCase(s[Length(s)-1]) = 'M' then
      begin
        vUnit := 1024 * 1024;
        Delete(s, Length(s)-2, 2);
      end;
    end;
  end;
    Result := StrToIntDef(s, -1);
    if Result > 0 then 
    begin 
      Result := Result * vUnit;
    end;
end;

{ TTurboCustomSymbol }
destructor TTurboCustomSymbol.Destroy;
begin
  Name := '';
  Inherited;
end;

procedure TTurboCustomSymbol.Assign(const aObj: PTurboCustomSymbol);
begin
  if Assigned(aObj) and aObj.InheritsFrom(Self.ClassType) then
    Name := aObj.Name;
end;

{ TTurboSymbol }
procedure TTurboSymbol.Assign(const aObj: PTurboCustomSymbol);
begin
  if Assigned(aObj) and aObj.InheritsFrom(Self.ClassType) then
    with PTurboSymbol(aObj)^ do
    begin
      Self.Visibility := Visibility;
    end;
  Inherited;
end;

{ TTurboLabelSymbol }
destructor TTurboLabelSymbol.Destroy;
begin
  WordName := '';
  Inherited;
end;

procedure TTurboLabelSymbol.Assign(const aObj: PTurboCustomSymbol);
begin
  if Assigned(aObj) and aObj.InheritsFrom(Self.ClassType) then
    with PTurboLabelSymbol(aObj)^ do
    begin
      Self.WordName := WordName;
    end;
  Inherited;
end;

{ TTurboMethodSymbol }
destructor TTurboMethodSymbol.Destroy;
begin
  ModuleName := '';
  Inherited;
end;

procedure TTurboMethodSymbol.Assign(const aObj: PTurboCustomSymbol);
begin
  if Assigned(aObj) and aObj.InheritsFrom(Self.ClassType) then
    with PTurboMethodSymbol(aObj)^ do
    begin
      Self.ModuleName := ModuleName;
      Self.CallStyle := CallStyle;
      Self.CodeFieldStyle := CodeFieldStyle;
      Self.ParamFieldLength := ParamFieldLength;
      Self.CFA:=CFA;
      Self.ModuleType:=ModuleType;
      Self.ModuleIndex:=ModuleIndex;
      Self.Module:=Module;
      Self.ExternalOptions:=ExternalOptions;
    end;
  Inherited;
end;

{ TTurboModuleSymbol }
destructor TTurboModuleSymbol.Destroy;
begin
  MeFreeAndNil(FMethods);
  //ModuleName := '';
  Inherited;
end;

procedure TTurboModuleSymbol.Assign(const aObj: PTurboCustomSymbol);
begin
  if Assigned(aObj) and aObj.InheritsFrom(Self.ClassType) then
    with PTurboModuleSymbol(aObj)^ do
    begin
      Self.Entry:= Entry;
      Self.ModuleType:=ModuleType;
      Self.Module:=Module;
      if Assigned(FMethods) then
        Self.Methods.Assign(FMethods)
      else 
        MeFreeAndNil(Self.FMethods);
    end;
  Inherited;
end;

function TTurboModuleSymbol.FindMethod(const aName: string): PTurboMethodSymbol;
var
  vMethodEntry: PTurboMethodEntry;
begin
  Result := PTurboMethodSymbol(Methods.Find(aName));
  if not Assigned(Result) and Assigned(Module) then
  begin
    vMethodEntry := Module.FindWordEntry(aName);
    if Assigned(vMethodEntry) and (vMethodEntry.Word.CodeFieldStyle = cfsFunction) then
    begin
        New(Result, Create);
        Result.Name := aName;
        Result.CFA := vMethodEntry.Word.MethodAddr;
        Result.CallStyle := vMethodEntry.Word.CallStyle;
        Result.CodeFieldStyle := cfsExternalFunction;
        Result.ModuleType := mtLib;
        Result.ModuleName := Name;
        Result.Module := Module;
        Methods.Add(Result);
    end;
  end;
end;

function TTurboModuleSymbol.GetMethods: PTurboSymbols;
begin
  if not Assigned(FMethods) then
  begin
    New(FMethods, Create);
  end;
  Result := FMethods;
end;

{ TTurboConstSymbol }
destructor TTurboConstSymbol.Destroy;
begin
  FValueStr := '';
  Inherited;
end;

procedure TTurboConstSymbol.Assign(const aObj: PTurboCustomSymbol);
begin
  if Assigned(aObj) and aObj.InheritsFrom(Self.ClassType) then
    with PTurboConstSymbol(aObj)^ do
    begin
      Self.FValueStr := FValueStr;
      Self.FTurboType:= FTurboType;
      Self.FValue:= FValue;
      Self.FSize := FSize;
    end;
  Inherited;
end;

function TTurboConstSymbol.AssignValue(const aValue: string; aType: PMeType): Boolean;
begin
  Result := True;
  //writeln('AssignValue:', Integer(aTypeKind));
  if (aType = nil) and (aValue[1] = '''') then
  begin
       //aTypeKind := mtkString;
       aType := GetRegisteredTypeByTypeInfo(TypeInfo(ShortString));
  end
  else if Assigned(aType) then
  begin
    Case aType.Kind of
      mtkString, mtkLString: 
      begin
        ValueStr := AnsiDequotedStr(aValue, '''');
      end;
      mtkChar:
      begin
        ValueStr := AnsiDequotedStr(aValue, '''');
        FValue.VByte := Ord(ValueStr[1]);
      end;
      mtkInteger, mtkInt64:
      try
        ValueStr := aValue;
        FValue.VInt64 := StrToInt64(aValue);
      except
        aType := nil;
        Result := False;
      end;
      mtkFloat:
      try
        ValueStr := aValue;
        FValue.VExtended := StrToFloat(aValue);
      except
        aType := nil;
        Result := False;
      end;
    end; //case
  end
  else //try....
  begin
      ValueStr := aValue;
      try //is Integer?
        FValue.VInt64 := StrToInt64(aValue);
        if (Value.VInt64 >= Low(ShortInt)) and (Value.VInt64<=High(ShortInt)) then
        begin
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(ShortInt));
        end
        else if (Value.VInt64 >= Low(Byte)) and (Value.VInt64<=High(Byte)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Byte))
        else if (Value.VInt64 >= Low(SmallInt)) and (Value.VInt64<=High(SmallInt)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(SmallInt))
        else if (Value.VInt64 >= Low(Word)) and (Value.VInt64<=High(Word)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Word))
        else if (Value.VInt64 >= Low(LongInt)) and (Value.VInt64<=High(LongInt)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(LongInt))
        else if (Value.VInt64 >= Low(LongWord)) and (Value.VInt64<=High(LongWord)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(LongWord))
        else //if (Value.VInt64 >= Low(Int64)) and (Value.VInt64<=High(Int64)) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Int64))
      except
        aType := nil;
        Result := False;
      end;

      if not Assigned(aType) then
      try //is Float?
        FValue.VExtended := StrToFloat(aValue);
        if (ABS(Value.VExtended) >= 1.5e-45) and (ABS(Value.VExtended) <= 3.4e38) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Single))
        else if (ABS(Value.VExtended) >= 5e-308) and (ABS(Value.VExtended) <= 1.7e308) then
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Double))
        else
          aType := GetRegisteredTypeByTypeInfo(TypeInfo(Extended))
      except
        aType := nil;
        Result := False;
      end
  end;

  TurboType := aType;
  if Assigned(aType) then begin
    Result := True;
  end;
end;

function TTurboConstSymbol.AssignValueTo(const Source: Pointer): Boolean;
begin
  Result := True;
  //writeln('AssignValueTo:', ValueStr);
  move(Value, Source^, Size);
  //writeln(InttoHex(Value.VInteger, 4));
  //writeln('TurboType=', Integer(TurboType));
  //writeln('PSource=', PInteger(Source)^);
end;

procedure TTurboConstSymbol.SaveString(const aModule: TCustomTurboModule);
begin
  if Assigned(TurboType) then
    Case TurboType.Kind of
      mtkString: begin
        FValue.VInteger := aModule.UsedDataSize;
        aModule.AddByteToData(Length(ValueStr));
        aModule.AddBufferToData(ValueStr[1], Length(ValueStr));
      end;
      mtkLString: begin
        aModule.AddIntToData(-1);
        aModule.AddIntToData(Length(ValueStr));
        FValue.VInteger := aModule.UsedDataSize;
        aModule.AddBufferToData(ValueStr[1], Length(ValueStr));
        aModule.AddByteToData(0);
      end;
    end;//case
end;

procedure TTurboConstSymbol.SetTurboType(const aValue: PMeType);
begin
  if FTurboType <> aValue then
  begin
    FTurboType := aValue;
    if Assigned(aValue) then
      FSize := GetTypeSize(aValue)
    else
      FSize := 0;
  end;
end;

{ TTurboVarSymbol }
procedure TTurboVarSymbol.Assign(const aObj: PTurboCustomSymbol);
begin
  if Assigned(aObj) and aObj.InheritsFrom(Self.ClassType) then
    with PTurboVarSymbol(aObj)^ do
    begin
      Self.Addr := Addr;
    end;
  Inherited;
end;

{ TTurboSymbols }
destructor TTurboSymbols.Destroy;
begin
  FreeMeObjects;
  inherited;
end;

procedure TTurboSymbols.Clear;
begin
  FreeMeObjects;
  inherited;
end;

function TTurboSymbols.GetItem(Index: Integer): PTurboSymbol;
begin
  Result := Inherited Get(Index);
end;

function TTurboSymbols.Find(const aName: string): PTurboSymbol;
var
  i: integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

function TTurboSymbols.IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if AnsiSameText(aName, Items[Result].Name) then
      exit;
  end;
  Result := -1;
end;

initialization
  SetMeVirtualMethod(TypeOf(TTurboCustomSymbol), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TTurboSymbol), ovtVmtParent, TypeOf(TTurboCustomSymbol));
  SetMeVirtualMethod(TypeOf(TTurboLabelSymbol), ovtVmtParent, TypeOf(TTurboCustomSymbol));
  SetMeVirtualMethod(TypeOf(TTurboConstSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboVarSymbol), ovtVmtParent, TypeOf(TTurboConstSymbol));
  SetMeVirtualMethod(TypeOf(TTurboMethodSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboModuleSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
end.
