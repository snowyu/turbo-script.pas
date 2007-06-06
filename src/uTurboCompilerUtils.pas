unit uTurboCompilerUtils;

interface

{$I TurboScript.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  //, TypInfo
  , uMeObject
  , uMeTypes
  , uMeProcType
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboExecutor
  ;


Type
  //the Compiler option state
  TTurboCompilerOptionState = (cosDefault, cosEnable, cosDisable);

  PTurboSymbols = ^ TTurboSymbols;
  PTurboModuleSymbols = ^ TTurboModuleSymbols;
  PTurboTypeSymbols = ^ TTurboTypeSymbols;
  PTurboCustomSymbol = ^ TTurboCustomSymbol;
  PTurboSymbol = ^ TTurboSymbol;
  PTurboLabelSymbol = ^ TTurboLabelSymbol;
  PTurboConstSymbol = ^TTurboConstSymbol;
  PTurboVarSymbol = ^ TTurboVarSymbol;
  PTurboMethodSymbol = ^ TTurboMethodSymbol;
  PTurboTypeSymbol = ^ TTurboTypeSymbol;
  PTurboTypeRefSymbol = ^ TTurboTypeRefSymbol;
  PTurboModuleSymbol = ^ TTurboModuleSymbol;

  TTurboCustomSymbol = Object(TMeDynamicObject)
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
  public
    Name: String;
  end;

  TTurboSymbol = Object(TTurboCustomSymbol)
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    function IsPublic(const aModule: TCustomTurboModule): Boolean;
    function IsTyped(const aModule: TCustomTurboModule): Boolean;
    function IsNamed(const aModule: TCustomTurboModule): Boolean;
  public
    Visibility: TTurboVisibility;
    {: the ModuleSymbol owns this symbol. }
    ModuleSymbol: PTurboModuleSymbol;
  end;

  TTurboLabelSymbol = object(TTurboCustomSymbol)
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    procedure DeclareTo(const aModule: TCustomTurboModule);
  public
    Method: PTurboMethodSymbol;
    Addr: Integer;
  end;
  
  TTurboConstSymbol = object(TTurboSymbol)
  protected
    FTurboType: PMeType;
    FValue: TMeVarRec;
    FValueStr: String;
    FSize: tsInt;
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    procedure SetTurboType(const aValue: PMeType);
    {: assign value to mem! }
    {
      note: you must allocate the space first!
    }
    function AssignValueTo(const Source: Pointer): Boolean;
    //根据aValue 如果aTypeKind is mtkUnknown 那么会自动判断其类型
    function AssignValue(const aValue: string; aType: PMeType = nil; const aQuote: char = ''''): Boolean; overload;
    procedure AssignValue(const aConst: PTurboConstSymbol); overload;
    //if the value is string then save the string to the module's dataMemory.
    //and put the offset of the dataMemory to Value.VInteger
    procedure DeclareStringTo(const aModule: TCustomTurboModule);
    //generate the op-code push const for the constant to the aModule
    procedure PushTo(const aModule: TCustomTurboModule); virtual;
  public
    property TurboType: PMeType read FTurboType write SetTurboType;
    property Value: TMeVarRec read FValue write FValue;
    property ValueStr: string read FValueStr write FValueStr;
    //这里的Size是指压入(PushTo)到代码区中数据大小
    property Size: tsInt read FSize write FSize;
  end;

  TTurboVarSymbol = object(TTurboConstSymbol)
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    //declare(allocate space) the variable to aModule
    procedure DeclareTo(const aModule: TCustomTurboModule);
    //generate the op-code push variable addr to the aModule
    procedure PushTo(const aModule: TCustomTurboModule); virtual; {override}
  public
    Addr: tsInt;
  end;

  TTurboMethodSymbol = object(TTurboSymbol)
  protected
    FLabels: PTurboSymbols;
    FiTypeSymbol: PTurboTypeSymbol; //internal used, when this type is not Declared!
    FTypeSymbol: PTurboTypeSymbol;
  protected
    procedure Init;virtual; {override}
    function GetLabels: PTurboSymbols;
    function GetTypeSymbol: PTurboTypeSymbol;
    function GetTypeInfo: PMeProcType;
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    //generate the call op-code to the aModule
    procedure PushTo(const aModule: TCustomTurboModule); virtual; {override}
    procedure DeclareTo(const aModule: TCustomTurboModule);
    //for local method to push the opReturn instruction.
    procedure DeclareEndTo(const aModule: TCustomTurboModule);
    function DeclareLabelTo(const aLabelName: string; const aModule: TCustomTurboModule): Integer;
    function FindLabel(const aName: string): PTurboLabelSymbol;
  public
    //Options: TTurboWordOptions;
    CallStyle: TCallingConvention;
    CodeFieldStyle: TTurboCodeFieldStyle;
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord; //this is in the Entry.
    CFA: tsInt;//the offset address of the memory.
    ModuleName: String;  //for external word
    ModuleType: TTurboModuleType;  //for external word
    //Module: TCustomTurboModule; //for external forth word
    ExternalOptions: TTurboExteralMethodOptions; //Exteral Word Options
    Entry: PTurboMethodEntry;
    property Labels: PTurboSymbols read GetLabels;
    property TypeSymbol: PTurboTypeSymbol read GetTypeSymbol;
    property TypeInfo: PMeProcType read GetTypeInfo;
  end;

  {: the user defined type symbol.}
  {
    类型先挂在 iType 上,等DeclareTo到模块后，设置 iType 为 nil， 在转为挂在 MeType
    总是 MeFreeAndNil(iType)， 这样自然解决未用类型的删除问题！
  }
  TTurboTypeSymbol = object(TTurboSymbol)
  protected
    {: internal used type, not declare to the module.}
    FiType: PMeType;
    FTypeInfo: PMeType;
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    procedure DeclareTo(const aModule: TCustomTurboModule);
    function GetTypeInfo(const aClass: TMeClass = nil): PMeType;
  public
    destructor Destroy; virtual; {override}
  public
    Entry: PTurboTypeInfoEntry;
    //Property TypeInfo: PMeType read GetTypeInfo;
  end;

  TTurboTypeRefSymbol = object(TTurboTypeSymbol)
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
  public
    //use the TypeSymbol's entry:
    //Entry: PTurboTypeRefEntry;
    //this typeRef is Declared to RefModule in fact.
    RefModule: PTurboModuleSymbol;
    TypeId: tsInt;
  end;

  TTurboModuleSymbol = object(TTurboSymbol)
  protected
    //collects methods in the Module.
    FMethods: PTurboSymbols;
    FTypes: PTurboTypeSymbols;
    FTypeRefs: PTurboTypeSymbols;
    FVars: PTurboSymbols;
    FConsts: PTurboSymbols;
    FUsedModules: PTurboModuleSymbols;
  protected
    function GetTypeRefs: PTurboTypeSymbols;
    function GetTypes: PTurboTypeSymbols;
    function GetVars: PTurboSymbols;
    function GetConsts: PTurboSymbols;
    function GetMethods: PTurboSymbols;
    function GetUsedModules: PTurboModuleSymbols;
    function CreateLibModuleSymbol(const aName: string): PTurboModuleSymbol;
  public
    destructor Destroy; virtual; {override}
    procedure Clear;
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    function FindType(const aName: string): PMeType;
    function FindLocalMethod(const aName: string): PTurboMethodSymbol;
    function FindLocalConst(const aName: string; const aType: PMeType): PTurboConstSymbol;
    function AddUsedModule(const aName: String; const aModuleType: TTurboModuleType): Integer;
    procedure DeclareTo(const aModule: TCustomTurboModule);
    function IsUniqueIdentifier(const aName: String): Boolean;
    procedure PushInt32(const aStr: string);overload;
    procedure PushInt32(const aInt: tsInt);overload;

    //Create a new constant
    function NewConst(const aName: string): PTurboConstSymbol;
    function NewVar(const aName: string): PTurboVarSymbol;
    function NewType(const aName: string): PTurboTypeSymbol;
    function NewMethod(const aName: string): PTurboMethodSymbol;
    //if true then apply external param to the aMethod
    function IsExternalMethod(const aMethod: PTurboMethodSymbol): Boolean;
    //当带常量的变量有动态数组，AnsiString 的时候需要在模块的程序头添加初始化过程：对变量分配内存，并将常量赋值给变量；在程序尾添加终止过程，释放变量内存，并最后加上halt指令。
    //procedure DoInitModuleProc;
    //procedure DoFinalModuleProc;
  public
    Entry: PTurboModuleRefEntry;
    ModuleType: TTurboModuleType;
    Module: TCustomTurboModule;
    property Types: PTurboTypeSymbols read GetTypes;
    property TypeRefs: PTurboTypeSymbols read GetTypeRefs;
    property Vars: PTurboSymbols read GetVars;
    property Consts: PTurboSymbols read GetConsts;
    property Methods: PTurboSymbols read GetMethods;
    property UsedModules: PTurboModuleSymbols read GetUsedModules;
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

  TTurboModuleSymbols = object(TTurboSymbols)
  protected
    function GetItem(Index: Integer): PTurboModuleSymbol;
  public
    function IndexOf(const aModule: TCustomTurboModule; const aBeginIndex: Integer = 0): Integer; overload;
    function IndexOf(const aModuleEntry: PTurboModuleRefEntry; const aBeginIndex: Integer = 0): Integer; overload;
    function IndexOf(const aName: string; const aModuleType: TTurboModuleType; const aBeginIndex: Integer = 0): Integer; overload;
    function Find(const aName: string; const aModuleType: TTurboModuleType): PTurboModuleSymbol;overload;
    function Find(const aModule: TCustomTurboModule): PTurboModuleSymbol;overload;
    function Find(const aModuleEntry: PTurboModuleRefEntry): PTurboModuleSymbol;overload;
    function FindMethod(const aName: string; const aModuleType: TTurboModuleType = mtLib): PTurboMethodSymbol;
  public
    property Items[Index: Integer]: PTurboModuleSymbol read GetItem; default;
  end;

  TTurboTypeSymbols = object(TTurboSymbols)
  protected
    function GetItem(Index: Integer): PTurboTypeSymbol;
  public
    function IndexOf(const aType: PMeType; const aBeginIndex: Integer = 0): Integer; overload;
    function IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer; overload;
    function Find(const aName: string): PTurboTypeSymbol;overload;
    function Find(const aType: PMeType): PTurboTypeSymbol;overload;
  public
    property Items[Index: Integer]: PTurboTypeSymbol read GetItem; default;
  end;

//convert the unit string(100KB, 100MB) to int(byte).
function UnitStrToInt(s: string): integer;
//Identifier = LETTER {IDENTCHARS|DOT IDENTCHARS}.
{
 LETTER="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_".
 DIGIT =  "0123456789".
 DOT = ".".
 IDENTCHARS = LETTER + DIGIT.

}
function IsIdentifier(const aValue: string): Boolean;

implementation

const
  LetterSet = ['A'..'Z', 'a'..'z', '_'];
  DigitSet = ['0'..'9'];
  IdentSet = LetterSet + DigitSet;
  DotChar = '.';

function IsIdentifier(const aValue: string): Boolean;
var
  i: Integer;
begin
  i := 1;
  Result := aValue <> '';
  if Result then
  begin
    {$IFDEF MBCS_SUPPORT}
    if ByteType(aValue, i) <> mbSingleByte then
    begin
      Result := true;
      inc(i);
      while (ByteType(aValue, i) <> mbTrailByte) and (i < Length(aValue)) do inc(i);
    end
    else
    {$ENDIF}
      Result := aValue[i] in LetterSet;
    if Result then
    begin
      inc(i);
      while i <= Length(aValue) do
      begin
        {$IFDEF MBCS_SUPPORT}
        if ByteType(aValue, i) <> mbSingleByte then
        begin
          Result := true;
          inc(i);
          while (ByteType(aValue, i) <> mbTrailByte) and (i < Length(aValue)) do inc(i);
        end
        else
        {$ENDIF}
        if aValue[i] = DotChar then
        begin
          Result := ((i+1) <= Length(aValue)) and (aValue[i+1] in IdentSet);
        end
        else
          Result := aValue[i] in IdentSet;
        if not Result then exit;
        inc(i);
      end;
    end;
  end;
end;

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

procedure TTurboCustomSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    Name := aSymbol.Name;
end;

{ TTurboSymbol }
procedure TTurboSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboSymbol(aSymbol)^ do
    begin
      Self.Visibility := Visibility;
      Self.ModuleSymbol := ModuleSymbol;
    end;
  Inherited;
end;

function TTurboSymbol.IsPublic(const aModule: TCustomTurboModule): Boolean;
begin
  Result := ((Visibility and tvPublicVisibilty) = tvPublicVisibilty) or (([soSymbolPublic, soSymbolTyped, soSymbolNamed]*aModule.Options) <> []);
end;

function TTurboSymbol.IsTyped(const aModule: TCustomTurboModule): Boolean;
begin
  Result := ((Visibility and tvPublicTypedVisibilty) = tvPublicTypedVisibilty) or (soSymbolTyped in aModule.Options);
end;

function TTurboSymbol.IsNamed(const aModule: TCustomTurboModule): Boolean;
begin
  Result := ((Visibility and tvPublicNamedVisibilty) = tvPublicNamedVisibilty) or (soSymbolNamed in aModule.Options);
end;

{ TTurboLabelSymbol }
destructor TTurboLabelSymbol.Destroy;
begin
  //WordName := '';
  Inherited;
end;

procedure TTurboLabelSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboLabelSymbol(aSymbol)^ do
    begin
      Self.Method := Method;
      Self.Addr := Addr;
    end;
  Inherited;
end;

procedure TTurboLabelSymbol.DeclareTo(const aModule: TCustomTurboModule);
begin
  if Addr = 0 then
    Addr := aModule.UsedMemory;
end;

{ TTurboMethodSymbol }
procedure TTurboMethodSymbol.Init;
begin
  Inherited;
  CFA := -1;
end;

destructor TTurboMethodSymbol.Destroy;
begin
  ModuleName := '';
  MeFreeAndNil(FiTypeSymbol);
  MeFreeAndNil(FLabels);
  Inherited;
end;

procedure TTurboMethodSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboMethodSymbol(aSymbol)^ do
    begin
      Self.ModuleName := ModuleName;
      Self.CallStyle := CallStyle;
      Self.CodeFieldStyle := CodeFieldStyle;
      Self.ParamFieldLength := ParamFieldLength;
      Self.CFA:=CFA;
      Self.ModuleType:=ModuleType;
      //Self.Module:=Module;
      Self.ExternalOptions:=ExternalOptions;
      Self.Entry := Entry;
      Self.FTypeSymbol := FTypeSymbol;
      Self.FiTypeSymbol := FiTypeSymbol;
    end;
  Inherited;
end;

function TTurboMethodSymbol.FindLabel(const aName: string): PTurboLabelSymbol;
var
  i: integer;
begin
  with Labels^ do
  begin
    i := IndexOf(aName);
    if i >= 0 then
      Result := PTurboLabelSymbol(Items[i])
    else
      Result := nil;
  end;
end;

function TTurboMethodSymbol.GetLabels: PTurboSymbols;
begin
  if not Assigned(FLabels) then
  begin
    New(FLabels, Create);
  end;
  Result := FLabels;
end;

function TTurboMethodSymbol.GetTypeSymbol: PTurboTypeSymbol;
begin
  if Assigned(FTypeSymbol) then
    Result := FTypeSymbol
  else if Assigned(FiTypeSymbol) then
    Result := FiTypeSymbol
  else 
  begin
    //no Assigned, Create a one.
    New(FiTypeSymbol, Create);
    FiTypeSymbol.ModuleSymbol := ModuleSymbol;
    Result := FiTypeSymbol;
    //Result.GetTypeInfo(TypeOf(TMeProcType));
  end;
end;

function TTurboMethodSymbol.GetTypeInfo: PMeProcType;
begin
  Result := PMeProcType(TypeSymbol.GetTypeInfo(TypeOf(TMeProcType)));
end;

procedure TTurboMethodSymbol.DeclareTo(const aModule: TCustomTurboModule);
begin
  if not Assigned(Entry) then
  begin
    if CodeFieldStyle = cfsFunction then
    begin
      CFA := aModule.UsedMemory;
    end
    else //the external method.
      CFA := 0;
    if IsPublic(aModule) then
    begin
      //writeln('aModule.UsedDataSize=',InttoHex(aModule.UsedDataSize,4));
      aModule.AlignData;
      //writeln('aModule.UsedDataSize=',InttoHex(aModule.UsedDataSize,4));
      Integer(Entry) := Integer(aModule.DataMemory) + aModule.UsedDataSize;
      aModule.AllocDataSpace(SizeOf(TTurboMethodEntry));
      Entry.Prior := aModule.LastWordEntry;
      if IsNamed(aModule) then
      begin
        Entry.Word.Name := Pointer(aModule.UsedDataSize);
        //aModule.AddByteToData(Length(Name));
        aModule.AddBufferToData(Name[1], Length(Name));
        aModule.AddByteToData(0);
      end
      else
      begin
        //aModule.AddByteToData(0);
        Entry.Word.Name := nil;
      end;
      Entry.Word.MethodAddr := CFA;
      Entry.Word.Visibility := Visibility;
      Entry.Word.CallStyle := CallStyle;
      Entry.Word.CodeFieldStyle := CodeFieldStyle;
    end;
  end;
end;

procedure TTurboMethodSymbol.DeclareEndTo(const aModule: TCustomTurboModule);
var
  vIsPublic: Boolean;
begin
  if (CodeFieldStyle = cfsFunction) and Assigned(Entry) then
  begin
    vIsPublic := IsPublic(aModule);
    if vIsPublic then
      aModule.AddOpToMem(opExitFar)
    else
      aModule.AddOpToMem(opExit);
    ParamFieldLength := aModule.UsedMemory - CFA + 1;

    if vIsPublic then
    begin
      Entry.Word.ParamFieldLength := ParamFieldLength;
      aModule.LastWordEntry := Pointer(Integer(Entry) - Integer(aModule.DataMemory));
    end;
  end;
end;

function TTurboMethodSymbol.DeclareLabelTo(const aLabelName: string; const aModule: TCustomTurboModule): Integer;
var
  vLabel: PTurboLabelSymbol;
begin
  Result := Labels.IndexOf(aLabelName);
  if Result < 0 then
  begin
    New(vLabel, Create);
    Result := Labels.Add(vLabel);
    with vLabel^ do
    begin
      Name := alabelName;
      Method := @Self;
      //Addr := aModule.UsedMemory;
    end;
    vLabel.DeclareTo(aModule);
  end;
end;

procedure TTurboMethodSymbol.PushTo(const aModule: TCustomTurboModule);
begin
    case CodeFieldStyle of
      cfsFunction:
      begin
        if IsPublic(aModule) then
        begin
          aModule.AddOpToMem(opEnterFar);
          aModule.AddIntToMem(0);
          aModule.AddIntToMem(CFA);
        end
        else
        begin
          aModule.AddOpToMem(opEnter);
          aModule.AddIntToMem(CFA);
        end;
        //Result := True;
      end;
      cfsExternalFunction:
      begin
        case CallStyle of
          ccForth:
          begin
            if Assigned(ModuleSymbol) and Assigned(ModuleSymbol.Module) then
            begin
              if CFA = -1 then
              begin
                if ExternalOptions.Name <> '' then
                  CFA := ModuleSymbol.Module.GetWordCFA(ExternalOptions.Name)
                else
                  CFA := ModuleSymbol.Module.GetWordCFA(Name);
              end;
              if CFA <> -1 then
              begin
                aModule.AddOpToMem(opCallFar);
                //writeln(Name, '.ModEntry:',Integer(ModuleSymbol.Entry)- Integer(aModule.DataMemory));
                //point to the TurboModuleInfo
                if not Assigned(ExternalOptions.ModuleRef) then
                begin
                  ModuleSymbol.DeclareTo(aModule);
                  ExternalOptions.ModuleRef := Pointer(Integer(ModuleSymbol.Entry) + SizeOf(tsPointer));
                end;
                //writeln(Name, '.ModRef:',Integer(ExternalOptions.ModuleRef) - Integer(aModule.DataMemory));
                aModule.AddIntToMem(Integer(ExternalOptions.ModuleRef) - Integer(aModule.DataMemory));
                aModule.AddIntToMem(CFA);
                //writeln(Name, '.CFA:',CFA);
                //Result := True;
              end
              else
              begin
               //writeln('cWordNotFoundError');
                //SynError(cWordNotFoundError, '"'+ aName + '" CFA not in ' + FUsedModules.Items[ModuleIndex].Name);
              end;
            end
            else
            begin
              //writeln(Name, ' not assigned module!!');
              //SynError(cWordNotFoundError, '"'+ aName + '" not in ' + FUsedModules.Items[ModuleIndex].Name);
            end;
          end;
        end; //case
      end;
    end;//case
end;

{ TTurboModuleSymbol }
destructor TTurboModuleSymbol.Destroy;
begin
  Clear;
  Inherited;
end;

procedure TTurboModuleSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboModuleSymbol(aSymbol)^ do
    begin
      Self.Entry:= Entry;
      Self.ModuleType:=ModuleType;
      Self.Module:=Module;
      if Assigned(FMethods) then
        Self.Methods.Assign(FMethods)
      else 
        MeFreeAndNil(Self.FMethods);

      if Assigned(FVars) then
        Self.Vars.Assign(FVars)
      else 
        MeFreeAndNil(Self.FVars);

      if Assigned(FConsts) then
        Self.Consts.Assign(FConsts)
      else 
        MeFreeAndNil(Self.FConsts);

      if Assigned(FTypes) then
        Self.Types.Assign(FTypes)
      else 
        MeFreeAndNil(Self.FTypes);

      if Assigned(FTypeRefs) then
        Self.TypeRefs.Assign(FTypeRefs)
      else 
        MeFreeAndNil(Self.FTypeRefs);

      if Assigned(FUsedModules) then
        Self.UsedModules.Assign(FUsedModules)
      else 
        MeFreeAndNil(Self.FUsedModules);
    end;
  Inherited;
end;

procedure TTurboModuleSymbol.Clear;
begin
  MeFreeAndNil(FMethods);
  MeFreeAndNil(FUsedModules);
  MeFreeAndNil(FVars);
  MeFreeAndNil(FConsts);
  MeFreeAndNil(FTypes);
  MeFreeAndNil(FTypeRefs);
end;

function TTurboModuleSymbol.FindType(const aName: string): PMeType;
begin
  //find local type:
  Result := PMeType(Types.Find(aName));
  if Assigned(Result) then
    Result := PTurboTypeSymbol(Result).GetTypeInfo;
  if not Assigned(Result) then
  begin //TODO: find external type:
  end;

  if not Assigned(Result) then
  begin //find internal type:
    Result := GRegisteredTypes.GetRegisteredTypeByName(aName);
  end;
end;

function TTurboModuleSymbol.FindLocalConst(const aName: string; const aType: PMeType): PTurboConstSymbol;
var
  i: Integer;
begin
  with Consts^ do
  begin
    for i := 0 to Count - 1 do
    begin
      Result := PTurboConstSymbol(Items[i]);
      with Result^ do
      if (aName = Name) and (aType = TurboType) then
      begin
        exit;
      end;
    end;
  end; //with
  Result := nil;
end;

function TTurboModuleSymbol.FindLocalMethod(const aName: string): PTurboMethodSymbol;
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
        Result.ExternalOptions.ModuleRef := Pointer(Integer(Entry) + SizeOf(Pointer));
        Result.CallStyle := vMethodEntry.Word.CallStyle;
        Result.CodeFieldStyle := cfsExternalFunction;
        Result.ModuleType := mtLib;
        Result.ModuleName := Name;
        //Result.Module := Module;
        Result.ModuleSymbol := @Self;
        Result.Entry := vMethodEntry;
        Methods.Add(Result);
    end;
  end;
end;

function TTurboModuleSymbol.GetConsts: PTurboSymbols;
begin
  if not Assigned(FConsts) then
  begin
    New(FConsts, Create);
  end;
  Result := FConsts;
end;

function TTurboModuleSymbol.GetMethods: PTurboSymbols;
begin
  if not Assigned(FMethods) then
  begin
    New(FMethods, Create);
  end;
  Result := FMethods;
end;

function TTurboModuleSymbol.GetUsedModules: PTurboModuleSymbols;
begin
  if not Assigned(FUsedModules) then
  begin
    New(FUsedModules, Create);
  end;
  Result := FUsedModules;
end;

function TTurboModuleSymbol.GetTypeRefs: PTurboTypeSymbols;
begin
  if not Assigned(FTypeRefs) then
  begin
    New(FTypeRefs, Create);
  end;
  Result := FTypeRefs;
end;

function TTurboModuleSymbol.GetTypes: PTurboTypeSymbols;
begin
  if not Assigned(FTypes) then
  begin
    New(FTypes, Create);
  end;
  Result := FTypes;
end;

function TTurboModuleSymbol.GetVars: PTurboSymbols;
begin
  if not Assigned(FVars) then
  begin
    New(FVars, Create);
  end;
  Result := FVars;
end;

function TTurboModuleSymbol.IsExternalMethod(const aMethod: PTurboMethodSymbol): Boolean;
var
  i: Integer;
begin
  if aMethod.ModuleName = '' then
  begin
    i := UsedModules.Count -1;
    if i >= 0 then
    begin
      aMethod.ModuleSymbol := UsedModules.Items[i];
      aMethod.ModuleName := aMethod.ModuleSymbol.Name;
    end
    else
    begin 
      aMethod.ModuleSymbol := nil;
      //SynError(cDLLModuleMissError, aWord.Name);
    end;
  end
  else
  begin
    i := UsedModules.IndexOf(aMethod.ModuleName, aMethod.ModuleType);
    if i < 0 then i := AddUsedModule(aMethod.ModuleName, aMethod.ModuleType);
    if i >= 0 then
    begin
      aMethod.ModuleSymbol := UsedModules.Items[i];
    end
    else
    begin 
      //writeln('Can not add used module:' + aMethod.ModuleName);
      //SynError(cDLLModuleMissError, 'Can not add used module:' + aWord.ModuleName);
      aMethod.ModuleSymbol := nil;
    end;
  end;

  Result := Assigned(aMethod.ModuleSymbol); 
  if Result then
  begin
    Result := Assigned(aMethod.ModuleSymbol.FindLocalMethod(aMethod.Name));
    if Result and Assigned(aMethod.ModuleSymbol.Entry) then
    begin
      aMethod.ExternalOptions.ModuleRef := Pointer(Integer(aMethod.ModuleSymbol.Entry) + SizeOf(tsPointer));
    end
    else
      aMethod.ExternalOptions.ModuleRef := nil;
  end;
end;

function TTurboModuleSymbol.AddUsedModule(const aName: String; const aModuleType: TTurboModuleType): Integer;
var
  vModuleSymbol: PTurboModuleSymbol;
begin
  vModuleSymbol := nil;
  Result := UsedModules.IndexOf(aName, aModuleType);
  if Result = -1 then
  begin
    case aModuleType of
      mtLib: vModuleSymbol := CreateLibModuleSymbol(aName);
    end;
    if Assigned(vModuleSymbol) then
    begin
      Result := UsedModules.Add(vModuleSymbol);
    end;
  end
  //else
    //Result := -1;
end;

function TTurboModuleSymbol.CreateLibModuleSymbol(const aName: string): PTurboModuleSymbol;
var
  vModule: TCustomTurboModule;
begin
  vModule := Module.RequireModule(PChar(aName));
  if Assigned(vModule) then
  begin
    New(Result, Create);
    with Result^ do
    begin
      Name := aName;
      ModuleType := mtLib;
      Module := vModule;
    end;
    //Integer(Result.Entry) := Integer(Module.DataMemory) + Module.UsedDataSize; //the offset address.
  end
  else
    Result := nil;
end;

procedure TTurboModuleSymbol.DeclareTo(const aModule: TCustomTurboModule);
begin
  if not Assigned(Entry) then
  begin
    Integer(Entry) := Integer(aModule.DataMemory) + aModule.UsedDataSize; //the offset address.
    with aModule do
    begin
      AllocDataSpace(SizeOf(TTurboModuleRefEntry));
      Entry.Prior := LastModuleRefEntry;
      Entry.Module.ModuleType := ModuleType;
      Entry.Module.Revision := Module.ModuleVersion;
      Entry.Module.BuildDate := Module.ModuleDate;
      Entry.Module.Name := Pointer(UsedDataSize);
      if Length(Self.Name) > 0 then
        AddBufferToData(Self.Name[1], Length(Self.Name));
      AddByteToData(0);
      LastModuleRefEntry := Pointer(tsInt(Entry) - Integer(DataMemory));
    end;
  end;
end;

function TTurboModuleSymbol.IsUniqueIdentifier(const aName: String): Boolean;
begin
  Result := Consts.IndexOf(aName) < 0;
  if Result then
    Result := Vars.IndexOf(aName)< 0;
  if Result then
    Result := Methods.IndexOf(aName) < 0;
end;

function TTurboModuleSymbol.NewConst(const aName: string): PTurboConstSymbol;
begin
  if  (aName = '') or (Consts.IndexOf(aName) < 0) then
  begin
    New(Result, Create);
    Result.Name := aName;
    Result.ModuleSymbol := @Self;
    if aName <> '' then Consts.Add(Result);
  end
  else
    Result := nil;
end;

function TTurboModuleSymbol.NewVar(const aName: string): PTurboVarSymbol;
begin
  if (aName = '') or (Vars.IndexOf(aName) < 0) then
  begin
    New(Result, Create);
    Result.Name := aName;
    Result.ModuleSymbol := @Self;
    if aName <> '' then Vars.Add(Result);
  end
  else
    Result := nil;
end;
function TTurboModuleSymbol.NewMethod(const aName: string): PTurboMethodSymbol;
begin
  if (aName = '') or (Methods.IndexOf(aName) < 0) then
  begin
    New(Result, Create);
    Result.Name := aName;
    Result.ModuleSymbol := @Self;
    if aName <> '' then Methods.Add(Result);
  end
  else
    Result := nil;
end;

function TTurboModuleSymbol.NewType(const aName: string): PTurboTypeSymbol;
begin
  if (aName = '') or (Types.IndexOf(aName) < 0) then
  begin
    New(Result, Create);
    Result.Name := aName;
    Result.ModuleSymbol := @Self;
    if aName <> '' then Types.Add(Result);
  end
  else
    Result := nil;
end;

procedure TTurboModuleSymbol.PushInt32(const aStr: string);
begin
  PushInt32(StrToInt(aStr));
end;

procedure TTurboModuleSymbol.PushInt32(const aInt: tsInt);
begin
  Module.AddOpToMem(opPushInt);
  Module.AddIntToMem(aInt);
end;

{ TTurboConstSymbol }
destructor TTurboConstSymbol.Destroy;
begin
  FValueStr := '';
  Inherited;
end;

procedure TTurboConstSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
  begin
    AssignValue(PTurboConstSymbol(aSymbol));
  end;
  Inherited; //assign the name.
end;

procedure TTurboConstSymbol.AssignValue(const aConst: PTurboConstSymbol);
begin
  if Assigned(aConst) then
    with aConst^ do
    begin
      Self.FValueStr := FValueStr;
      Self.FTurboType:= FTurboType;
      Self.FValue:= FValue;
      Self.FSize := FSize;
    end;
end;

function TTurboConstSymbol.AssignValue(const aValue: string; aType: PMeType; const aQuote: char): Boolean;
var
  i: Integer;
begin
  //Result := True;
  //writeln('AssignValue:', Integer(aTypeKind));
  if (aType = nil) and (aValue[1] = aQuote) then
  begin
     if Length(aValue)-2 > 255 then
       aType := GetRegisteredTypeByTypeInfo(TypeInfo(String))
     else
       aType := GetRegisteredTypeByTypeInfo(TypeInfo(ShortString));
     ValueStr := AnsiDequotedStr(aValue, aQuote);
     Result := true;
  end
  else if Assigned(aType) then
  begin
    Result := Assigned(ModuleSymbol);
    if Result then
    begin
      i:= Integer(ModuleSymbol.FindLocalConst(aValue, aType));
      Result := i <> 0;
      if Result then
        AssignValue(PTurboConstSymbol(i));
    end;

    if not Result then
    Case aType.Kind of
      mtkString, mtkLString: 
      begin
        ValueStr := AnsiDequotedStr(aValue, aQuote);
        FValue.VInteger := 0;
      end;
      mtkChar:
      begin
        ValueStr := AnsiDequotedStr(aValue, aQuote);
        FValue.VByte := Ord(ValueStr[1]);
      end;
      mtkEnumeration:
      begin
        with PMeEnumerationType(aType)^ do
        begin
          i := NameList.IndexOf(aValue);
          Result := i >= 0;
          if Result then
          begin
            ValueStr := aValue;
            FValue.VInteger := NameList.Objects[i];
          end;
        end;
      end;
      mtkWChar:
      begin
        ValueStr := AnsiDequotedStr(aValue, aQuote);
        FValue.VWideChar := WideString(ValueStr)[1];
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
        ;
        Result := True;
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
        ;
        Result := True;
      except
        aType := nil;
        Result := False;
      end;

      if not Assigned(aType) and Assigned(ModuleSymbol) then
      begin
        i := ModuleSymbol.Consts.IndexOf(aValue);
        Result := i >= 0;
        if Result then
        begin
          ValueStr := aValue;
          AssignValue(PTurboConstSymbol(ModuleSymbol.Consts.Items[i]));
          aType := FTurboType;
        end;
      end; //}
  end;

  if Result and not Assigned(aType) then
  begin
    Result := False;
  end;
  if Result then TurboType := aType;
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

procedure TTurboConstSymbol.DeclareStringTo(const aModule: TCustomTurboModule);
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

procedure TTurboConstSymbol.PushTo(const aModule: TCustomTurboModule);
var
  p: Pointer;
begin
  if Assigned(TurboType) then
  begin
    Case TurboType.Kind of
      mtkLString, mtkString: if FValue.VInteger = 0 then DeclareStringTo(aModule);
      else
      begin
        if Size <= SizeOf(Integer) then
        begin
          //aModule.AddOpToMem(opPushInt);
          //Size := SizeOf(Integer);
          Case Size of
            SizeOf(Byte): aModule.AddOpToMem(opPushByte);
            SizeOf(Word): aModule.AddOpToMem(opPushWord);
            else 
            begin
              aModule.AddOpToMem(opPushInt);
              Size := SizeOf(tsInt);
            end;
          end;//case
        end
        else
          aModule.AddOpToMem(opPushInt64);
      end;
    end;
    Integer(p) := Integer(aModule.Memory) + aModule.UsedMemory;
    aModule.AllocSpace(Size);
    AssignValueTo(p);
  end;
end;

procedure TTurboConstSymbol.SetTurboType(const aValue: PMeType);
begin
  if FTurboType <> aValue then
  begin
    FTurboType := aValue;
    if Assigned(aValue) then
    begin
      if aValue.Kind = mtkString then //ShortString is Pointer too.
        FSize := SizeOf(tsPointer)
      else
        FSize := GetTypeSize(aValue);
    end
    else
      FSize := 0;
  end;
end;

{ TTurboVarSymbol }
procedure TTurboVarSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboVarSymbol(aSymbol)^ do
    begin
      Self.Addr := Addr;
    end;
  Inherited;
end;

procedure TTurboVarSymbol.DeclareTo(const aModule: TCustomTurboModule);
var
  vVaraibleEntry: PTurboStaticFieldEntry;
  vValue: Pointer;
  //vTypeSafety, vTypeNamed: Boolean;
begin
  vVaraibleEntry := nil;
  //vTypeSafety := (Visibility >= fvProtected) or (soSymbolTyped in aModule.Options); 
  //vTypeNamed := (Visibility >= fvPublished) or (soSymbolNamed in aModule.Options); 
  if IsPublic(aModule) then
  begin
    //aModule.AlignData;
    Integer(vVaraibleEntry) := Integer(aModule.DataMemory) + aModule.UsedDataSize;
    aModule.AllocDataSpace(SizeOf(TTurboStaticFieldEntry));
    vVaraibleEntry.Prior := aModule.LastVariableEntry;
    vVaraibleEntry.Variable.Size := Size;
    vVaraibleEntry.Variable.Addr := nil;
    vVaraibleEntry.Variable.TypeInfo := nil;
    vVaraibleEntry.Variable.Name := nil;
  end;

  if IsRequiredAlignMem(TurboType) then aModule.AlignData;
  Addr := aModule.UsedDataSize;

  if IsPublic(aModule) then
  begin
    tsInt(vVaraibleEntry.Variable.Addr) := Addr;
    aModule.LastVariableEntry := Pointer(Integer(vVaraibleEntry) - Integer(aModule.DataMemory));
  end;

  Integer(vValue) := Integer(aModule.DataMemory)  + Addr;
  aModule.AllocDataSpace(Size);
  if ValueStr <> '' then //该变量有初值.
  begin
    DeclareStringTo(aModule); //if the init value is string
    AssignValueTo(vValue);
  end;

  if IsNamed(aModule) then
  begin
      vVaraibleEntry.Variable.Name := Pointer(aModule.UsedDataSize);
      //fill the variable name
      //aModule.AddByteToData(Length(Name));
      aModule.AddBufferToData(Name[1], Length(Name));
      aModule.AddByteToData(0);
  end;
end;

procedure TTurboVarSymbol.PushTo(const aModule: TCustomTurboModule);
begin
  if Addr = 0 then DeclareTo(aModule);
  aModule.AddOpToMem(opPushInt);
  aModule.AddIntToMem(Addr);
end;

{ TTurboTypeSymbol }
destructor TTurboTypeSymbol.Destroy;
begin
  MeFreeAndNil(FiType);
  Inherited;
end;

procedure TTurboTypeSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboTypeSymbol(aSymbol)^ do
    begin
      Self.Entry := Entry;
      Self.FTypeInfo := FTypeInfo;
      Self.FiType := FiType;
    end;
  Inherited;
end;

procedure TTurboTypeSymbol.DeclareTo(const aModule: TCustomTurboModule);
begin
  if Assigned(FiType) and not Assigned(FTypeInfo) then
  begin
    if aModule.RegisteredTypes.RegisterType(FiType) then
    begin
      FTypeInfo:= FiType;
      FiType := nil;
    end;
  end;
end;

function TTurboTypeSymbol.GetTypeInfo(const aClass: TMeClass): PMeType;
begin
  if Assigned(FTypeInfo) then
    Result := FTypeInfo
  else if Assigned(FiType) then
    Result := FiType
  else if Assigned(aClass) and MeInheritsFrom(aClass, TypeOf(TMeType)) then
  begin
    Result := PMeType(NewMeObject(aClass));
    if Assigned(ModuleSymbol) and Assigned(ModuleSymbol.Module) then
      Result.Owner := ModuleSymbol.Module.RegisteredTypes;
  end
  else 
    Result := nil;
end;

{ TTurboTypeRefSymbol }
procedure TTurboTypeRefSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboTypeRefSymbol(aSymbol)^ do
    begin
      Self.RefModule := RefModule;
      Self.TypeId := TypeId;
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

{ TTurboModuleSymbols }
function TTurboModuleSymbols.GetItem(Index: Integer): PTurboModuleSymbol;
begin
  Result := PTurboModuleSymbol(Inherited Get(Index));
end;

function TTurboModuleSymbols.IndexOf(const aName: string; const aModuleType: TTurboModuleType; const aBeginIndex: Integer): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    with Items[Result]^ do
    if AnsiSameText(aName, Name) and (aModuleType = ModuleType) then
      exit;
  end;
  Result := -1;
end;

function TTurboModuleSymbols.IndexOf(const aModule: TCustomTurboModule; const aBeginIndex: Integer): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    with Items[Result]^ do
    if aModule = Module then
      exit;
  end;
  Result := -1;
end;

function TTurboModuleSymbols.IndexOf(const aModuleEntry: PTurboModuleRefEntry; const aBeginIndex: Integer): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    with Items[Result]^ do
    if aModuleEntry = Entry then
      exit;
  end;
  Result := -1;
end;

function TTurboModuleSymbols.Find(const aName: string; const aModuleType: TTurboModuleType): PTurboModuleSymbol;
var
  i: integer;
begin
  i := IndexOf(aName, aModuleType);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
end;

function TTurboModuleSymbols.Find(const aModule: TCustomTurboModule): PTurboModuleSymbol;
var
  i: integer;
begin
  i := IndexOf(aModule);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
end;

function TTurboModuleSymbols.Find(const aModuleEntry: PTurboModuleRefEntry): PTurboModuleSymbol;
var
  i: integer;
begin
  i := IndexOf(aModuleEntry);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
end;

function TTurboModuleSymbols.FindMethod(const aName: string; const aModuleType: TTurboModuleType): PTurboMethodSymbol;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  with Items[i]^ do
  begin
    if ModuleType = aModuleType then
    begin
      Result := FindLocalMethod(aName);
      if Assigned(Result) then exit;
    end;
  end;
  Result := nil;
end;

{ TTurboTypeSymbols }
function TTurboTypeSymbols.GetItem(Index: Integer): PTurboTypeSymbol;
begin
  Result := PTurboTypeSymbol(Inherited Get(Index));
end;

function TTurboTypeSymbols.IndexOf(const aType: PMeType; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    with Items[Result]^ do
    if aType = GetTypeInfo then
      exit;
  end;
  Result := -1;
end;

function TTurboTypeSymbols.IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    with Items[Result]^ do
    if AnsiSameText(aName, Name) then
      exit;
  end;
  Result := -1;
end;

function TTurboTypeSymbols.Find(const aName: string): PTurboTypeSymbol;
var
  i: integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
end;

function TTurboTypeSymbols.Find(const aType: PMeType): PTurboTypeSymbol;
var
  i: integer;
begin
  i := IndexOf(aType);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
end;

initialization
  SetMeVirtualMethod(TypeOf(TTurboCustomSymbol), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TTurboSymbol), ovtVmtParent, TypeOf(TTurboCustomSymbol));
  SetMeVirtualMethod(TypeOf(TTurboLabelSymbol), ovtVmtParent, TypeOf(TTurboCustomSymbol));
  SetMeVirtualMethod(TypeOf(TTurboConstSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboVarSymbol), ovtVmtParent, TypeOf(TTurboConstSymbol));
  SetMeVirtualMethod(TypeOf(TTurboMethodSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboModuleSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboTypeSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboTypeRefSymbol), ovtVmtParent, TypeOf(TTurboTypeSymbol));

  {$IFDEF MeRTTI_SUPPORT}
  //Make the ovtVmtClassName point to PShortString class name
  //SetMeVirtualMethod(TypeOf(TTurboCustomSymbol), ovtVmtClassName, @cMeObjectClassName);
  SetMeVirtualMethod(TypeOf(TTurboCustomSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboLabelSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboConstSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboVarSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboMethodSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboModuleSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboTypeSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboTypeRefSymbol), ovtVmtClassName, nil);
  {$ENDIF}
end.
