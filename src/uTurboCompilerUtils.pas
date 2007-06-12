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

Const
  cDotSeparatorSymbol = '.';
  //the internal Main Entry procedure name.
  cMainEntryProcName = '._Main';
  cSymbolErrorOk = 0;
  cSymbolErrorOwnerisNil = 313;
  cSymbolErrorUnknownCallStyle = 314;
  cSymbolErrorNoSpecifiedModule = 315;
  cSymbolErrorNoSpecifiedMethodAddr = 316;
  cSymbolErrorModuleRefAddedFailed = 317;
  cSymbolErrorRegisterType = 341;
  cSymbolErrorUnknownConstType = 311;

  cSymbolErrorLabelRedeclaration = 300;
  cSymbolErrorWordNameIsNil = 301;
  cSymbolErrorUnknownMethod = 302;
  cSymbolErrorVarRedeclaration = 303;
  cSymbolErrorConstRedeclaration = 304;
  cSymbolErrorRedeclaration = 305;
  cSymbolErrorUnknownModule = 306;
  cSymbolErrorFileNotFound  = 307;
  cSymbolErrorMethodNotFound  = 308;
  cSymbolErrorStrToIntCovnert = 309;
  cSymbolErrorConstDeclaration = 310;
  cSymbolErrorVarDeclaration = 320;
  cSymbolErrorMethodDeclaration = 330;
  cSymbolErrorTypeDeclaration = 340;
  cSymbolErrorLabelDeclaration = 350;

  cSymbolErrorInvalidOption = 400;
  cSymbolErrorInvalidOptionParam = 401;
  cSymbolErrorMessageCompilerOption = 500;

Type
  //the Compiler option state
  TTurboCompilerOptionState = (cosDefault, cosEnable, cosDisable);

  PTurboSymbols = ^ TTurboSymbols;
  PTurboModuleRefSymbols = ^ TTurboModuleRefSymbols;
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
  PTurboStatementSymbol = ^ TTurboStatementSymbol;
  PTurboOpStatementSymbol = ^ TTurboOpStatementSymbol;

  {: abstract Symbol }
  TTurboCustomSymbol = Object(TMeDynamicObject)
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
  public
    Name: String;
  end;

  {: abstract Statement Symbol }
  {
    Note: this would slow down the compiling speed.
  }
  TTurboStatementSymbol = Object(TTurboCustomSymbol)
  protected
    //to generate new VMT for it.
    //class function ParentClassAddress: TMeClass;virtual;abstract;
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
  public
    {: the OwnerSymbol owns this symbol. }
    OwnerSymbol: PTurboMethodSymbol;
  end;

  {: the VM Instruction Statement Symbol. }
  TTurboOpParamRec = packed record
    case integer of
      1: (i: tsInt; ii: tsInt);
      2: (i8: Int64);
      3: (s: single);
      4: (d: double);
      5: (c: Currency);
  end;

  TTurboOpStatementSymbol = Object(TTurboStatementSymbol)
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
  public
    Instruction: TTurboVMInstruction;
    Param: TTurboOpParamRec;
  end;

  TTurboSymbol = Object(TTurboCustomSymbol)
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    function IsPublic(const aModule: TCustomTurboModule): Boolean;
    function IsTyped(const aModule: TCustomTurboModule): Boolean;
    function IsNamed(const aModule: TCustomTurboModule): Boolean;
  public
    Visibility: TTurboVisibility;
    {: the OwnerSymbol owns this symbol. }
    OwnerSymbol: PTurboModuleSymbol;
  end;

  TTurboLabelSymbol = object(TTurboCustomSymbol)
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    function DeclareTo(const aModule: PTurboModuleSymbol): Integer;
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
    function PushTo(const aModule: PTurboModuleSymbol): Integer; virtual;
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
    function DeclareTo(const aModule: PTurboModuleSymbol): Integer;
    //generate the op-code push variable addr to the aModule
    function PushTo(const aModule: PTurboModuleSymbol): Integer; virtual; {override}
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
    function GetExternalLocalMethodAddr(): tsInt;
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    //generate the call op-code to the aModule
    function PushTo(const aModule: PTurboModuleSymbol): Integer; virtual; {override}
    function DeclareTo(const aModule: PTurboModuleSymbol): Integer;
    //for local method to push the opReturn instruction.
    procedure DeclareEndTo(const aModule: PTurboModuleSymbol);
    function DeclareLabelTo(const aLabelName: string; const aModule: PTurboModuleSymbol): Integer;
    function FindLabel(const aName: string): PTurboLabelSymbol;
  public
    //Options: TTurboWordOptions;
    CallStyle: TCallingConvention;
    CodeFieldStyle: TTurboCodeFieldStyle;
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord; //this is in the Entry.
    CFA: tsInt;//the offset address of the memory. -1 means not declare to Module.Memory
    ModuleName: String;  //for external word
    ModuleType: TTurboModuleType;  //for external word
    //Module: TCustomTurboModule; //for external forth word
    ModuleSymbol: PTurboModuleSymbol;//for external forth word, the word is in the ModuleSymbol
    ExternalMethodSymbol: PTurboMethodSymbol;
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
    function DeclareTo(const aModule: PTurboModuleSymbol): Integer;
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
    FUsedModules: PTurboModuleRefSymbols;
  protected
    function GetTypeRefs: PTurboTypeSymbols;
    function GetTypes: PTurboTypeSymbols;
    function GetVars: PTurboSymbols;
    function GetConsts: PTurboSymbols;
    function GetMethods: PTurboSymbols;
    function GetUsedModules: PTurboModuleRefSymbols;
    function CreateLibModuleSymbol(const aName: string): PTurboModuleSymbol;
    //function GetModuleEntry(const aModule: PTurboModuleSymbol): PTurboModuleRefEntry;
  public
    destructor Destroy; virtual; {override}
    procedure Clear;
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual;
    function FindType(const aName: string): PMeType;
    //function FindLocalType(const aName: string): PMeType;
    function FindLocalTypeSymbol(const aName: string): PTurboTypeSymbol;
    function FindLocalMethodSymbol(const aName: string): PTurboMethodSymbol;
    function FindLocalConstSymbol(const aName: string; const aType: PMeType): PTurboConstSymbol;
    //find all method in all place.
    function FindMethodSymbol(const aName: string): PTurboMethodSymbol;
    function AddUsedModule(const aName: String; const aModuleType: TTurboModuleType): Integer;
    //将外部的TSLib方法添加到本地的Methods方法列表中，注意：没有写入内存(DeclareTo)。
    function AddExternalLibMethod(const aMethod: PTurboMethodSymbol): PTurboMethodSymbol;
    function DeclareTo(const aModule: PTurboModuleSymbol): Integer;
    function IsUniqueIdentifier(const aName: String): Boolean;
    procedure PushInt32(const aStr: string);overload;
    procedure PushInt32(const aInt: tsInt);overload;

    //Create a new constant
    function NewConst(const aName: string): PTurboConstSymbol;
    function NewVar(const aName: string): PTurboVarSymbol;
    function NewType(const aName: string): PTurboTypeSymbol;
    function NewMethod(const aName: string): PTurboMethodSymbol;
    //if true then apply external param to the aMethod
    function IsExternalMethod(const aMethod: PTurboMethodSymbol): Integer;
    //当带常量的变量有动态数组，AnsiString 的时候需要在模块的程序头添加初始化过程：对变量分配内存，并将常量赋值给变量；在程序尾添加终止过程，释放变量内存，并最后加上halt指令。
    //procedure DoInitModuleProc;
    //procedure DoFinalModuleProc;
  public
    //Entry: PTurboModuleRefEntry;
    ModuleType: TTurboModuleType;
    Module: TCustomTurboModule;
    property Types: PTurboTypeSymbols read GetTypes;
    property TypeRefs: PTurboTypeSymbols read GetTypeRefs;
    property Vars: PTurboSymbols read GetVars;
    property Consts: PTurboSymbols read GetConsts;
    property Methods: PTurboSymbols read GetMethods;
    property UsedModules: PTurboModuleRefSymbols read GetUsedModules;
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

  TTurboModuleRefSymbols = object(TTurboSymbols)
  protected
    //the module entry list(PTurboModuleRefEntry).
    FEntries: PMeList;
  protected
    function GetItem(Index: Integer): PTurboModuleSymbol;
    function GetEntry(Index: Integer): PTurboModuleRefEntry;
    procedure Init; virtual; {override}
  public
    destructor Destroy; virtual; {override}
    function Add(const aModuleSymbol: PTurboModuleSymbol): Integer;
    procedure Assign(const aSrc: PTurboModuleRefSymbols);
    procedure Delete(const Index: Integer);
    procedure Swap(const Idx1, Idx2: Integer);
    procedure Remove(const Value: PTurboModuleSymbol);
    procedure Move(const CurIndex, NewIndex: Integer);
    function IndexOf(const aModuleSymbol: PTurboModuleSymbol; const aBeginIndex: Integer = 0): Integer; overload;
    function IndexOf(const aModule: TCustomTurboModule; const aBeginIndex: Integer = 0): Integer; overload;
    function IndexOf(const aModuleEntry: PTurboModuleRefEntry; const aBeginIndex: Integer = 0): Integer; overload;
    function IndexOf(const aName: string; const aModuleType: TTurboModuleType; const aBeginIndex: Integer = 0): Integer; overload;
    function Find(const aName: string; const aModuleType: TTurboModuleType): PTurboModuleSymbol;overload;
    function Find(const aModule: TCustomTurboModule): PTurboModuleSymbol;overload;
    function Find(const aModuleEntry: PTurboModuleRefEntry): PTurboModuleSymbol;overload;
    function FindMethodSymbol(const aName: string; const aModuleType: TTurboModuleType = mtLib): PTurboMethodSymbol;
    function FindTypeSymbol(const aName: string): PTurboTypeSymbol;
  public
    OwnerSymbol: PTurboModuleSymbol;
    property Entries[Index: Integer]: PTurboModuleRefEntry read GetEntry;
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
        if aValue[i] = cDotSeparatorSymbol then
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
      Self.OwnerSymbol := OwnerSymbol;
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

function TTurboLabelSymbol.DeclareTo(const aModule: PTurboModuleSymbol): integer;
begin
  if Addr = 0 then
  begin
    Addr := aModule.Module.UsedMemory;
    Result := cSymbolErrorOk;
  end
  else
    Result := cSymbolErrorRedeclaration;
end;

{ TTurboMethodSymbol }
procedure TTurboMethodSymbol.Init;
begin
  Inherited;
  CFA := -1;
  ExternalOptions.Index := -1;
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
      Self.CFA:= CFA;
      Self.ModuleType:=ModuleType;
      Self.ModuleSymbol := ModuleSymbol;
      Self.ExternalMethodSymbol := ExternalMethodSymbol;
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
  else if Assigned(ExternalMethodSymbol) then
  begin
    Result := ExternalMethodSymbol.TypeSymbol;
  end
  else
  begin
    //no Assigned, Create a one.
    New(FiTypeSymbol, Create);
    FiTypeSymbol.OwnerSymbol := OwnerSymbol;
    Result := FiTypeSymbol;
    //Result.GetTypeInfo(TypeOf(TMeProcType));
  end;
end;

function TTurboMethodSymbol.GetTypeInfo: PMeProcType;
begin
  Result := PMeProcType(TypeSymbol.GetTypeInfo(TypeOf(TMeProcType)));
end;

function TTurboMethodSymbol.GetExternalLocalMethodAddr(): tsInt;
begin
  Result := -1;
  if Assigned(ExternalMethodSymbol) then
  begin
    Result := ExternalMethodSymbol.CFA;
  end
  else if Assigned(ModuleSymbol) then
  begin
    if ExternalOptions.Name <> '' then
      ExternalMethodSymbol := ModuleSymbol.FindLocalMethodSymbol(ExternalOptions.Name)
    else
      ExternalMethodSymbol := ModuleSymbol.FindLocalMethodSymbol(Name);
    if Assigned(ExternalMethodSymbol) then
    begin
      Result := ExternalMethodSymbol.CFA;
    end;
  end;
end;

function TTurboMethodSymbol.DeclareTo(const aModule: PTurboModuleSymbol): Integer;
var
  p: PTurboExteralMethodOptions;
begin
  if CFA = -1 then
  begin
    Result := cSymbolErrorOk;
    Case CodeFieldStyle of
      cfsFunction:
        begin
          CFA := aModule.Module.UsedMemory;
        end;
      cfsExternalFunction: //the external forth function
        begin
          CFA := GetExternalLocalMethodAddr();
          if CFA = -1 then
            Result := cSymbolErrorUnknownMethod;

          if (Result = cSymbolErrorOk) and not Assigned(ExternalOptions.ModuleRef) then
          begin
            Result := ModuleSymbol.DeclareTo(aModule);
            if Result <> cSymbolErrorOk then Exit;
            Integer(p) := aModule.UsedModules.IndexOf(ModuleSymbol);
            if Integer(p) >= 0 then
            begin
              ExternalOptions.ModuleRef := Pointer(Integer(aModule.UsedModules.Entries[Integer(p)]) + SizeOf(tsPointer));
            end
            else
              Result := cSymbolErrorModuleRefAddedFailed;
          end;
          if Result <> cSymbolErrorOk then Exit;
        end;
    end; //case
    if IsPublic(aModule.Module) then with aModule.Module do
    begin
      //writeln('aModule.UsedDataSize=',InttoHex(aModule.UsedDataSize,4));
      AlignData;
      //writeln('aModule.UsedDataSize=',InttoHex(aModule.UsedDataSize,4));
      Integer(Entry) := Integer(DataMemory) + UsedDataSize;
      AllocDataSpace(SizeOf(TTurboMethodEntry));
      Entry.Prior := LastWordEntry;
      if IsNamed(aModule.Module) then
      begin
        Entry.Word.Name := Pointer(UsedDataSize);
        //PChar:
        AddBufferToData(Self.Name[1], Length(Self.Name));
        AddByteToData(0);
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
      if Entry.Word.IsExternal then
      begin
        Integer(p) := Integer(DataMemory) + UsedDataSize;
        AllocDataSpace(SizeOf(TTurboExteralMethodOptions));
        p^ := ExternalOptions;
        Assert(p=Entry.Word.GetExternalOptionsAddr, 'Method.DeclareTo: ExternalOptionsAddr mismatch');
      end;
    end;
  end
  else
    Result := cSymbolErrorRedeclaration;
end;

procedure TTurboMethodSymbol.DeclareEndTo(const aModule: PTurboModuleSymbol);
var
  vIsPublic: Boolean;
begin
  if (CodeFieldStyle = cfsFunction) and (CFA <> -1) then with aModule.Module do
  begin
    vIsPublic := IsPublic(aModule.Module);
    
    if Self.Name = cMainEntryProcName then
    begin
      //writeln('DeclareEndTo:',cMainEntryProcName );
      AddOpToMem(opPushInt); AddIntToMem(0); 
      AddOpToMem(opHalt);
    end
    else if vIsPublic then
      AddOpToMem(opExitFar)
    else
      AddOpToMem(opExit);
    ParamFieldLength := UsedMemory - CFA + 1;

    if vIsPublic then
    begin
      Entry.Word.ParamFieldLength := ParamFieldLength;
      LastWordEntry := Pointer(Integer(Entry) - Integer(DataMemory));
    end;
  end;
end;

function TTurboMethodSymbol.DeclareLabelTo(const aLabelName: string; const aModule: PTurboModuleSymbol): Integer;
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

function TTurboMethodSymbol.PushTo(const aModule: PTurboModuleSymbol): Integer;
var
  vCFA: tsInt;
  i: Integer;
begin
  Result := cSymbolErrorOk;
  case CodeFieldStyle of
    cfsFunction:
      begin
        if CFA = -1 then
        begin
          Result := cSymbolErrorNoSpecifiedMethodAddr;
          Exit;
        end;
        with aModule.Module do if IsPublic(aModule.Module) then
        begin
          AddOpToMem(opEnterFar);
          AddIntToMem(0);
          AddIntToMem(CFA);
        end
        else
        begin
          AddOpToMem(opEnter);
          AddIntToMem(CFA);
        end;
      end;
    cfsExternalFunction: //the external TurboScript Lib Function.
      begin
        vCFA := CFA;
        if vCFA = -1 then
        begin
          vCFA := GetExternalLocalMethodAddr();
        end;
        if vCFA <> -1 then
        begin
          aModule.Module.AddOpToMem(opCallFar);
          //writeln(Name, '.ModEntry:',Integer(OwnerSymbol.Entry)- Integer(aModule.DataMemory));
          //point to the TurboModuleInfo
          //TODO: BUG ExternalOptions.ModuleRef if the DeclareTo(Module) is not same as the PushTo(Module)
          //      the ExternalOptions.ModuleRef should put into Module too.
          if not Assigned(ExternalOptions.ModuleRef) then
          begin
            ModuleSymbol.DeclareTo(aModule);
            i := aModule.UsedModules.IndexOf(ModuleSymbol);
            ExternalOptions.ModuleRef := Pointer(Integer(aModule.UsedModules.Entries[i]) + SizeOf(tsPointer));
          end;
          //writeln(Name, '.ModRef:',Integer(ExternalOptions.ModuleRef) - Integer(aModule.DataMemory));
          aModule.Module.AddIntToMem(Integer(ExternalOptions.ModuleRef) - Integer(aModule.Module.DataMemory));
          aModule.Module.AddIntToMem(vCFA);
          //writeln(Name, '.CFA:',vCFA);
        end
        else
          Result := cSymbolErrorUnknownMethod;
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
      //Self.Entry:= Entry;
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
      begin
        Self.UsedModules.Assign(FUsedModules);
      end
      else 
      begin
        MeFreeAndNil(Self.FUsedModules);
        //MeFreeAndNil(Self.FUsedModuleEntries);
      end;
    end;
  Inherited;
end;

procedure TTurboModuleSymbol.Clear;
begin
  MeFreeAndNil(FMethods);
  MeFreeAndNil(FUsedModules);
  //MeFreeAndNil(FUsedModuleEntries);
  MeFreeAndNil(FVars);
  MeFreeAndNil(FConsts);
  MeFreeAndNil(FTypes);
  MeFreeAndNil(FTypeRefs);
end;

function TTurboModuleSymbol.FindType(const aName: string): PMeType;
begin
  //find local type:
  Result := PMeType(FindLocalTypeSymbol(aName));
  if Assigned(Result) then
    Result := PTurboTypeSymbol(Result).GetTypeInfo;
  if not Assigned(Result) and Assigned(FUsedModules) then
  begin // find external type:
    Result := PMeType(FUsedModules.FindTypeSymbol(aName));
    if Assigned(Result) then
      Result := PTurboTypeSymbol(Result).GetTypeInfo;
  end;

  if not Assigned(Result) then
  begin //find internal type:
    Result := GRegisteredTypes.GetRegisteredTypeByName(aName);
  end;
end;

function TTurboModuleSymbol.FindLocalTypeSymbol(const aName: string): PTurboTypeSymbol;
Var
  vType: PMeType;
begin
  //find local type:
  Result := Types.Find(aName);
  if not Assigned(Result) and Assigned(Module) then
  begin //search on the module:
    vType := Module.RegisteredTypes.FindLocalType(aName);
    if Assigned(vType) then
    begin
      //add to Types cache
      Result := NewType(aName);
      Result.FTypeInfo := vType;
    end;
  end;
end;

function TTurboModuleSymbol.FindLocalConstSymbol(const aName: string; const aType: PMeType): PTurboConstSymbol;
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

function TTurboModuleSymbol.FindLocalMethodSymbol(const aName: string): PTurboMethodSymbol;
var
  vMethodEntry: PTurboMethodEntry;
begin
  Result := PTurboMethodSymbol(Methods.Find(aName));
  if not Assigned(Result) and Assigned(Module) then
  begin
    //Integer(vMethodEntry) := Integer(Module.LastWordEntry) + Integer(Module.DataMemory);
    //writeln('LastWordEntry:', PChar(Integer(Module.DataMemory)+Integer(vMethodEntry.Word.Name)));
    
    vMethodEntry := Module.FindWordEntry(aName, cfsFunction);
    if Assigned(vMethodEntry) and (vMethodEntry.Word.CodeFieldStyle = cfsFunction) then
    begin
        New(Result, Create);
        Result.Name := aName;
        Result.CFA := vMethodEntry.Word.MethodAddr;
        //Result.ExternalOptions.ModuleRef := Pointer(Integer(Entry) + SizeOf(Pointer));
        Result.CallStyle := vMethodEntry.Word.CallStyle;
        Result.CodeFieldStyle := vMethodEntry.Word.CodeFieldStyle;
        Result.ModuleType := mtLib;
        Result.ModuleName := Name;
        //Result.ModuleSymbol := @Self;
        //Result.Module := Module;
        Result.OwnerSymbol := @Self;
        Result.Entry := vMethodEntry;
        Methods.Add(Result); //cache this method!
    end;
  end;
end;

function TTurboModuleSymbol.FindMethodSymbol(const aName: string): PTurboMethodSymbol;
var
  i: Integer;
begin
  i := Methods.IndexOf(aName);
  if i >= 0 then
    Result := PTurboMethodSymbol(Methods.Items[i])
  else begin
    Result := UsedModules.FindMethodSymbol(aName);
    if Assigned(Result) then
    begin
      i := Methods.IndexOf(Result.ModuleName + cDotSeparatorSymbol + aName);
      if i >= 0 then
        Result := PTurboMethodSymbol(Methods.Items[i])
      else
        Result := AddExternalLibMethod(Result);
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

function TTurboModuleSymbol.GetUsedModules: PTurboModuleRefSymbols;
begin
  if not Assigned(FUsedModules) then
  begin
    New(FUsedModules, Create);
    FUsedModules.OwnerSymbol := @Self;
    //New(FUsedModuleEntries, Create);
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

function TTurboModuleSymbol.IsExternalMethod(const aMethod: PTurboMethodSymbol): Integer;
var
  i: Integer;
  vModuleSymbol: PTurboModuleSymbol;
  vModuleEntry: PTurboModuleRefEntry;
begin
  Result := cSymbolErrorOk;
  vModuleEntry := nil;
  //try to determine the module of the aMethod.
  if aMethod.ModuleName = '' then
  begin
    i := UsedModules.Count -1;
    if i >= 0 then
    begin
      //aMethod.ModuleSymbol := UsedModules.Items[i];
      vModuleSymbol := UsedModules.Items[i];
      aMethod.ModuleName := vModuleSymbol.Name;
      vModuleEntry := UsedModules.Entries[i];
    end
    else
    begin 
      vModuleSymbol := nil;
      Result := cSymbolErrorNoSpecifiedModule;
      //SynError(cDLLModuleMissError, aWord.Name);
    end;
  end
  else
  begin
    i := UsedModules.IndexOf(aMethod.ModuleName, aMethod.ModuleType);
    if i < 0 then i := AddUsedModule(aMethod.ModuleName, aMethod.ModuleType);
    if i >= 0 then
    begin
      vModuleSymbol := UsedModules.Items[i];
      vModuleEntry := UsedModules.Entries[i];
    end
    else
    begin 
      //writeln('Can not add used module:' + aMethod.ModuleName);
      //SynError(cDLLModuleMissError, 'Can not add used module:' + aWord.ModuleName);
      vModuleSymbol := nil;
      Result := cSymbolErrorUnknownModule;
    end;
  end;

  aMethod.ModuleSymbol := vModuleSymbol;
  if Assigned(vModuleSymbol) then
  begin
    if aMethod.ExternalOptions.Name <> '' then
      aMethod.ExternalMethodSymbol := vModuleSymbol.FindLocalMethodSymbol(aMethod.ExternalOptions.Name)
    else
      aMethod.ExternalMethodSymbol := vModuleSymbol.FindLocalMethodSymbol(aMethod.Name);

    if not Assigned(aMethod.ExternalMethodSymbol) then
      Result := cSymbolErrorUnknownMethod
    else if Assigned(vModuleEntry) then
    begin
      aMethod.ExternalOptions.ModuleRef := Pointer(Integer(vModuleEntry) + SizeOf(tsPointer));
    end
    else
      aMethod.ExternalOptions.ModuleRef := nil;
  end
  else
  begin
    aMethod.ExternalMethodSymbol := nil;
    Result := cSymbolErrorUnknownModule;
  end;
end;

function TTurboModuleSymbol.AddExternalLibMethod(const aMethod: PTurboMethodSymbol): PTurboMethodSymbol;
begin
  if Assigned(aMethod) then
  begin
    Result := NewMethod(aMethod.ModuleName + cDotSeparatorSymbol + aMethod.Name);
    if Assigned(Result) then
    begin
      Result.ExternalMethodSymbol := aMethod;
      Result.ModuleSymbol := aMethod.OwnerSymbol;
      Result.CallStyle := aMethod.CallStyle;
      Result.CodeFieldStyle := cfsExternalFunction;
    end;
  end
  else
    Result := nil;
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

function TTurboModuleSymbol.DeclareTo(const aModule: PTurboModuleSymbol): Integer;
var
  i: Integer;
  vModuleEntry: PTurboModuleRefEntry;
begin
  Result := cSymbolErrorOk;
  i := aModule.UsedModules.IndexOf(PTurboModuleSymbol(@Self));
  if i < 0 then
  begin
    i := aModule.UsedModules.Add(@Self);
  end;
  if i >= 0 then
  begin
    vModuleEntry := aModule.UsedModules.Entries[i];
  end
  else
  begin
    Result := cSymbolErrorModuleRefAddedFailed;
    Exit;
  end;
  if not Assigned(vModuleEntry) then
  begin
    with aModule.Module do
    begin
      Integer(vModuleEntry) := Integer(DataMemory) + UsedDataSize; //the offset address.
      AllocDataSpace(SizeOf(TTurboModuleRefEntry));
      vModuleEntry.Prior := LastModuleRefEntry;
      vModuleEntry.Module.ModuleType := ModuleType;
      vModuleEntry.Module.Revision := Module.ModuleVersion;
      vModuleEntry.Module.BuildDate := Module.ModuleDate;
      vModuleEntry.Module.Name := Pointer(UsedDataSize);
      if Length(Self.Name) > 0 then
        AddBufferToData(Self.Name[1], Length(Self.Name));
      AddByteToData(0);
      LastModuleRefEntry := Pointer(tsInt(vModuleEntry) - Integer(DataMemory));
    end; //with
    aModule.UsedModules.FEntries.List^[i] := vModuleEntry;
  end
  else
    Result := cSymbolErrorRedeclaration;
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
    Result.OwnerSymbol := @Self;
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
    Result.OwnerSymbol := @Self;
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
    Result.OwnerSymbol := @Self;
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
    Result.OwnerSymbol := @Self;
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
    Result := Assigned(OwnerSymbol);
    if Result then
    begin
      i:= Integer(OwnerSymbol.FindLocalConstSymbol(aValue, aType));
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

      if not Assigned(aType) and Assigned(OwnerSymbol) then
      begin
        i := OwnerSymbol.Consts.IndexOf(aValue);
        Result := i >= 0;
        if Result then
        begin
          ValueStr := aValue;
          AssignValue(PTurboConstSymbol(OwnerSymbol.Consts.Items[i]));
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

function TTurboConstSymbol.PushTo(const aModule: PTurboModuleSymbol): Integer;
var
  p: Pointer;
begin
  if Assigned(TurboType) then
  begin
    Case TurboType.Kind of
      mtkLString, mtkString: if FValue.VInteger = 0 then DeclareStringTo(aModule.Module);
      else
      begin
        if Size <= SizeOf(Integer) then
        begin
          //aModule.AddOpToMem(opPushInt);
          //Size := SizeOf(Integer);
          Case Size of
            SizeOf(Byte): aModule.Module.AddOpToMem(opPushByte);
            SizeOf(Word): aModule.Module.AddOpToMem(opPushWord);
            else 
            begin
              aModule.Module.AddOpToMem(opPushInt);
              Size := SizeOf(tsInt);
            end;
          end;//case
        end
        else
          aModule.Module.AddOpToMem(opPushInt64);
      end;
    end;
    Integer(p) := Integer(aModule.Module.Memory) + aModule.Module.UsedMemory;
    aModule.Module.AllocSpace(Size);
    AssignValueTo(p);
    Result := cSymbolErrorOk;
  end
  else
    Result := cSymbolErrorUnknownConstType;
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

function TTurboVarSymbol.DeclareTo(const aModule: PTurboModuleSymbol): Integer;
var
  vVaraibleEntry: PTurboStaticFieldEntry;
  vValue: Pointer;
  //vTypeSafety, vTypeNamed: Boolean;
begin
  if Addr <> 0 then 
  begin
    Result := cSymbolErrorRedeclaration;
    exit;
  end;
  Result := cSymbolErrorOk;
  vVaraibleEntry := nil;
  //vTypeSafety := (Visibility >= fvProtected) or (soSymbolTyped in aModule.Options); 
  //vTypeNamed := (Visibility >= fvPublished) or (soSymbolNamed in aModule.Options); 
  if IsPublic(aModule.Module) then with aModule.Module do
  begin
    //aModule.AlignData;
    Integer(vVaraibleEntry) := Integer(DataMemory) + UsedDataSize;
    AllocDataSpace(SizeOf(TTurboStaticFieldEntry));
    vVaraibleEntry.Prior := LastVariableEntry;
    vVaraibleEntry.Variable.Size := Size;
    vVaraibleEntry.Variable.Addr := nil;
    vVaraibleEntry.Variable.TypeInfo := nil;
    vVaraibleEntry.Variable.Name := nil;
  end;

  if IsRequiredAlignMem(TurboType) then aModule.Module.AlignData;
  Addr := aModule.Module.UsedDataSize;

  if IsPublic(aModule.Module) then
  begin
    tsInt(vVaraibleEntry.Variable.Addr) := Addr;
    aModule.Module.LastVariableEntry := Pointer(Integer(vVaraibleEntry) - Integer(aModule.Module.DataMemory));
  end;

  Integer(vValue) := Integer(aModule.Module.DataMemory)  + Addr;
  aModule.Module.AllocDataSpace(Size);
  if ValueStr <> '' then //该变量有初值.
  begin
    DeclareStringTo(aModule.Module); //if the init value is string
    AssignValueTo(vValue);
  end;

  if IsNamed(aModule.Module) then with aModule.Module do
  begin
      vVaraibleEntry.Variable.Name := Pointer(UsedDataSize);
      //fill the variable name
      //aModule.AddByteToData(Length(Name));
      AddBufferToData(Name[1], Length(Name));
      AddByteToData(0);
  end;
end;

function TTurboVarSymbol.PushTo(const aModule: PTurboModuleSymbol): Integer;
begin
  if Addr = 0 then
  begin
    Result := DeclareTo(aModule);
    if Result <> cSymbolErrorOk then exit;
  end
  else 
    Result := cSymbolErrorOk;
  aModule.Module.AddOpToMem(opPushInt);
  aModule.Module.AddIntToMem(Addr);
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

function TTurboTypeSymbol.DeclareTo(const aModule: PTurboModuleSymbol): Integer;
begin
  if Assigned(FiType) and not Assigned(FTypeInfo) then
  begin
    if aModule.Module.RegisteredTypes.RegisterType(FiType) then
    begin
      FTypeInfo:= FiType;
      FiType := nil;
      Result := cSymbolErrorOk;
    end
    else
      Result := cSymbolErrorRegisterType;
  end
  else
    Result := cSymbolErrorRedeclaration;
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
    if Assigned(OwnerSymbol) and Assigned(OwnerSymbol.Module) then
      Result.Owner := OwnerSymbol.Module.RegisteredTypes;
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

{ TTurboStatementSymbol }
procedure TTurboStatementSymbol.Assign(const aSymbol: PTurboCustomSymbol); 
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboStatementSymbol(aSymbol)^ do
    begin
      Self.OwnerSymbol := OwnerSymbol;
    end;
  Inherited;
end;

{ TTurboOpStatementSymbol }
procedure TTurboOpStatementSymbol.Assign(const aSymbol: PTurboCustomSymbol); 
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(Self.ClassType) then
    with PTurboOpStatementSymbol(aSymbol)^ do
    begin
      Self.Instruction := Instruction;
      Self.Param := Param;
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

{ TTurboModuleRefSymbols }
procedure TTurboModuleRefSymbols.Init;
begin
  Inherited;
  New(FEntries, Create);
end;

destructor TTurboModuleRefSymbols.Destroy; 
begin
  MeFreeAndNil(FEntries);
  Inherited;
end;

procedure TTurboModuleRefSymbols.Assign(const aSrc: PTurboModuleRefSymbols);
begin
  if Assigned(aSrc) then
  begin
    Inherited Assign(aSrc);
    FEntries.Assign(aSrc.FEntries);
  end;
end;

function TTurboModuleRefSymbols.Add(const aModuleSymbol: PTurboModuleSymbol): Integer;
begin
  Result := Inherited Add(aModuleSymbol);
  if (Result >= 0) then
  begin
    FEntries.Add(nil);
  end;
end;

procedure TTurboModuleRefSymbols.Delete(const Index: Integer);
begin
  Inherited Delete(Index);
  FEntries.Delete(Index);
end;

procedure TTurboModuleRefSymbols.Swap(const Idx1, Idx2: Integer);
begin
  Inherited Swap(Idx1, Idx2);
  FEntries.Swap(Idx1, Idx2);
end;

procedure TTurboModuleRefSymbols.Move(const CurIndex, NewIndex: Integer);
begin
  Inherited Move(CurIndex, NewIndex);
  FEntries.Move(CurIndex, NewIndex);
end;

procedure TTurboModuleRefSymbols.Remove(const Value: PTurboModuleSymbol);
var
  i: Integer;
begin
  i:= IndexOf(Value);
  if i >= 0 then
  begin
    Delete(i);
  end;
end;

function TTurboModuleRefSymbols.GetEntry(Index: Integer): PTurboModuleRefEntry;
begin
  Result := PTurboModuleRefEntry(FEntries.Items[Index]);
end;

function TTurboModuleRefSymbols.GetItem(Index: Integer): PTurboModuleSymbol;
begin
  Result := PTurboModuleSymbol(Inherited Get(Index));
end;

function TTurboModuleRefSymbols.IndexOf(const aName: string; const aModuleType: TTurboModuleType; const aBeginIndex: Integer): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    with Items[Result]^ do
    if AnsiSameText(aName, Name) and (aModuleType = ModuleType) then
      exit;
  end;
  Result := -1;
end;

function TTurboModuleRefSymbols.IndexOf(const aModule: TCustomTurboModule; const aBeginIndex: Integer): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    with Items[Result]^ do
    if aModule = Module then
      exit;
  end;
  Result := -1;
end;

function TTurboModuleRefSymbols.IndexOf(const aModuleSymbol: PTurboModuleSymbol; const aBeginIndex: Integer = 0): Integer; 
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if List^[Result] = aModuleSymbol then
      exit;
  end;
  Result := -1;
end;

function TTurboModuleRefSymbols.IndexOf(const aModuleEntry: PTurboModuleRefEntry; const aBeginIndex: Integer): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if aModuleEntry = Entries[Result] then
      exit;
  end;
  Result := -1;
end;

function TTurboModuleRefSymbols.Find(const aName: string; const aModuleType: TTurboModuleType): PTurboModuleSymbol;
var
  i: integer;
begin
  i := IndexOf(aName, aModuleType);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
end;

function TTurboModuleRefSymbols.Find(const aModule: TCustomTurboModule): PTurboModuleSymbol;
var
  i: integer;
begin
  i := IndexOf(aModule);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
end;

function TTurboModuleRefSymbols.Find(const aModuleEntry: PTurboModuleRefEntry): PTurboModuleSymbol;
var
  i: integer;
begin
  i := IndexOf(aModuleEntry);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
end;

function TTurboModuleRefSymbols.FindMethodSymbol(const aName: string; const aModuleType: TTurboModuleType): PTurboMethodSymbol;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  with Items[i]^ do
  begin
    if ModuleType = aModuleType then
    begin
      Result := FindLocalMethodSymbol(aName);
      if Assigned(Result) then exit;
    end;
  end;
  Result := nil;
end;

function TTurboModuleRefSymbols.FindTypeSymbol(const aName: string): PTurboTypeSymbol;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  with Items[i]^ do
  begin
    Result := FindLocalTypeSymbol(aName);
    if Assigned(Result) then exit;
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

  SetMeVirtualMethod(TypeOf(TTurboStatementSymbol), ovtVmtParent, TypeOf(TTurboCustomSymbol));
  SetMeVirtualMethod(TypeOf(TTurboOpStatementSymbol), ovtVmtParent, TypeOf(TTurboStatementSymbol));

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

  SetMeVirtualMethod(TypeOf(TTurboStatementSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboOpStatementSymbol), ovtVmtClassName, nil);
  {$ENDIF}
end.
