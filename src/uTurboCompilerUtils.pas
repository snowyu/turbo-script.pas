unit uTurboCompilerUtils;

interface

{$I MeSetting.inc}
{$I TurboScript.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeTypes
  , uMeProcType
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboExecutor
  , uTurboCustomSymbol
  ;

Const
  cDotSeparatorSymbol = '.';
  //the internal Main Entry procedure name.
  cMainEntryProcName = '._Main';
  cSymbolErrorOwnerisNil = 313;
  cSymbolErrorUnknownCallStyle = 314;
  cSymbolErrorNoSpecifiedModule = 315;
  cSymbolErrorNoSpecifiedMethodAddr = 316;
  cSymbolErrorModuleRefAddedFailed = 317;
  cSymbolErrorRegisterType = 341;
  cSymbolErrorUnknownConstType = 311;
  cSymbolErrorUnknownRef = 313;

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
  cSymbolErrorMethodBodyNotFine = 331;
  cSymbolErrorMethodExternalNoBody = 332; //the external function is no function body part.
  cSymbolErrorTypeDeclaration = 340;
  cSymbolErrorLabelDeclaration = 350;

  cSymbolErrorInvalidOption = 400;
  cSymbolErrorInvalidOptionParam = 401;
  cSymbolErrorMessageCompilerOption = 500;

Type
  PTurboModuleRefSymbols = ^ TTurboModuleRefSymbols;
  PTurboSymbol = ^ TTurboSymbol;
  PTurboTypeSymbols = ^ TTurboTypeSymbols;
  PTurboLabelSymbol = ^ TTurboLabelSymbol;
  PTurboConstSymbol = ^TTurboConstSymbol;
  PTurboVariableSymbol = ^ TTurboVariableSymbol;
  PTurboCustomBlockSymbol = ^ TTurboCustomBlockSymbol;
  PTurboCustomMethodSymbol = ^ TTurboCustomMethodSymbol;
  PTurboMethodSymbol = ^ TTurboMethodSymbol;
  PTurboMethodRefSymbol = ^ TTurboMethodRefSymbol;
  PTurboTypeSymbol = ^ TTurboTypeSymbol;
  PTurboTypeRefSymbol = ^ TTurboTypeRefSymbol;
  PTurboModuleSymbol = ^ TTurboModuleSymbol;
  PTurboStatementSymbol = ^ TTurboStatementSymbol;
  PTurboOpStatementSymbol = ^ TTurboOpStatementSymbol;

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
  protected
    procedure CompileError(const aErrCode: Integer);
    function IsCompileAllowed: Boolean; virtual; {override}
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
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
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
    function iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; virtual; {override}
    function iCompile: Integer; virtual; {override}
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual; {override}
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
  protected
    //generate the op-code push const for the constant to the aSymbol(MUST BE MethodSymbol)
    function iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; virtual; {override}
    function iCompile: Integer; virtual; {override}
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual; {override}
    //if the value is string then save the string to the module's dataMemory.
    //and put the offset of the dataMemory to Value.VInteger
    //procedure DeclareStringTo(const aModule: TCustomTurboModule);
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
    procedure SetTurboType(const aValue: PMeType);
    {: assign value to mem! }
    {
      note: you must allocate the space first!
    }
    function AssignValueTo(const Source: Pointer): Boolean;
    //根据aValue 如果aTypeKind is mtkUnknown 那么会自动判断其类型
    function AssignValue(const aValue: string; aType: PMeType = nil; const aQuote: char = ''''): Boolean; overload;
    procedure AssignValue(const aConst: PTurboConstSymbol); overload;
  public
    property TurboType: PMeType read FTurboType write SetTurboType;
    property Value: TMeVarRec read FValue write FValue;
    property ValueStr: string read FValueStr write FValueStr;
    //这里的Size是指压入(PushTo)到代码区中数据大小
    property Size: tsInt read FSize write FSize;
  end;

  TTurboVariableSymbol = object(TTurboConstSymbol)
  protected
    function iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; virtual; {override}
    function iCompile: Integer; virtual; {override}
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual; {override}
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
  public
    Addr: tsInt;
  end;

  //the abstract program block
  TTurboCustomBlockSymbol = object(TTurboSymbol)
  protected
    //to generate new VMT for it.
    class function ParentClassAddress: TMeClass;virtual;abstract;
  public
  end;

  TTurboMethodArgument = Object(TTurboCustomSymbol)
  protected
    //to generate new VMT for it.
    class function ParentClassAddress: TMeClass;virtual;abstract;
  Public
    ParamFlags: TParamFlags; //pfVar, pfConst, etc...
    Symbol: TTurboSymbol;
  end;

  TTurboCustomMethodSymbol = object(TTurboCustomBlockSymbol)
  protected
    //FiTypeSymbol:internal used, when this type is not Declared!
    FiTypeSymbol: PTurboTypeSymbol; 
    FTypeSymbol: PTurboTypeSymbol;
    FEntryOffset: tsInt;
    //cfsFunction: 在本函数中引用的其它未确定地址的(not declareTo)的符号 PTurboSymbol
    //if Method IsRef: 那么则是需要预先编译进入的ModuleRef符号地址（尚未确定地址的）。
    FRefSymbols: PMeListEx;
  protected
    procedure Init;virtual; {override}
    function iCompile: Integer; virtual; {override}
    function iMethodCompile: Integer; virtual;
    procedure iMethodCompileEntry; virtual;

    function GetTypeSymbol: PTurboTypeSymbol; virtual;
    function GetTypeInfo: PMeProcType;
    function GetEntry: PTurboMethodEntry;
    function GetRefSymbols: PMeListEx;

    function CompileRefSymbols: Integer;
    function DeclareMethodTypeTo(const aModule: PTurboModuleSymbol): Integer;
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
    function BodyInit: Integer;virtual;abstract;
    function BodyFinal: Integer;virtual;abstract;
  public
    CallStyle: TCallingConvention;
    CodeFieldStyle: TTurboCodeFieldStyle;
    CFA: tsInt;//the offset address of the memory. -1 means not declare to Module.Memory
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord; //this is in the Entry.
    //Entry: tsInt; //the MUST BE Offset address: PTurboMethodEntry; 
    property Entry: PTurboMethodEntry read GetEntry;
    property TypeSymbol: PTurboTypeSymbol read GetTypeSymbol;
    property TypeInfo: PMeProcType read GetTypeInfo;
    property RefSymbols: PMeListEx read GetRefSymbols;
  end;

  TTurboMethodRefSymbol = object(TTurboCustomMethodSymbol)
  protected
    procedure Init;virtual; {override}
    function iMethodCompile: Integer; virtual; {override}
    procedure iMethodCompileEntry; virtual; {override}
    function GetTypeSymbol: PTurboTypeSymbol; virtual; {override}
    function GetExternalLocalMethodAddr(): tsInt;
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
    function iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; virtual; {override}
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual; {override}
    function BodyInit: Integer;virtual; {override}
    function BodyFinal: Integer;virtual; {override}
  public
    ModuleName: String;  //for external word
    ModuleType: TTurboModuleType;  //for external word
    ModuleSymbol: PTurboModuleSymbol;//for external forth word, the word is in the ModuleSymbol
    ExternalMethodSymbol: PTurboCustomMethodSymbol;
    ExternalOptions: TTurboExteralMethodOptions; //Exteral Word Options
  end;

  TTurboMethodSymbol = object(TTurboCustomMethodSymbol)
  protected
    FBodyFine: Boolean; //for cfsFunction
    FLabels: PTurboSymbols;
    //FBody:store the cfsFunction's function body here.
    FBody: PTurboCodeMemory;
  protected
    procedure Init;virtual; {override}
    function iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; virtual; {override}
    function iMethodCompile: Integer; virtual; {override}
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual; {override}

    function GetLabels: PTurboSymbols;
    function GetBody: PTurboCodeMemory;

    //for cfsFunction MainEntryProc Only
    procedure FinalStaticFields;
  public
    destructor Destroy; virtual; {override}
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
    function DeclareLabel(const aLabelName: string): Integer;
    function FindLabel(const aName: string): PTurboLabelSymbol;
    function BodyInit: Integer;virtual; {override}
    function BodyFinal: Integer;virtual; {override}
    procedure PushStringSeqToStack(const aStr: string);
  public
    //Options: TTurboWordOptions;
    property Labels: PTurboSymbols read GetLabels;
    //for cfsFunction only
    property Body: PTurboCodeMemory read GetBody;
  end;

  {: the user defined type symbol.}
  {
    类型先挂在 iType 上,等DeclareTo到模块后，设置 iType 为 nil， 在转为挂在 MeType
    总是 MeFreeAndNil(iType)， 这样自然解决未用类型的删除问题！
  }
  TTurboTypeSymbol = object(TTurboSymbol)
  protected
    FIsInternal: Boolean; //if true this symbol is the GRegisteredTypes
    {: internal used type, not declare to the module.}
    FiType: PMeType;
    FTypeInfo: PMeType;
    FEntryOffset: tsInt;
  protected
    function iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; virtual; {override}
    function iCompile: Integer; virtual; {override}
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual; {override}
    function GetEntry: PTurboTypeInfoEntry;
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
    function GetTypeInfoEx(const aClass: TMeClass = nil): PMeType;
    function GetTypeInfo(): PMeType;
  public
    destructor Destroy; virtual; {override}
  public
    //Entry: tsInt; //MUST BE Offset address : PTurboTypeInfoEntry;
    property Entry: PTurboTypeInfoEntry read GetEntry;
    Property TypeInfo: PMeType read GetTypeInfo;
    Property IsInternal: Boolean read FIsInternal;
  end;

  TTurboTypeRefSymbol = object(TTurboTypeSymbol)
  public
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
  public
    //use the TypeSymbol's entry:
    //Entry: PTurboTypeRefEntry;
    //this typeRef is Declared to RefModule in fact.
    RefModule: PTurboModuleSymbol;
    TypeId: tsInt;
  end;

  {if OwnerSymbol = '' means this is the Compile Module, or it's external used module.}
  TTurboModuleSymbol = object(TTurboSymbol)
  protected
    //collects methods in the Module.
    FMethods: PTurboSymbols;
    FMethodRefs: PTurboSymbols;
    FTypes: PTurboTypeSymbols;
    FTypeRefs: PTurboTypeSymbols;
    FStaticFields: PTurboSymbols;
    FConsts: PTurboSymbols;
    FUsedModules: PTurboModuleRefSymbols;
  protected
    function iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; virtual; {override}
    function iCompile: Integer; virtual; {override}
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual; {override}

    function GetTypeRefs: PTurboTypeSymbols;
    function GetTypes: PTurboTypeSymbols;
    function GetStaticFields: PTurboSymbols;
    function GetConsts: PTurboSymbols;
    function GetMethods: PTurboSymbols;
    function GetMethodRefs: PTurboSymbols;
    function GetUsedModules: PTurboModuleRefSymbols;
    function CreateLibModuleSymbol(const aName: string): PTurboModuleSymbol;
    function CreateDLLModuleSymbol(const aName: string): PTurboModuleSymbol;
    //For external Module(Reference) only
    function DeclareTo(const aModule: PTurboModuleSymbol): Integer;
  public
    destructor Destroy; virtual; {override}
    procedure Clear;
    procedure Assign(const aSymbol: PTurboCustomSymbol); virtual; {override}
    function FindType(const aName: string): PMeType;
    //function FindLocalType(const aName: string): PMeType;
    function FindLocalTypeSymbol(const aName: string): PTurboTypeSymbol;
    function FindLocalMethodSymbol(const aName: string): PTurboMethodSymbol;
    function FindLocalConstSymbol(const aName: string; const aType: PMeType): PTurboConstSymbol;
    //find all method in all place.
    function FindMethodSymbol(const aName: string): PTurboCustomMethodSymbol;
    function AddUsedModule(const aName: String; const aModuleType: TTurboModuleType): Integer;
    //将外部的TSLib方法添加到本地的Methods方法列表中，注意：没有写入内存(DeclareTo)。
    function AddExternalLibMethod(const aMethod: PTurboCustomMethodSymbol): PTurboMethodRefSymbol;
    //Declare the external module To aModule.
    function IsUniqueIdentifier(const aName: String): Boolean;
    procedure PushInt32(const aStr: string);overload;
    procedure PushInt32(const aInt: tsInt);overload;

    //Create a new constant
    function NewConst(const aName: string): PTurboConstSymbol;
    function NewVar(const aName: string): PTurboVariableSymbol;
    function NewType(const aName: string): PTurboTypeSymbol;
    function NewMethod(const aName: string): PTurboMethodSymbol;
    function NewMethodRef(const aName: string): PTurboMethodRefSymbol;
    //当带常量的变量有动态数组，AnsiString 的时候需要在模块的程序头添加初始化过程：对变量分配内存，并将常量赋值给变量；在程序尾添加终止过程，释放变量内存，并最后加上halt指令。
    //procedure DoInitModuleProc;
    //procedure DoFinalModuleProc;
  public
    ModuleType: TTurboModuleType;
    Module: TCustomTurboModule;
    property Types: PTurboTypeSymbols read GetTypes;
    property TypeRefs: PTurboTypeSymbols read GetTypeRefs;
    property StaticFields: PTurboSymbols read GetStaticFields;
    property Consts: PTurboSymbols read GetConsts;
    property Methods: PTurboSymbols read GetMethods;
    property MethodRefs: PTurboSymbols read GetMethodRefs;
    property UsedModules: PTurboModuleRefSymbols read GetUsedModules;
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
    procedure Assign(const aSrc: PTurboModuleRefSymbols); {override}
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
    function FindMethodSymbol(const aName: string; const aModuleType: TTurboModuleType = mtLib): PTurboCustomMethodSymbol;
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
    function IndexOfSameAs(const aType: PMeType; const aBeginIndex: Integer = 0): Integer;
    function IndexOf(const aType: PMeType; const aBeginIndex: Integer = 0): Integer; overload;
    function IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer; overload;
    function Find(const aName: string): PTurboTypeSymbol;overload;
    function Find(const aType: PMeType): PTurboTypeSymbol;overload;
    function FindSameAs(const aType: PMeType): PTurboTypeSymbol;
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

{ TTurboSymbol }

destructor TTurboSymbol.Destroy;
begin
  //writeLn(Name,'.Free');
  Inherited;
end;

procedure TTurboSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboSymbol)) then
    with PTurboSymbol(aSymbol)^ do
    begin
      Self.Visibility := Visibility;
      Self.OwnerSymbol := OwnerSymbol;
    end;
  Inherited;
end;

procedure TTurboSymbol.CompileError(const aErrCode: Integer);
begin
  if Assigned(OwnerSymbol) and Assigned(OwnerSymbol.OnError) then
    OwnerSymbol.OnError(@Self, aErrCode)
  else
    Inherited;
end;

function TTurboSymbol.IsCompileAllowed: Boolean;
begin
  Result := (FRefCount > 0) or (not Assigned(OwnerSymbol)) or (Assigned(OwnerSymbol) and IsPublic(OwnerSymbol.Module));
end;

function TTurboSymbol.IsPublic(const aModule: TCustomTurboModule): Boolean;
begin
  Result := InheritsFrom(TypeOf(TTurboModuleSymbol)) or ((Visibility and tvPublicVisibilty) = tvPublicVisibilty) or (Assigned(aModule) and ([soSymbolPublic, soSymbolTyped, soSymbolNamed]*aModule.Options <> []));
end;

function TTurboSymbol.IsTyped(const aModule: TCustomTurboModule): Boolean;
begin
  Result := ((Visibility and tvPublicTypedVisibilty) = tvPublicTypedVisibilty) or (Assigned(aModule) and (soSymbolTyped in aModule.Options));
end;

function TTurboSymbol.IsNamed(const aModule: TCustomTurboModule): Boolean;
begin
  Result := ((Visibility and tvPublicNamedVisibilty) = tvPublicNamedVisibilty) or (Assigned(aModule) and (soSymbolNamed in aModule.Options));
end;

{ TTurboLabelSymbol }
destructor TTurboLabelSymbol.Destroy;
begin
  //WordName := '';
  Inherited;
end;

procedure TTurboLabelSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboLabelSymbol)) then
    with PTurboLabelSymbol(aSymbol)^ do
    begin
      Self.Method := Method;
      Self.Addr := Addr;
    end;
  Inherited;
end;

function TTurboLabelSymbol.iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; 
begin
  Result := cSymbolErrorOk;
end;

procedure TTurboLabelSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
begin
end;

function TTurboLabelSymbol.iCompile: Integer;
begin
  if Addr = 0 then
  begin
    Addr := Method.Body.UsedSize;
    Result := cSymbolErrorOk;
  end
  else
    Result := cSymbolErrorRedeclaration;
end;

{ TTurboCustomMethodSymbol }
procedure TTurboCustomMethodSymbol.Init;
begin
  Inherited;
  CFA := -1;
end;

destructor TTurboCustomMethodSymbol.Destroy;
begin
  MeFreeAndNil(FiTypeSymbol);
  Inherited;
end;

procedure TTurboCustomMethodSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboCustomMethodSymbol)) then
    with PTurboCustomMethodSymbol(aSymbol)^ do
    begin
      Self.CallStyle := CallStyle;
      Self.CodeFieldStyle := CodeFieldStyle;
      Self.CFA:= CFA;
      Self.FEntryOffset := FEntryOffset;
      Self.FTypeSymbol := FTypeSymbol;
      Self.FiTypeSymbol := FiTypeSymbol;
    end;
  Inherited;
end;

Function TTurboCustomMethodSymbol.CompileRefSymbols: Integer;
var
  i: Integer;
begin
  if Assigned(FRefSymbols) then with FRefSymbols^ do
  begin
    for i := 0 to Count -1 do if not PTurboSymbol(Items[i]).IsCompiled then
    begin
      Result := PTurboSymbol(Items[i]).Compile;
      if not IsSymbolOk(Result) then exit;
    end;
    Clear;
  end;
  Result := cSymbolErrorOk;
end;

function TTurboCustomMethodSymbol.DeclareMethodTypeTo(const aModule: PTurboModuleSymbol): Integer;
var
  vTypeSymbol: PTurboTypeSymbol;
  i: Integer;
begin
  if not Assigned(FiTypeSymbol) then
  begin
    Result := cSymbolErrorRedeclaration;
    Exit;
  end;
  vTypeSymbol := aModule.Types.FindSameAs(TypeInfo);
  if Assigned(vTypeSymbol) then
  begin
    Assert(vTypeSymbol <> FiTypeSymbol, 'DeclareMethodTypeTo: vTypeSymbol = FiTypeSymbol Error');
    MeFreeAndNil(FiTypeSymbol);
    FTypeSymbol := vTypeSymbol;
  end
  else
  begin
    i := GRegisteredTypes.IndexOfSameAs(TypeInfo);
    if i >= 0 then
    begin
      MeFreeAndNil(FiTypeSymbol.FiType);
      FiTypeSymbol.FTypeInfo := GRegisteredTypes.Items[i];
      FiTypeSymbol.FIsInternal := True;
      Integer(Entry.Word.TurboType) := -i;
    end;
    FTypeSymbol := FiTypeSymbol;
    FiTypeSymbol := nil;
    aModule.Types.Add(FTypeSymbol);
  end;
  //Result := FTypeSymbol.DeclareTo(aModule);
  FTypeSymbol.ReferenceTo(OwnerSymbol);
  Result := FTypeSymbol.Compile;
  if Result = cSymbolErrorRedeclaration then
    Result := cSymbolErrorOk;
  if IsSymbolOk(Result) and not FTypeSymbol.IsInternal then
  begin
    i := aModule.Module.RegisteredTypes.IndexOf(FTypeSymbol.TypeInfo);
    if i >= 0 then Inc(i) else i := 0;
    Integer(Entry.Word.TurboType) := i;
  end;
  if IsSymbolOk(Result) and Assigned(Entry.Word.TurboType) then
    aModule.Module.RelocatedDataTypes.Add(Integer(@Entry.Word.TurboType) - Integer(aModule.Module.DataMemory));
end;

function TTurboCustomMethodSymbol.GetEntry: PTurboMethodEntry;
begin
  Assert(Assigned(OwnerSymbol), 'OwnerSymbol=nil');
  Assert(Assigned(OwnerSymbol.Module), 'OwnerSymbol.Module=nil');
  Result := Pointer(Integer(OwnerSymbol.Module.DataMemory) + FEntryOffset);
end;

function TTurboCustomMethodSymbol.GetRefSymbols: PMeListEx;
begin
  if not Assigned(FRefSymbols) then
  begin
    New(FRefSymbols, Create);
  end;
  Result := FRefSymbols;
end;


function TTurboCustomMethodSymbol.GetTypeSymbol: PTurboTypeSymbol;
begin
  if Assigned(FTypeSymbol) then
    Result := FTypeSymbol
  else if Assigned(FiTypeSymbol) then
    Result := FiTypeSymbol
  else
  begin
    //writeLn('MethodtypeCre:',Name);
    //no Assigned, Create a one.
    New(FiTypeSymbol, Create);
    FiTypeSymbol.OwnerSymbol := OwnerSymbol;
    Result := FiTypeSymbol;
    //Result.GetTypeInfo(TypeOf(TMeProcType));
  end;
end;

function TTurboCustomMethodSymbol.GetTypeInfo: PMeProcType;
begin
  Result := PMeProcType(TypeSymbol.GetTypeInfoEx(TypeOf(TMeProcType)));
end;

function TTurboCustomMethodSymbol.iCompile: Integer;
var
  //p: PTurboExteralMethodOptions;
  vIsPublic: Boolean;
begin
    //Result := cSymbolErrorOk;
  Result := CompileRefSymbols;

  if IsSymbolOk(Result) then
    Result := iMethodCompile;

  if IsSymbolOk(Result) then
  begin
    vIsPublic := IsPublic(OwnerSymbol.Module);
    if vIsPublic or (CodeFieldStyle in cTurboExternalFunctionTypes) then with OwnerSymbol.Module do
    begin
      //writeln('OwnerSymbol.UsedDataSize=',InttoHex(UsedDataSize,4));
      AlignData;
      //writeln('OwnerSymbol.UsedDataSize=',InttoHex(UsedDataSize,4));
      FEntryOffset := UsedDataSize;
      AllocDataSpace(SizeOf(TTurboMethodEntry));
      if vIsPublic then
        Entry.Prior := LastWordEntry
      else
        Entry.Prior := nil;
      if IsNamed(OwnerSymbol.Module) then
      begin
        Entry.Word.Name := Pointer(UsedDataSize);
        //PChar:
        AddPCharToData(Self.Name);
        //AddBufferToData(Self.Name[1], Length(Self.Name));
        //AddByteToData(0);
      end
      else
      begin
        //OwnerSymbol.AddByteToData(0);
        Entry.Word.Name := nil;
      end;
      Entry.Word.MethodAddr := CFA;
      Entry.Word.Visibility := Visibility;
      Entry.Word.CallStyle := CallStyle;
      Entry.Word.CodeFieldStyle := CodeFieldStyle;

      iMethodCompileEntry;

      if vIsPublic then
      begin
        LastWordEntry := Pointer(FEntryOffset);
        Entry.Word.ParamFieldLength := ParamFieldLength;
      end;

      if IsTyped(OwnerSymbol.Module) or Entry.Word.IsRef then
      begin
        Result := DeclareMethodTypeTo(OwnerSymbol);
      end
      else
        Entry.Word.TurboType := nil;
    end;
  end;
end;

function TTurboCustomMethodSymbol.iMethodCompile: Integer;
begin
  Result :=cSymbolErrorOk;
end;

procedure TTurboCustomMethodSymbol.iMethodCompileEntry;
begin
end;

{ TTurboMethodRefSymbol }
procedure TTurboMethodRefSymbol.Init;
begin
  Inherited;
  ExternalOptions.Index := -1;
end;

destructor TTurboMethodRefSymbol.Destroy;
begin
  ModuleName := '';
  Inherited;
end;

procedure TTurboMethodRefSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboMethodRefSymbol)) then
    with PTurboMethodRefSymbol(aSymbol)^ do
    begin
      Self.ModuleName := ModuleName;
      Self.ModuleType:=ModuleType;
      Self.ModuleSymbol := ModuleSymbol;
      Self.ExternalMethodSymbol := ExternalMethodSymbol;
      Self.ExternalOptions:=ExternalOptions;
    end;
  Inherited;
end;

function TTurboMethodRefSymbol.BodyInit: Integer;
begin
  Result := cSymbolErrorOk;
  if Assigned(OwnerSymbol) and (OwnerSymbol.MethodRefs.IndexOf(@Self) < 0) then
  begin
    //Writeln('ok:',Name);
    if OwnerSymbol.IsUniqueIdentifier(Name) then
    begin
      OwnerSymbol.MethodRefs.Add(@Self);
    end
    else
      Result := cSymbolErrorRedeclaration;
  end;
end;

function TTurboMethodRefSymbol.BodyFinal: Integer;
var
  //vIsPublic: Boolean;
  i: Integer;
  vModuleEntry: PTurboModuleRefEntry;
begin
  case CodeFieldStyle of
    cfsExternalFunction, cfsDLLFunction, cfsHostFunction:
      begin
        Result := cSymbolErrorOk;
        vModuleEntry := nil;
        //try to determine the module of the aMethod.
        if ModuleName = '' then
        begin
          i := OwnerSymbol.UsedModules.Count -1;
          if i >= 0 then
          begin
            //aMethod.ModuleSymbol := UsedModules.Items[i];
            ModuleSymbol := OwnerSymbol.UsedModules.Items[i];
            ModuleName := ModuleSymbol.Name;
            vModuleEntry := OwnerSymbol.UsedModules.Entries[i];
          end
          else
          begin 
            ModuleSymbol := nil;
            Result := cSymbolErrorNoSpecifiedModule;
          end;
        end
        else //aMethod.ModuleName <> '' 
        begin
          i := OwnerSymbol.UsedModules.IndexOf(ModuleName, ModuleType);
          if i < 0 then i := OwnerSymbol.AddUsedModule(ModuleName, ModuleType);
          if i >= 0 then
          begin
            ModuleSymbol := OwnerSymbol.UsedModules.Items[i];
            vModuleEntry := OwnerSymbol.UsedModules.Entries[i];
          end
          else
          begin 
            //writeln('Can not add used module:' + aMethod.ModuleName);
            ModuleSymbol := nil;
            Result := cSymbolErrorUnknownModule;
          end;
        end;
      
        if Assigned(ModuleSymbol) and IsSymbolOk(Result) then
        begin
          if Assigned(vModuleEntry) then
          begin
            ExternalOptions.ModuleRef := Pointer(Integer(vModuleEntry) + SizeOf(tsPointer));
          end
          else
            ExternalOptions.ModuleRef := nil;
      
          if CodeFieldStyle = cfsExternalFunction then //the external native function
          begin
            if ExternalOptions.Name <> '' then
              ExternalMethodSymbol := ModuleSymbol.FindLocalMethodSymbol(ExternalOptions.Name)
            else
              ExternalMethodSymbol := ModuleSymbol.FindLocalMethodSymbol(Name);
            if not Assigned(ExternalMethodSymbol) then
              Result := cSymbolErrorUnknownMethod;
          end;
        end
        else
        begin
          ExternalMethodSymbol := nil;
          Result := cSymbolErrorUnknownModule;
        end;
      end;
    else
      Result := cSymbolErrorUnknownMethod;
  end; //case
end;

function TTurboMethodRefSymbol.GetExternalLocalMethodAddr(): tsInt;
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

function TTurboMethodRefSymbol.GetTypeSymbol: PTurboTypeSymbol;
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

function TTurboMethodRefSymbol.iMethodCompile: Integer;
var
  i: tsInt;
  //vIsPublic: Boolean;
begin
    Result := cSymbolErrorOk;
    Case CodeFieldStyle of
      cfsExternalFunction: //the external forth function
        begin
          CFA := GetExternalLocalMethodAddr();
          if CFA = -1 then
            Result := cSymbolErrorUnknownMethod;

          if (Result = cSymbolErrorOk) and not Assigned(ExternalOptions.ModuleRef) then
          begin
            Result := ModuleSymbol.Compile;
            if not IsSymbolOk(Result) then Exit;
            i := OwnerSymbol.UsedModules.IndexOf(ModuleSymbol);
            if i >= 0 then
            begin
              ExternalOptions.ModuleRef := Pointer(Integer(OwnerSymbol.UsedModules.Entries[i]) + SizeOf(tsPointer));
            end
            else
              Result := cSymbolErrorModuleRefAddedFailed;
          end;
        end;
      cfsDLLFunction:
        begin
          CFA := 0;
          //writeln('CompileDLL:', Name);
          if not Assigned(ExternalOptions.ModuleRef) then
            Result := ModuleSymbol.DeclareTo(OwnerSymbol);
          if not IsSymbolOk(Result) then Exit;
          i := OwnerSymbol.UsedModules.IndexOf(ModuleSymbol);
          if i >= 0 then
          begin
              ExternalOptions.ModuleRef := Pointer(Integer(OwnerSymbol.UsedModules.Entries[i]) + SizeOf(tsPointer));
          end
          else
              Result := cSymbolErrorModuleRefAddedFailed;
        end;
      else
        Result := cSymbolErrorUnknownMethod;
    end; //case

end;

procedure TTurboMethodRefSymbol.iMethodCompileEntry;
var
  p: PTurboExteralMethodOptions;
begin
      if Entry.Word.IsRef then with OwnerSymbol.Module do
      begin
        //writeln('IsExternal:', Self.Name);
        Integer(p) := UsedDataSize;
        AllocDataSpace(SizeOf(TTurboExteralMethodOptions));
        Integer(p) := Integer(DataMemory) + Integer(p);
        if not Assigned(Entry.Word.Name) and (ExternalOptions.Index = -1) then
        begin
          if CodeFieldStyle = cfsExternalFunction then
          begin
            //ExternalOptions.Name is shortString, but the Name is String, 万一放不下，所以对DLL可以用 ExternalOptions.Name，但是对本族的外部函数还是用这个！
            Entry.Word.Name := Pointer(UsedDataSize);
            //PChar:
            AddPCharToData(Self.Name);
          end
          else if ExternalOptions.Name = '' then
            ExternalOptions.Name := Self.Name;
        end;
        p^ := ExternalOptions;
        Integer(p^.ModuleRef) := Integer(ExternalOptions.ModuleRef) - Integer(DataMemory);
        RelocatedDataAddresses.Add(Integer(@p^.ModuleRef)-Integer(DataMemory));
        Assert(p=Entry.Word.GetExternalOptionsAddr, 'Method.DeclareTo: ExternalOptionsAddr mismatch');
      end;
end;

function TTurboMethodRefSymbol.iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer;
var
  vCFA: tsInt;
begin
  if aSymbol.InheritsFrom(TypeOf(TTurboMethodSymbol)) then
  begin
    Result := cSymbolErrorOk;
    case CodeFieldStyle of
      cfsExternalFunction: //the external TurboScript Lib Function.
        begin
          vCFA := CFA;
          if vCFA = -1 then
          begin
            vCFA := GetExternalLocalMethodAddr();  //外部的方法CFA地址总是存在的。
          end;
          if vCFA <> -1 then with PTurboMethodSymbol(aSymbol).Body^ do
          begin
            Assert(Assigned(ModuleSymbol), 'cfsExternalFunction: ModuleSymbol = nil');
            ModuleSymbol.ReferenceTo(aSymbol);
            PTurboMethodSymbol(aSymbol).RefSymbols.Add(Cardinal(ModuleSymbol));
            AddOpCode(opCallFar);
            if Assigned(ExternalOptions.ModuleRef) then
            begin
              AddInt(Integer(ExternalOptions.ModuleRef) - Integer(PTurboMethodSymbol(aSymbol).OwnerSymbol.Module.DataMemory));
            end
            else
            begin
              with UnResolvedRefs.Add()^ do
              begin
                Symbol := aSymbol;
                Addr := UsedSize;
              end;
              PTurboMethodSymbol(aSymbol).RefSymbols.Add(Cardinal(@Self));
              AddInt(0);
            end;
            AddInt(vCFA);
            //writeln(Name, '.CFA:',vCFA);
          end
          else
            Result := cSymbolErrorUnknownMethod;
        end;
      cfsDLLFunction:
        begin
          with PTurboMethodSymbol(aSymbol).Body^ do
          begin
            Assert(Assigned(ModuleSymbol), 'cfsExternalFunction: ModuleSymbol = nil');
            ModuleSymbol.ReferenceTo(aSymbol);
            PTurboMethodSymbol(aSymbol).RefSymbols.Add(Cardinal(ModuleSymbol));
            AddOpCode(opCallExt);
            if FIsCompiled then
            begin
              vCFA := Integer(@Entry.Word);
              vCFA := vCFA - Integer(PTurboMethodSymbol(aSymbol).OwnerSymbol.Module.DataMemory);
            end
            else
            begin
              with UnResolvedRefs.Add()^ do
              begin
                Symbol := aSymbol;
                Addr := UsedSize;
                //writeln('RefTO:',UsedSize);
              end;
              PTurboMethodSymbol(aSymbol).RefSymbols.Add(Cardinal(@Self));
              vCFA := 0;
            end;
            //writeln('RefCFA:',CFA);
            AddInt(vCFA);
          end;
        end;
    end;//case
  end
  else
    Result := cSymbolErrorUnknownRef;
end;

procedure TTurboMethodRefSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
var
  p: PInteger;
begin
  with aValue^ do 
    p := Pointer(Integer(PTurboMethodSymbol(Symbol)^.Body.Memory) + Addr);

  case CodeFieldStyle of
      cfsExternalFunction: //the external forth function
        begin
          p^ := Integer(ExternalOptions.ModuleRef) - Integer(PTurboSymbol(aValue.Symbol).OwnerSymbol.Module.DataMemory);
        end;
      cfsDLLFunction:
        begin
          //writeln(Entry.Word.GetExternalOptionsAddr.Name, ' ',Integer(Body.UsedSize),' = ', PInteger(p)^, ',', Integer(aValue.Symbol.OwnerSymbol.Module.DataMemory));
          p^ := Integer(@Entry.Word) - Integer(PTurboSymbol(aValue.Symbol).OwnerSymbol.Module.DataMemory);
          //writeln(Entry.Word.GetExternalOptionsAddr.Name, ' ',Integer(p),' = ', PInteger(p)^, ',', Integer(aValue.Symbol.OwnerSymbol.Module.DataMemory));
        end;
  end; //case
end;

{ TTurboMethodSymbol }
procedure TTurboMethodSymbol.Init;
begin
  Inherited;
  CodeFieldStyle := cfsFunction;
end;

destructor TTurboMethodSymbol.Destroy;
begin
  MeFreeAndNil(FLabels);
  MeFreeAndNil(FBody);
  MeFreeAndNil(FRefSymbols);
  Inherited;
end;

procedure TTurboMethodSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboMethodSymbol)) then
    with PTurboMethodSymbol(aSymbol)^ do
    begin
      Self.ParamFieldLength := ParamFieldLength;
      if Assigned(FLabels) then
        Self.Labels.Assign(FLabels);
      if Assigned(FBody) then
        Self.Body.Assign(FBody);
      if Assigned(FRefSymbols) then
        Self.RefSymbols.Assign(FRefSymbols);
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

function TTurboMethodSymbol.GetBody: PTurboCodeMemory;
begin
  if not Assigned(FBody) then
  begin
    New(FBody, Create);
  end;
  Result := FBody;
end;

function TTurboMethodSymbol.iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer;
//var
  //vCFA: tsInt;
begin
  if aSymbol.InheritsFrom(TypeOf(TTurboMethodSymbol)) then
  begin
    Result := cSymbolErrorOk;
    case CodeFieldStyle of
      cfsFunction: with PTurboMethodSymbol(aSymbol).Body^ do
        begin
          if IsPublic(OwnerSymbol.Module) then
          begin
            AddOpCode(opEnterFar);
            AddInt(0);
          end
          else
          begin
            AddOpCode(opEnter);
          end;
          if CFA = -1 then
          begin
            with UnResolvedRefs.Add^ do
            begin
              Symbol := aSymbol;
              Addr := UsedSize;
            end;
            PTurboMethodSymbol(aSymbol).RefSymbols.Add(Cardinal(@Self));
          end;
          AddInt(CFA);
        end;
    end;//case
  end
  else
    Result := cSymbolErrorUnknownRef;
end;

function TTurboMethodSymbol.iMethodCompile: Integer;
var
  p: Pointer;
  //vIsPublic: Boolean;
begin
  if FBodyFine then
  begin
      //writeln('TTurboMethodSymbol(',Name,').iCompile:',Body.UsedSize);
      Integer(p) := Integer(Body.Memory) + 16;
      //writeln(Integer(p), '=',PInteger(p)^);
      CFA := OwnerSymbol.Module.UsedMemory;
      OwnerSymbol.Module.AllocSpace(Body.UsedSize);
      p := Pointer(Integer(OwnerSymbol.Module.Memory) + CFA);
      Move(Body.Memory^, p^, Body.UsedSize);
      Result := cSymbolErrorOk;
  end
  else
  begin
    Result := cSymbolErrorMethodBodyNotFine;
  end;
end;

procedure TTurboMethodSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
var
  p: PInteger;
begin
  with aValue^ do 
    p := Pointer(Integer(PTurboMethodSymbol(Symbol)^.Body.Memory) + Addr);

  case CodeFieldStyle of
      cfsFunction:
        begin
          p^ := CFA;
        end;
  end; //case
end;

function TTurboMethodSymbol.BodyInit: Integer;
begin
  Result := cSymbolErrorOk;
  if Assigned(OwnerSymbol) and (OwnerSymbol.Methods.IndexOf(@Self) < 0) then
  begin
    if OwnerSymbol.IsUniqueIdentifier(Name) then
    begin
      OwnerSymbol.Methods.Add(@Self);
    end
    else
      Result := cSymbolErrorRedeclaration;
  end;

  if CodeFieldStyle = cfsFunction then
    Body.Clear;
  FBodyFine := False;
end;

function TTurboMethodSymbol.BodyFinal: Integer;
var
  vIsPublic: Boolean;
  //i: Integer;
  //vModuleEntry: PTurboModuleRefEntry;
begin
  //if FBodyFine then exit;
  case CodeFieldStyle of
    cfsFunction: with Body^ do
      begin
        vIsPublic := IsPublic(OwnerSymbol.Module);
    
        if Self.Name = cMainEntryProcName then  //for MainEntryProc ONly
        begin
          Inc(FRefCount);
          //writeln('DeclareEndTo:',cMainEntryProcName );
          FinalStaticFields;
          //AddOpPushInt32(0);
          AddOpCode(opHalt);
        end
        else if vIsPublic then
          AddOpCode(opExitFar)
        else
          AddOpCode(opExit);
        ParamFieldLength := UsedSize;
        FBodyFine := True;
        Result := cSymbolErrorOk;
      end;
    else
      Result := cSymbolErrorUnknownMethod;
  end; //case
end;

procedure TTurboMethodSymbol.FinalStaticFields;
var
  i: Integer;
begin
  for i := 0 to OwnerSymbol.StaticFields.Count - 1 do
  begin
    with PTurboVariableSymbol(OwnerSymbol.StaticFields.Items[i])^ do
    begin
      if Assigned(TurboType) and ((FRefCount > 0) or IsPublic(OwnerSymbol.Module)) then
      begin
        case TurboType.Kind of
          mtkLString: //clear the AnsiString(LongString)
            begin
              with Body^ do 
              begin
                AddOpCode(opPushInt); AddInt(0); 
                ReferenceTo(@Self);
                //AddOpCode(opFetchInt);
                AddOpCode(opLStrAsg);
              end;
            end;
        end; //case
      end;
    end;
  end;
end;

function TTurboMethodSymbol.DeclareLabel(const aLabelName: string): Integer;
var
  vLabel: PTurboLabelSymbol;
begin
  Result := Labels.IndexOf(aLabelName);
  if Result < 0 then
  begin
    New(vLabel, Create);
    Labels.Add(vLabel);
    with vLabel^ do
    begin
      Name := alabelName;
      Method := @Self;
    end;
    Result := vLabel.Compile;
  end
  else
    Result := cSymbolErrorLabelRedeclaration;
end;

procedure TTurboMethodSymbol.PushStringSeqToStack(const aStr: string);
var
  i: Integer;
begin
  for i := length(aStr) downto 1 do with Body^ do
  begin
    AddOpPushInt32(Ord(aStr[i]));
  end;
end;

{ TTurboModuleSymbol }
destructor TTurboModuleSymbol.Destroy;
begin
  Clear;
  Inherited;
end;

procedure TTurboModuleSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboModuleSymbol)) then
    with PTurboModuleSymbol(aSymbol)^ do
    begin
      //Self.Entry:= Entry;
      Self.ModuleType:=ModuleType;
      Self.Module:=Module;
      if Assigned(FMethods) then
        Self.Methods.Assign(FMethods)
      else 
        MeFreeAndNil(Self.FMethods);

      if Assigned(FMethodRefs) then
        Self.MethodRefs.Assign(FMethodRefs)
      else 
        MeFreeAndNil(Self.FMethodRefs);
        //}

      if Assigned(FStaticFields) then
        Self.StaticFields.Assign(FStaticFields)
      else 
        MeFreeAndNil(Self.FStaticFields);

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
  MeFreeAndNil(FMethodRefs);
  MeFreeAndNil(FUsedModules);
  //MeFreeAndNil(FUsedModuleEntries);
  MeFreeAndNil(FStaticFields);
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
  if not Assigned(Result) then
  begin
    case ModuleType of
      mtLib: if Assigned(Module) then
      begin
        //Integer(vMethodEntry) := Integer(Module.LastWordEntry) + Integer(Module.DataMemory);
        //writeln('LastWordEntry:', PChar(Integer(Module.DataMemory)+Integer(vMethodEntry.Word.Name)));
        
        vMethodEntry := Module.FindWordEntry(aName, cfsFunction);
        //writeln(Name, ' try...', aName, ' ', Integer(vMethodEntry));
        if Assigned(vMethodEntry) then
        begin
            New(Result, Create);
            Result.Name := aName;
            Result.CFA := vMethodEntry.Word.MethodAddr;
            //Result.ExternalOptions.ModuleRef := Pointer(Integer(Entry) + SizeOf(Pointer));
            Result.CallStyle := vMethodEntry.Word.CallStyle;
            Result.CodeFieldStyle := vMethodEntry.Word.CodeFieldStyle;
            //Result.ModuleType := mtLib;
            //Result.ModuleName := Name;
            //Result.ModuleSymbol := @Self;
            //Result.Module := Module;
            Result.OwnerSymbol := @Self;
            Result.FEntryOffset := Integer(vMethodEntry) - Integer(Module.DataMemory);
            Methods.Add(Result); //cache this method!
        end;
      end;
    end;//case
  end;
end;

function TTurboModuleSymbol.FindMethodSymbol(const aName: string): PTurboCustomMethodSymbol;
var
  i: Integer;
begin
  i := Methods.IndexOf(aName);
  if i >= 0 then
    Result := PTurboCustomMethodSymbol(Methods.Items[i])
  else begin
    i := MethodRefs.IndexOf(aName);
    if i >= 0 then
      Result := PTurboCustomMethodSymbol(MethodRefs.Items[i])
    else //}
    begin //search the MethodRefs in the UsedModules
      Result := UsedModules.FindMethodSymbol(aName);
      if Assigned(Result) then
      begin
        Assert(Assigned(Result.OwnerSymbol), 'Result.OwnerSymbol is nil');
        i := MethodRefs.IndexOf(Result.OwnerSymbol.Name + cDotSeparatorSymbol + aName);
        if i >= 0 then
          Result := PTurboCustomMethodSymbol(MethodRefs.Items[i])
        else
          Result := PTurboCustomMethodSymbol(AddExternalLibMethod(Result));
      end;
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

function TTurboModuleSymbol.GetMethodRefs: PTurboSymbols;
begin
  if not Assigned(FMethodRefs) then
  begin
    New(FMethodRefs, Create);
  end;
  Result := FMethodRefs;
end;
//}

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

function TTurboModuleSymbol.GetStaticFields: PTurboSymbols;
begin
  if not Assigned(FStaticFields) then
  begin
    New(FStaticFields, Create);
  end;
  Result := FStaticFields;
end;

function TTurboModuleSymbol.AddExternalLibMethod(const aMethod: PTurboCustomMethodSymbol): PTurboMethodRefSymbol;
begin
  if Assigned(aMethod) then
  begin
    Assert(Assigned(aMethod.OwnerSymbol), 'aMethod.OwnerSymbol is nil');
    Result := NewMethodRef(aMethod.OwnerSymbol.Name + cDotSeparatorSymbol + aMethod.Name);
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
      mtDLL: vModuleSymbol := CreateDLLModuleSymbol(aName);
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

function TTurboModuleSymbol.CreateDLLModuleSymbol(const aName: string): PTurboModuleSymbol;
begin
    New(Result, Create);
    with Result^ do
    begin
      Name := aName;
      ModuleType := mtDLL;
      Module := nil;
    end;
    //Integer(Result.Entry) := Integer(Module.DataMemory) + Module.UsedDataSize; //the offset address.
end;

function TTurboModuleSymbol.iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer;
begin
  Result := cSymbolErrorOk;
end;

function TTurboModuleSymbol.iCompile: Integer;
begin
  if Assigned(OwnerSymbol) then
  begin
    //it is external module
    Result := DeclareTo(OwnerSymbol);
  end
  else
  begin
    Result := Types.Compile;
    if not IsSymbolOk(Result) then Exit;
    Result := Consts.Compile;
    if not IsSymbolOk(Result) then Exit;
    Result := StaticFields.Compile;
    if not IsSymbolOk(Result) then Exit;
    Result := MethodRefs.Compile;
    if not IsSymbolOk(Result) then Exit;
    Result := Methods.Compile;
    if not IsSymbolOk(Result) then Exit;
    Result := cSymbolErrorOk;
  end;
end;

procedure TTurboModuleSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
begin
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
      vModuleEntry.Module.ModuleType := Self.ModuleType;
      //writeln(Self.Name, Integer(Self.ModuleType));
      if not (Self.ModuleType in cTurboExternalModuleTypes) then
      begin
        vModuleEntry.Module.Revision := Module.ModuleVersion;
        vModuleEntry.Module.BuildDate := Module.ModuleDate;
      end
      else
      begin
        vModuleEntry.Module.Handle := nil;
        vModuleEntry.Module.Revision := 0;
        vModuleEntry.Module.BuildDate.Time := 0;
        vModuleEntry.Module.BuildDate.Date := 0;
      end;
      vModuleEntry.Module.Name := Pointer(UsedDataSize);
      if Length(Self.Name) > 0 then
        AddBufferToData(Self.Name[1], Length(Self.Name));
      AddByteToData(0);
      LastModuleRefEntry := Pointer(tsInt(vModuleEntry) - Integer(DataMemory));
    end; //with
    aModule.UsedModules.FEntries.List^[i] := vModuleEntry;
  end;
  //else
    //Result := cSymbolErrorRedeclaration;
end;

function TTurboModuleSymbol.IsUniqueIdentifier(const aName: String): Boolean;
begin
  Result := Consts.IndexOf(aName) < 0;
  if Result then
    Result := StaticFields.IndexOf(aName)< 0;
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

function TTurboModuleSymbol.NewVar(const aName: string): PTurboVariableSymbol;
begin
  if (aName = '') or (StaticFields.IndexOf(aName) < 0) then
  begin
    New(Result, Create);
    Result.Name := aName;
    Result.OwnerSymbol := @Self;
    if aName <> '' then StaticFields.Add(Result);
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

function TTurboModuleSymbol.NewMethodRef(const aName: string): PTurboMethodRefSymbol;
begin
  if (aName = '') or (MethodRefs.IndexOf(aName) < 0) then
  begin
    New(Result, Create);
    Result.Name := aName;
    Result.OwnerSymbol := @Self;
    if aName <> '' then MethodRefs.Add(Result);
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
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboConstSymbol)) then
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

function TTurboConstSymbol.iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer;
var
  p: Pointer;
  vResolved: Boolean;
begin
   //writeln('Enter TTurboConstSymbol(',Name,').iReferenceTo:', aSymbol.Name);
  if aSymbol.InheritsFrom(TypeOf(TTurboMethodSymbol)) then
  begin
    //writeln('TTurboConstSymbol.iReferenceTo:', Value.VInteger);
    with PTurboMethodSymbol(aSymbol)^ do
    if Assigned(TurboType) then
    begin
      vResolved := True; //purpose already resolved.
      Case TurboType.Kind of
        mtkLString, mtkString: if FValue.VInteger = 0 then vResolved := False;
        else
        begin
          if Size <= SizeOf(Integer) then
          begin
            //aModule.AddOpToMem(opPushInt);
            //Size := SizeOf(Integer);
            Case Size of
              SizeOf(Byte): Body.AddOpCode(opPushByte);
              SizeOf(Word): Body.AddOpCode(opPushWord);
              else 
              begin
                Body.AddOpCode(opPushInt);
                Size := SizeOf(tsInt);
              end;
            end;//case
          end
          else
            Body.AddOpCode(opPushInt64);
        end;
      end; //Case
      Integer(p) := Body.UsedSize;
      Body.AllocSpace(Size);
      if vResolved then
      begin
        Integer(p) := Integer(p) + Integer(Body.Memory);
        AssignValueTo(p);
      end
      else
        with UnResolvedRefs.Add()^ do
        begin
          Symbol := aSymbol;
          Addr := Integer(p);
        end;
      Result := cSymbolErrorOk;
    end
    else
      Result := cSymbolErrorUnknownConstType;
  end
  else
    Result := cSymbolErrorUnknownRef;
end;

function TTurboConstSymbol.iCompile: Integer;
begin
  if Assigned(TurboType) then 
  begin
    Result := cSymbolErrorOk;
    with OwnerSymbol.Module do
    Case TurboType.Kind of
      mtkString: begin
        FValue.VInteger := AddShortStringToData(ValueStr);
      end;
      mtkLString: begin
        FValue.VInteger := AddStringToData(ValueStr);
      end;
    end;//case
  end
  else
    Result := cSymbolErrorUnknownConstType;
end;

procedure TTurboConstSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
var
  p: Pointer;
begin
  with aValue^ do 
    p := Pointer(Integer(PTurboMethodSymbol(Symbol)^.Body.Memory) + Addr);
  AssignValueTo(p);
end;

{
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
        FValue.VInteger := aModule.AddStringToData(ValueStr);
      end;
    end;//case
end;
//}

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

{ TTurboVariableSymbol }
procedure TTurboVariableSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboVariableSymbol)) then
    with PTurboVariableSymbol(aSymbol)^ do
    begin
      Self.Addr := Addr;
    end;
  Inherited;
end;

function TTurboVariableSymbol.iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer;
begin
  if aSymbol.InheritsFrom(TypeOf(TTurboMethodSymbol)) then
  begin
    with PTurboMethodSymbol(aSymbol).Body^ do
    begin
      AddOpCode(opPushInt);
      if Addr = 0 then
        with UnResolvedRefs.Add()^ do
        begin
          Symbol := aSymbol;
          Addr := UsedSize;
        end;
      AddInt(Addr);
      //writeln('Var(',Self.Name,').RefTo');
    end;
    Result := cSymbolErrorOk;
  end
  else
    Result := cSymbolErrorUnknownRef;
end;

function TTurboVariableSymbol.iCompile: Integer;
var
  vVaraibleEntry: PTurboStaticFieldEntry;
  vValue: Pointer;
  //vTypeSafety, vTypeNamed: Boolean;
begin
  //writeln('Var(',Self.Name,').Compile');
  Result := cSymbolErrorOk;
  vVaraibleEntry := nil;
  if IsPublic(OwnerSymbol.Module) then 
  with OwnerSymbol.Module do
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

  if IsRequiredAlignMem(TurboType) then OwnerSymbol.Module.AlignData;
  Addr := OwnerSymbol.Module.UsedDataSize;

  if IsPublic(OwnerSymbol.Module) then //I must know the all Variables for relocate the Variable addr to absolute addr.
  begin
    tsInt(vVaraibleEntry.Variable.Addr) := Addr;
    OwnerSymbol.Module.LastVariableEntry := Pointer(Integer(vVaraibleEntry) - Integer(OwnerSymbol.Module.DataMemory));
  end;

  Integer(vValue) := Integer(OwnerSymbol.Module.DataMemory)  + Addr;
  OwnerSymbol.Module.AllocDataSpace(Size);
  if ValueStr <> '' then //该变量有初值.
  begin
    if TurboType.Kind in [mtkLString, mtkString] then
    begin
      //DeclareStringTo(OwnerSymbol.Module); //if the init value is string
      Result := Inherited iCompile;
      OwnerSymbol.Module.RelocatedDataAddresses.Add(Addr);
    end;
    AssignValueTo(vValue);
  end
  else
  begin
    //clear to zero
    FillChar(vValue^, Size, 0);
  end;

  if IsNamed(OwnerSymbol.Module) then with OwnerSymbol.Module do
  begin
      vVaraibleEntry.Variable.Name := Pointer(UsedDataSize);
      //fill the variable name
      //aModule.AddByteToData(Length(Name));
      AddBufferToData(Name[1], Length(Name));
      AddByteToData(0);
  end;
end;

procedure TTurboVariableSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
var
  p: PInteger;
begin
  with aValue^ do
    p := Pointer(Integer(PTurboMethodSymbol(Symbol).Body.Memory) + Addr);
  p^ := Addr;
end;

{ TTurboTypeSymbol }
destructor TTurboTypeSymbol.Destroy;
begin
  MeFreeAndNil(FiType);
  Inherited;
end;

procedure TTurboTypeSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboTypeSymbol)) then
    with PTurboTypeSymbol(aSymbol)^ do
    begin
      Self.FEntryOffset := FEntryOffset;
      Self.FTypeInfo := FTypeInfo;
      Self.FiType := FiType;
      Self.FIsInternal := FIsInternal;
    end;
  Inherited;
end;

function TTurboTypeSymbol.iReferenceTo(const aSymbol: PTurboCustomSymbol): Integer; 
begin
  //TODO
  Result := cSymbolErrorOk;
end;

function TTurboTypeSymbol.iCompile: Integer;
begin
  Result := cSymbolErrorOk;
  if FIsInternal then Exit;
  if Assigned(FiType) and not Assigned(FTypeInfo) then
  begin
    if OwnerSymbol.Module.RegisteredTypes.RegisterType(FiType) then
    begin
      FTypeInfo:= FiType;
      FiType := nil;
    end
    else
      Result := cSymbolErrorRegisterType;
  end
  else
    Result := cSymbolErrorRedeclaration;
end;

procedure TTurboTypeSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec); 
begin
  //  Inherited;
end;

function TTurboTypeSymbol.GetEntry: PTurboTypeInfoEntry;
begin
  Result := Pointer(Integer(OwnerSymbol.Module.DataMemory) + FEntryOffset);
end;

function TTurboTypeSymbol.GetTypeInfo(): PMeType;
begin
  Result := GetTypeInfoEx();
end;

function TTurboTypeSymbol.GetTypeInfoEx(const aClass: TMeClass): PMeType;
begin
  if Assigned(FTypeInfo) then
    Result := FTypeInfo
  else if Assigned(FiType) then
    Result := FiType
  else if Assigned(aClass) and MeInheritsFrom(aClass, TypeOf(TMeType)) then
  begin
    Result := PMeType(NewMeObject(aClass));
    FiType := Result;
    if Assigned(OwnerSymbol) and Assigned(OwnerSymbol.Module) then
      Result.Owner := OwnerSymbol.Module.RegisteredTypes;
  end
  else 
    Result := nil;
end;

{ TTurboTypeRefSymbol }
procedure TTurboTypeRefSymbol.Assign(const aSymbol: PTurboCustomSymbol);
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboTypeRefSymbol)) then
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
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboStatementSymbol)) then
    with PTurboStatementSymbol(aSymbol)^ do
    begin
      Self.OwnerSymbol := OwnerSymbol;
    end;
  Inherited;
end;

{ TTurboOpStatementSymbol }
procedure TTurboOpStatementSymbol.Assign(const aSymbol: PTurboCustomSymbol); 
begin
  if Assigned(aSymbol) and aSymbol.InheritsFrom(TypeOf(TTurboOpStatementSymbol)) then
    with PTurboOpStatementSymbol(aSymbol)^ do
    begin
      Self.Instruction := Instruction;
      Self.Param := Param;
    end;
  Inherited;
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
    aModuleSymbol.OwnerSymbol := OwnerSymbol;
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

function TTurboModuleRefSymbols.FindMethodSymbol(const aName: string; const aModuleType: TTurboModuleType): PTurboCustomMethodSymbol;
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
function TTurboTypeSymbols.IndexOfSameAs(const aType: PMeType; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    with Items[Result]^ do
    if GetTypeInfo.IsSameAs(aType) then
      exit;
  end;
  Result := -1;
end;

function TTurboTypeSymbols.FindSameAs(const aType: PMeType): PTurboTypeSymbol;
var
  i: integer;
begin
  i := IndexOfSameAs(aType);
  if i >= 0 then
    Result := Items[i]
  else 
    Result := nil;
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
  SetMeVirtualMethod(TypeOf(TTurboSymbol), ovtVmtParent, TypeOf(TTurboCustomSymbol));
  SetMeVirtualMethod(TypeOf(TTurboLabelSymbol), ovtVmtParent, TypeOf(TTurboCustomSymbol));
  SetMeVirtualMethod(TypeOf(TTurboConstSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboVariableSymbol), ovtVmtParent, TypeOf(TTurboConstSymbol));

  SetMeVirtualMethod(TypeOf(TTurboCustomBlockSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboCustomMethodSymbol), ovtVmtParent, TypeOf(TTurboCustomBlockSymbol));
  SetMeVirtualMethod(TypeOf(TTurboMethodSymbol), ovtVmtParent, TypeOf(TTurboCustomMethodSymbol));
  SetMeVirtualMethod(TypeOf(TTurboMethodRefSymbol), ovtVmtParent, TypeOf(TTurboCustomMethodSymbol));

  SetMeVirtualMethod(TypeOf(TTurboModuleSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboTypeSymbol), ovtVmtParent, TypeOf(TTurboSymbol));
  SetMeVirtualMethod(TypeOf(TTurboTypeRefSymbol), ovtVmtParent, TypeOf(TTurboTypeSymbol));

  SetMeVirtualMethod(TypeOf(TTurboStatementSymbol), ovtVmtParent, TypeOf(TTurboCustomSymbol));
  SetMeVirtualMethod(TypeOf(TTurboOpStatementSymbol), ovtVmtParent, TypeOf(TTurboStatementSymbol));

  {$IFDEF MeRTTI_SUPPORT}
  //Make the ovtVmtClassName point to PShortString class name
  //SetMeVirtualMethod(TypeOf(TTurboCustomSymbol), ovtVmtClassName, @cMeObjectClassName);
  SetMeVirtualMethod(TypeOf(TTurboSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboLabelSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboConstSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboVariableSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboCustomBlockSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboCustomMethodSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboMethodSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboMethodRefSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboModuleSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboTypeSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboTypeRefSymbol), ovtVmtClassName, nil);

  SetMeVirtualMethod(TypeOf(TTurboStatementSymbol), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TTurboOpStatementSymbol), ovtVmtClassName, nil);
  {$ENDIF}
end.
