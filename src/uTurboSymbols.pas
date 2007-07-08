{: the turbo script symbols. }
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
  {: 导入方式}
  {
    misImport: 嵌入
    misUse: 引用, like DLL.
  }
  TTurboModuleImportStyle = (misNone, misImport, misUse);
  
  TTurboSymbol = class;
  TTurboCustomSymbol = class;
  TTurboTypeSymbol = class;
  TTurboModuleSymbol = class;
  TTurboProgramSymbol = class;
  TTurboSymbolList = class;
  TTurboConstSymbol = class;
  TTurboVariableSymbol = class;
  TTurboConstSymbolList = class;
  TTurboMethodSymbol = class;
  TTurboVariableSymbolList = class;
  TTurboWordSymbolList = class;
  {: the abstract symbol class. }
  { Description
  ~ 编译标识符
    可以分为标识符本体编译，和标识符的引用编译。
    常量除开字符串常量外，没有本体！
    变量的本体就是变量数据所在地址空间，引用编译则是将该数据地址编入代码区内存。
    过程的本体就是过程的代码，引用编译就是将过程的入口地址编译到指定的内存。
  }
  TTurboSymbol = class(TTurboCustomSymbol)
  private
    FOwnerSymbol: TTurboModuleSymbol;
  public
    function IsCompileAllowed: Boolean; override;
    procedure IsNamed(const aModule: TCustomTurboModule);
    procedure IsPublic(const aModule: TCustomTurboModule);
    procedure IsTyped(const aModule: TCustomTurboModule);
    property OwnerSymbol: TTurboModuleSymbol read FOwnerSymbol write
            FOwnerSymbol;
  end;

  {: the abstract symbol class. }
  { Description
  ~ 编译标识符
    可以分为标识符本体编译，和标识符的引用编译。
    常量除开字符串常量外，没有本体！
    变量的本体就是变量数据所在地址空间，引用编译则是将该数据地址编入代码区内存。
    过程的本体就是过程的代码，引用编译就是将过程的入口地址编译到指定的内存。
  }
  TTurboCustomSymbol = class(TObject)
  private
    FColumn: Integer;
    FLine: Integer;
    FName: string;
    FOnError: TTurboCompilerErrorEvent;
    FRefCount: Integer;
    FUnResolvedRefs: TList;
    function GetUnResolvedRefs: TList;
  protected
    procedure iCompile; virtual; abstract;
    procedure iReferenceTo; virtual; abstract;
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); virtual;
            abstract;
  public
    procedure Assign(const aSymbol: TTurboCustomSymbol);
    procedure Compile;
    procedure CompileError(const aErrCode: Integer);
    function IsCompileAllowed: Boolean; virtual;
    procedure ReferenceTo(const aSymbol: TurboSymbol);
    procedure ResolveRefs;
    property Column: Integer read FColumn write FColumn;
    property Line: Integer read FLine write FLine;
    property Name: string read FName write FName;
    property RefCount: Integer read FRefCount;
    property UnResolvedRefs: TList read GetUnResolvedRefs;
    property OnError: TTurboCompilerErrorEvent read FOnError write FOnError;
  end;

  TTurboTypeSymbol = class(TTurboSymbol)
  private
    FIsInternal: Boolean;
    FTypeInfo: PTurboTypeInfo;
  protected
    procedure iCompile; override;
    procedure iReferenceTo; override;
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); override;
  public
    property IsInternal: Boolean read FIsInternal;
    property TypeInfo: PTurboTypeInfo read FTypeInfo write FTypeInfo;
  end;

  TTurboModuleSymbol = class(TTurboSymbol)
  private
    FConstants: TTurboConstSymbolList;
    FMethods: TTurboWordSymbolList;
    FStaticFields: TTurboVariableSymbolList;
    FTypes: TList;
    FUsedModules: TTurboModuleSymbolList;
  protected
    FModule: TCustomTurboModule;
    procedure iCompile; override;
    procedure iReferenceTo; override;
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Constants: TTurboConstSymbolList read FConstants write FConstants;
    property Methods: TTurboWordSymbolList read FMethods write FMethods;
    property Module: TCustomTurboModule read FModule write FModule;
    property StaticFields: TTurboVariableSymbolList read FStaticFields write
            FStaticFields;
    property Types: TList read FTypes write FTypes;
    property UsedModules: TTurboModuleSymbolList read FUsedModules write
            FUsedModules;
  end;

  TTurboProgramSymbol = class(TTurboModuleSymbol)
  private
    FMaxMemorySize: Integer;
    FMaxMetaDataSize: Integer;
    FTurboGlobalOptions: TTurboGlobalOptions;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Compile;
    procedure Init;
    {: the Max FMemory Size. }
    property MaxMemorySize: Integer read FMaxMemorySize write FMaxMemorySize;
    {: the Max FDataMemory Size. }
    property MaxMetaDataSize: Integer read FMaxMetaDataSize write
            FMaxMetaDataSize;
  end;

  TTurboSymbolList = class(TList)
  private
    function GetItems(Index: Integer): TTurboSymbol;
  protected
    FOwner: TTurboSymbol;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(aOwner: TTurboSymbol); reintroduce;
    {: 返回供编译到内存的模块核。 }
    function GetModule: TCustomTurboModule;
    function IndexOf(const aName: String): Integer;
    property Items[Index: Integer]: TTurboSymbol read GetItems; default;
    property Owner: TTurboSymbol read FOwner write FOwner;
  end;

  {: the module constant symbol. }
  { Description
  对于字符串类型
   我是利用 VInteger 字段保存指向String 的DataMemory地址的。
  }
  TTurboConstSymbol = class(TTurboSymbol)
  private
    FSize: Integer;
    FTurboType: PMeType;
    FValue: TTurboValueRec;
    FValueStr: string;
    function AssignValue(const aValue: string; const aTypeKind:
            TTurboSimpleTypeKind = ttkUnknown): Boolean;
    procedure AssignValueTo(aMem: Pointer);
    procedure SaveString(const aModule: TCustomTurboModule);
    procedure SetTypeKind(aValue: TTurboSimpleTypeKind);
    procedure SetValue(const aValue: TTurboValueRec);
  protected
    {: 如果是字符串类型（序列类型）则将其编译入数据区。 }
    procedure iCompile; override;
    procedure iReferenceTo; override;
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); override;
  public
    procedure Assign(const aSymbol: TTurboCustomSymbol);
    property Size: Integer read FSize write FSize;
    property TurboType: PMeType read FTurboType write FTurboType;
    property Value: TTurboValueRec read FValue write SetValue;
    property ValueStr: string read FValueStr write FValueStr;
  end;

  {: the module variable symbol. }
  TTurboVariableSymbol = class(TTurboConstSymbol)
  private
    FAddr: tsInt;
  protected
    procedure iCompile; override;
    procedure iReferenceTo; override;
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); override;
  public
    property Addr: tsInt read FAddr write FAddr;
  end;

  TTurboConstSymbolList = class(TTurboSymbolList)
  private
    function GetItems(Index: Integer): TTurboConstSymbol;
  public
    {: if successful then return the index number, else return -1 means failed.
            }
    function Add(const aName, aValue: String): Integer;
    {: Add constant CFA To Code Mem. }
    function AddConstCFA(const aName: string): Boolean;
    function CreateConst(const aName, aValue: String): TTurboConstSymbol;
    function GetConstValueRec(const aTypeKind: TTurboSimpleTypeKind; const
            aName: string): TTurboValueRec;
    property Items[Index: Integer]: TTurboConstSymbol read GetItems; default;
  end;

  {: the turbo script virtual machine code symbol. }
  TTurboMethodSymbol = class(TTurboSymbol)
  private
    FCallStyle: TTurboCallStyle;
    FCFA: tsInt;
    FExternalOptions: TTurboExteralWordCFA;
    FModuleIndex: Integer;
    FModuleName: string;
    FModuleType: TTurboModuleType;
    FTyped: Boolean;
  protected
    FModule: TCustomTurboModule;
    procedure iCompileTo(const aModule: TCustomTurboModule); override;
    procedure iReferenceTo(const aMem: Pointer); override;
    procedure ResolveAddr(const aValue: PTurboUnResolvedRefRec); override;
  public
    constructor Create;
    destructor Destroy; override;
    property CallStyle: TTurboCallStyle read FCallStyle write FCallStyle;
    property CFA: tsInt read FCFA write FCFA;
    {: Exteral Word Options }
    property ExternalOptions: TTurboExteralWordCFA read FExternalOptions write
            FExternalOptions;
    {: for external forth word }
    property Module: TCustomTurboModule read FModule write FModule;
    {: for external word }
    property ModuleIndex: Integer read FModuleIndex write FModuleIndex;
    {: for external word }
    property ModuleName: string read FModuleName write FModuleName;
    {: for external word }
    property ModuleType: TTurboModuleType read FModuleType write FModuleType;
    {: 注明该标识符需要编入RTTI信息。 }
    property Typed: Boolean read FTyped write FTyped;
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

  {: keep the function body symbol. }
  TTurboWordSymbolList = class(TTurboSymbolList)
  private
    function GetItems(Index: Integer): TTurboMethodSymbol;
    function GetOwner: TTurboModuleSymbol;
  public
    function Add: TTurboMethodSymbol;
    function AddWordCFA(aName: string): Boolean;
    property Items[Index: Integer]: TTurboMethodSymbol read GetItems; default;
    property Owner: TTurboModuleSymbol read GetOwner;
  end;


implementation

//Const
  //cReqAlignMemTypes = [ttkUWord, ttkSWord, ttkULong, ttkSLong, ttkPointer, ttkString, ttkLString, ttkInt64, ttkQWord];


{
********************************* TTurboSymbol *********************************
}
function TTurboSymbol.IsCompileAllowed: Boolean;
begin
end;

procedure TTurboSymbol.IsNamed(const aModule: TCustomTurboModule);
begin
end;

procedure TTurboSymbol.IsPublic(const aModule: TCustomTurboModule);
begin
end;

procedure TTurboSymbol.IsTyped(const aModule: TCustomTurboModule);
begin
end;

{
****************************** TTurboCustomSymbol ******************************
}
procedure TTurboCustomSymbol.Assign(const aSymbol: TTurboCustomSymbol);
begin
end;

procedure TTurboCustomSymbol.Compile;
begin
end;

procedure TTurboCustomSymbol.CompileError(const aErrCode: Integer);
begin
end;

function TTurboCustomSymbol.GetUnResolvedRefs: TList;
begin
end;

function TTurboCustomSymbol.IsCompileAllowed: Boolean;
begin
end;

procedure TTurboCustomSymbol.ReferenceTo(const aSymbol: TurboSymbol);
begin
end;

procedure TTurboCustomSymbol.ResolveRefs;
begin
end;

{
******************************* TTurboTypeSymbol *******************************
}
procedure TTurboTypeSymbol.iCompile;
begin
end;

procedure TTurboTypeSymbol.iReferenceTo;
begin
end;

procedure TTurboTypeSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
begin
end;

{
****************************** TTurboModuleSymbol ******************************
}
constructor TTurboModuleSymbol.Create;
begin
  inherited Create;
  FConstants := TTurboConstSymbolList.Create(Self);
  FStaticFields := TTurboVariableSymbolList.Create(Self);
  FMethods     := TTurboWordSymbolList.Create(Self);
end;

destructor TTurboModuleSymbol.Destroy;
begin
  FreeAndNil(FConstants);
  FreeAndNil(FStaticFields);
  FreeAndNil(FMethods);
  inherited Destroy;
end;

procedure TTurboModuleSymbol.iCompile;
begin
end;

procedure TTurboModuleSymbol.iReferenceTo;
begin
end;

procedure TTurboModuleSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
begin
end;

{
***************************** TTurboProgramSymbol ******************************
}
constructor TTurboProgramSymbol.Create;
begin
  inherited Create;
  FModule := TCustomTurboModule.Create;
  FModule.GlobalOptions := @FTurboGlobalOptions;
end;

destructor TTurboProgramSymbol.Destroy;
begin
  FreeAndNil(FModule);
  inherited Destroy;
end;

procedure TTurboProgramSymbol.Compile;
begin
end;

procedure TTurboProgramSymbol.Init;
begin
  with FModule do
  begin
    ClearMemory;
    MemorySize     := MaxMemorySize;
    DataMemorySize := MaxMetaDataSize;
    Status := [psCompiling];
    FConstants.Clear;
    FVariables.Clear;
    FWords.Clear;
  end;
end;

{
******************************* TTurboSymbolList *******************************
}
constructor TTurboSymbolList.Create(aOwner: TTurboSymbol);
begin
  inherited Create;
  FOwner := aOwner;
end;

function TTurboSymbolList.GetItems(Index: Integer): TTurboSymbol;
begin
  Result := TTurboSymbol(Get(Index));
end;

function TTurboSymbolList.GetModule: TCustomTurboModule;
var
  vSymbol: TTurboModuleSymbol;
begin
  Result := nil;
  if Owner is TTurboModuleSymbol then
  begin
    vSymbol := TTurboModuleSymbol(Owner);
    while Assigned(vSymbol) and (vSymbol.ImportStyle <> misNone) do
    begin
      if vSymbol.Parent is TTurboModuleSymbol then
      begin
        vSymbol := TTurboModuleSymbol(vSymbol.Parent);
      end
      else
      begin
        exit;
      end;
    end; //while
    if Assigned(vSymbol) then
      Result := vSymbol.Module;
  end;
end;

function TTurboSymbolList.IndexOf(const aName: String): Integer;
begin
  For Result := 0 to Count - 1 do
  begin
    if aName = Items[Result].Name then
      exit;
  end;
  Result := -1;
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
procedure TTurboConstSymbol.Assign(const aSymbol: TTurboCustomSymbol);
begin
  inherited Assign(aSymbol);
end;

function TTurboConstSymbol.AssignValue(const aValue: string; const aTypeKind:
        TTurboSimpleTypeKind = ttkUnknown): Boolean;
begin
  Result := True;
  //writeln('AssignValue:', Integer(aTypeKind));
  if aValue[1] = '''' then
  begin
    FValueStr := AnsiDequotedStr(aValue, '''');
    if aTypeKind = ttkUnknown then
    begin
       TypeKind := ttkString;
      {if Length(ValueStr) < 256 then
      begin
        TypeKind := ttkString;
      end
      else begin
        TypeKind := ttkLString;
      end;}
    end
    else if aTypeKind = ttkChar then
      FValue.VByte := Ord(ValueStr[1]);
  end
  else
    try
      FValueStr := aValue;
      FValue.VInt64 := StrToInt64(aValue);
      //writeln(Name,'=',Value.VByte);
      if aTypeKind = ttkUnknown then
      begin
        if (Value.VInt64 >= Low(ShortInt)) and (Value.VInt64<=High(ShortInt)) then
          FTurboType := ttkSByte
        else if (Value.VInt64 >= Low(Byte)) and (Value.VInt64<=High(Byte)) then
          FTurboType := ttkUByte
        else if (Value.VInt64 >= Low(SmallInt)) and (Value.VInt64<=High(SmallInt)) then
          FTurboType := ttkSWord
        else if (Value.VInt64 >= Low(Word)) and (Value.VInt64<=High(Word)) then
          FTurboType := ttkUWord
        else if (Value.VInt64 >= Low(LongInt)) and (Value.VInt64<=High(LongInt)) then
          FTurboType := ttkSLong
        else if (Value.VInt64 >= Low(LongWord)) and (Value.VInt64<=High(LongWord)) then
          FTurboType := ttkULong
        else //if (Value.VInt64 >= Low(Int64)) and (Value.VInt64<=High(Int64)) then
          FTurboType := ttkInt64;
      end;
    except
      FTurboType := ttkUnknown;
      Result := False;
    end;
  if aTypeKind <> ttkUnknown then begin
    FTurboType := aTypeKind;
    Result := True;
  end;
  if Result then
    FSize := GetSimpleTurboTypeSize(TypeKind);
end;

procedure TTurboConstSymbol.AssignValueTo(aMem: Pointer);
begin
  Result := True;
  //writeln('AssignValueTo:', ValueStr);
  move(FValue, aMem^, Size);
  //writeln(InttoHex(Value.VInteger, 4));
  {case TypeKind of
    ttkSLong, ttkInterface, ttkProcedure, ttkPointer, ttkString, ttkLString: PInteger(aMem)^ := Value.VInteger;
    ttkSByte: PShortInt(aMem)^  := Value.VShortInt;
    ttkUByte, ttkChar, ttkSet: PByte(aMem)^ := Value.VByte;
    ttkSWord:PSmallInt(aMem)^ := Value.VSmallInt;
    ttkUWord: PWord(aMem)^ := Value.VWord;
    ttkULong: PLongWord(aMem)^ := Value.VLongword;
    ttkQWord, ttkInt64: PInt64(aMem)^:= Value.VInt64;
    else
      Result := False;
  end;//}
  //writeln('TypeKind=', Integer(TypeKind));
  //writeln('PSource=', PInteger(aMem)^);
end;

procedure TTurboConstSymbol.iCompile;
begin
  inherited iCompile;
end;

procedure TTurboConstSymbol.iReferenceTo;
begin
  AssignValueTo(aMem);
end;

procedure TTurboConstSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
begin
end;

procedure TTurboConstSymbol.SaveString(const aModule: TCustomTurboModule);
begin
  Case TypeKind of
    ttkString: begin
      FValue.VInteger := aModule.UsedDataSize;
      aModule.AddByteToData(Length(FValueStr));
      aModule.AddBufferToData(FValueStr[1], Length(FValueStr));
    end;
    ttkLString: begin
      aModule.AddIntToData(-1);
      aModule.AddIntToData(Length(FValueStr));
      FValue.VInteger := aModule.UsedDataSize;
      aModule.AddBufferToData(FValueStr[1], Length(FValueStr));
      aModule.AddByteToData(0);
    end;
  end;//case
end;

procedure TTurboConstSymbol.SetTypeKind(aValue: TTurboSimpleTypeKind);
begin
  FTurboType := aValue;
  FSize := GetSimpleTurboTypeSize(FTurboType);
end;

procedure TTurboConstSymbol.SetValue(const aValue: TTurboValueRec);
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
***************************** TTurboVariableSymbol *****************************
}
procedure TTurboVariableSymbol.iCompile;
var
  vVaraibleEntry: PTurboVariableEntry;
  vValue: Pointer;
begin
  if Visibility >= fvProtected then
  begin
    Integer(vVaraibleEntry) := Integer(aModule.DataMemory) + aModule.UsedDataSize;
    aModule.AllocDataSpace(SizeOf(TTurboVariableEntry));
    vVaraibleEntry.Prior := aModule.LastVariableEntry;
    vVaraibleEntry.Variable.Size := Size;
    vVaraibleEntry.Variable.Addr := nil;
    vVaraibleEntry.Variable.TypeInfo := nil;
  end;

  if (TypeKind in cReqAlignMemTypes) then aModule.AlignData;
  Addr := aModule.UsedDataSize;
  if Visibility >= fvProtected then
  begin
    Integer(vVaraibleEntry.Variable.Addr) := Addr;
    aModule.LastVariableEntry := Pointer(Integer(vVaraibleEntry) - Integer(aModule.DataMemory));
  end;
  Integer(vValue) := Integer(aModule.DataMemory)  + Addr;
  aModule.AllocDataSpace(Size);
  if ValueStr <> '' then
  begin
    SaveString(aModule); //if this is string
    AssignValueTo(vValue);
  end;

  if Visibility >= fvPublished then
  begin
    vVaraibleEntry.Variable.Name := Pointer(aModule.UsedDataSize);
    //fill the variable name
    aModule.AddByteToData(Length(Name));
    aModule.AddBufferToData(Name[1], Length(Name));
  end
  else if Visibility >= fvProtected then
  begin
    //no name
    vVaraibleEntry.Variable.Name := nil;
  end;
end;

procedure TTurboVariableSymbol.iReferenceTo;
begin
  PtsInt(aMem)^ := Addr;
end;

procedure TTurboVariableSymbol.ResolveAddr(const aValue:
        PTurboUnResolvedRefRec);
begin
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

function TTurboConstSymbolList.AddConstCFA(const aName: string): Boolean;
var
  I: Integer;
  p: Pointer;
  vModule: TCustomTurboModule;
begin
  I := IndexOf(aName);
  Result := I >= 0;
  if Result then
  begin
    vModule := GetModule;
    Result := Assigned(vModule);
    if Result then with Items[I] do
    begin
      if Size <= SizeOf(Integer) then
      begin
        //vModule.AddOpToMem(opPushInt);
        //Size := SizeOf(Integer);
        Case Size of
          SizeOf(Byte): vModule.AddOpToMem(opPushByte);
          SizeOf(Word): vModule.AddOpToMem(opPushWord);
          //SizeOf(tsInt): vModule.AddOpToMem(opPushInt);
          //SizeOf(Int64): vModule.AddOpToMem(opPushInt64);
          else
          begin
            vModule.AddOpToMem(opPushInt);
            Size := SizeOf(Integer);
          end;
        end;//case
      end
      else
        vModule.AddOpToMem(opPushInt64);
      Integer(p) := Integer(vModule.Memory) + vModule.UsedMemory;
      vModule.AllocSpace(Size);
      ReferenceTo(p);
    end
  end;
end;

function TTurboConstSymbolList.CreateConst(const aName, aValue: String):
        TTurboConstSymbol;
begin
  Result := TTurboConstSymbol.Create;
  Result.Parent := Owner;
  Result.Name  := aName;
  Result.AssignValue(aValue);
end;

function TTurboConstSymbolList.GetConstValueRec(const aTypeKind:
        TTurboSimpleTypeKind; const aName: string): TTurboValueRec;
var
  I: Integer;
begin
  for i := 0 to Count -1 do
    with Items[i] do
    if (aName = Name) and (aTypeKind = TypeKind) then
    begin
      Result := Value;
      exit;
    end;

  Raise Exception.Create('Unknown typed Constant name:'+ aNmae);
end;

function TTurboConstSymbolList.GetItems(Index: Integer): TTurboConstSymbol;
begin
  Result := TTurboSymbol(Get(Index));
end;

{
****************************** TTurboMethodSymbol ******************************
}
constructor TTurboMethodSymbol.Create;
var
  vProg: TTurboProgramSymbol;
begin
  inherited Create;
  {FBody := TCustomTurboModule.Create;
  vProg := ProgramSymbol;
  if Assigned(vProg) then
  begin
    FBody.GlobalOptions := vProg.FTurboGlobalOptions;
  end;
  FBody.Status := [psCompiling];
  }
  //all words must be compiled!
  FRefs := 1;
end;

destructor TTurboMethodSymbol.Destroy;
begin
  //FreeAndNil(FBody);
  inherited Destroy;
end;

procedure TTurboMethodSymbol.iCompileTo(const aModule: TCustomTurboModule);
var
  I: Integer;
  vName: string;
begin
  if IsExternal then
  begin
    Case CallStyle of
      csForth:
      begin
        if Assigned(Module) then
        begin
          vName := Name;
          if ExternalOptions.Name <> '' then vName := ExternalOptions.Name;
          CFA := Module.GetWordCFA(vName);
          if CFA = -1 then
            //SynError(cWordNotFoundError, '"'+ vName + '" CFA not in ' + Module.Name);
            raise Exception.Create('"'+ vName + '" CFA not in ' + Module.Name);
        end
        else
          //SynError(cWordNotFoundError, '"'+ vName + '" not in ' + Module.Name);
          raise Exception.Create('"'+ vName + '" not in ' + Module.Name);
      end;
    End; //Case
  end
  else begin
    //the word CFA
    CFA := aModule.UsedMemory;
    //aModule.AddBufferToMem(Body.Memory^, Body.UsedMemory);
  end;
end;

procedure TTurboMethodSymbol.iReferenceTo(const aMem: Pointer);
begin
  Move(FCFA, aMem^, SizeOf(Addr));
end;

procedure TTurboMethodSymbol.ResolveAddr(const aValue: PTurboUnResolvedRefRec);
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

{
***************************** TTurboWordSymbolList *****************************
}
function TTurboWordSymbolList.Add: TTurboMethodSymbol;
begin
  Result := TTurboMethodSymbol.Create;
  Result.Parent := TTurboModuleSymbol(FOwner);
  inherited Add(Result);
end;

function TTurboWordSymbolList.AddWordCFA(aName: string): Boolean;
var
  I: Integer;
  vModule: TCustomTurboModule;
  p: Pointer;
begin
  I := indexOf(aName);
  Result := I >= 0;
  if Result then
  begin
    vModule := GetModule;
    Result := Assigned(vModule);
    if Result then with Items[i] do
    begin
      if not IsExternal then
      begin
        if Visibility < fvProtected then
        begin
          vModule.AddOpToMem(opEnter);
          //vModule.AddIntToMem(CFA);
        end
        else
        begin
          vModule.AddOpToMem(opEnterFar);
          vModule.AddIntToMem(0);
          //vModule.AddIntToMem(CFA);
        end;
        p := Pointer(Integer(vModule.Memory) + vModule.UsedMemory);
        vModule.AddIntToMem(0);
        ReferenceTo(p);
        //Result := True;
      end
      else
      begin
        case CallStyle of
          csForth:
          begin
            vModule.AddOpToMem(opCallFar);
            vModule.AddIntToMem(Integer(FUsedModules[ModuleIndex].Entry)+SizeOf(Pointer)-Integer(vModule.DataMemory));
            p := Pointer(Integer(vModule.Memory) + vModule.UsedMemory);
            vModule.AddIntToMem(0);
            ReferenceTo(p);
          end
          else
            Result := False;
        end; //case
      end;
    end;
  end;
end;

function TTurboWordSymbolList.GetItems(Index: Integer): TTurboMethodSymbol;
begin
  Result := TTurboMethodSymbol(inherited Get(Index));
end;

function TTurboWordSymbolList.GetOwner: TTurboModuleSymbol;
begin
  Result := TTurboModuleSymbol(FOwner);
end;



end.
