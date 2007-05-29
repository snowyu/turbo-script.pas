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
  TTurboTypeSymbol = class;
  TTurboModuleSymbol = class;
  TTurboProgramSymbol = class;
  TTurboSymbolList = class;
  TTurboConstSymbol = class;
  TTurboVariableSymbol = class;
  TTurboConstSymbolList = class;
  TTurboWordSymbol = class;
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
  TTurboSymbol = class(TObject)
  private
    FCaption: string;
    FDescription: string;
    FIsCompiled: Boolean;
    FIsExternal: Boolean;
    FName: string;
    FParent: TObject;
    FRefs: LongInt;
    FVisibility: TTurboVisibility;
    function GetProgramSymbol: TTurboProgramSymbol;
  protected
    {: 没有解决引用地址的列表 }
    FUnResolvedRefs: TList;
    {: : 将标识符本体编译（添加到）到指定的模块。 }
    procedure iCompileTo(const aModule: TCustomTurboModule); virtual; abstract;
    {: 将标识符引用编译（添加到）到指定的地址 }
    { Description
    注意你必须保证有足够的空间容纳该地址。
    }
    procedure iReferenceTo(const aMem: Pointer); virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {: :将标识符本体编译（添加到）到指定的模块。 }
    { Description
    只有 Refs 引用计数大于0 的才会被编译。
    }
    procedure CompileTo(const aModule: TCustomTurboModule);
    {: 将标识符引用编译（添加到）到指定的地址 }
    { Description
    注意你必须保证有足够的空间容纳该地址。
    }
    procedure ReferenceTo(const aMem: Pointer);
    {: 解决所有未填充的引用地址。 }
    { Description
    注意，必须编译本体才能确定应用地址。
    IsCompiled 必须为真.
    }
    procedure ResolveRefs;
    {: the short comment(one line) for the symbol. }
    property Caption: string read FCaption write FCaption;
    {: the long comment(multi-line) for the symbol. }
    property Description: string read FDescription write FDescription;
    {: 标识符主体是否已经被编译进入内存。 }
    property IsCompiled: Boolean read FIsCompiled;
    property IsExternal: Boolean read FIsExternal write FIsExternal;
    {: the symbol name }
    property Name: string read FName write FName;
    property Parent: TObject read FParent write FParent;
    property ProgramSymbol: TTurboProgramSymbol read GetProgramSymbol;
    {: Indicates how many times this symbol is refered in the parsed code for
            compiling only. }
    { Description
    }
    property Refs: LongInt read FRefs write FRefs;
    property Visibility: TTurboVisibility read FVisibility write FVisibility;
  end;

  TTurboTypeSymbol = class(TTurboSymbol)
  private
    FTypeInfo: PTurboTypeInfo;
  public
    property TypeInfo: PTurboTypeInfo read FTypeInfo write FTypeInfo;
  end;

  TTurboModuleSymbol = class(TTurboSymbol)
  private
    FConstants: TTurboConstSymbolList;
    FImportStyle: TTurboModuleImportStyle;
    FUsedModules: TTurboModuleSymbolList;
    FVariables: TTurboVariableSymbolList;
    FWords: TTurboWordSymbolList;
  protected
    FModule: TCustomTurboModule;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Constants: TTurboConstSymbolList read FConstants write FConstants;
    property ImportStyle: TTurboModuleImportStyle read FImportStyle write
            FImportStyle;
    property Module: TCustomTurboModule read FModule write FModule;
    property UsedModules: TTurboModuleSymbolList read FUsedModules write
            FUsedModules;
    property Variables: TTurboVariableSymbolList read FVariables write
            FVariables;
    property Words: TTurboWordSymbolList read FWords write FWords;
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
    FTypeKind: TTurboSimpleTypeKind;
    FValue: TTurboValueRec;
    FValueStr: string;
    procedure SetValue(const aValue: TTurboValueRec);
  protected
    {: 如果是字符串类型（序列类型）则将其编译入数据区。 }
    procedure iCompileTo(const aModule: TCustomTurboModule); override;
    procedure iReferenceTo(const aMem: Pointer); override;
  public
    {: 根据aValue 如果aTypeKind is ttkUnknown 那么会自动判断其类型 }
    function AssignValue(const aValue: string; const aTypeKind:
            TTurboSimpleTypeKind = ttkUnknown): Boolean;
    procedure AssignValueTo(aMem: Pointer);
    procedure SaveString(const aModule: TCustomTurboModule);
    procedure SetTypeKind(aValue: TTurboSimpleTypeKind);
    property Size: Integer read FSize write FSize;
    property TypeKind: TTurboSimpleTypeKind read FTypeKind write FTypeKind;
    property Value: TTurboValueRec read FValue write SetValue;
    property ValueStr: string read FValueStr write FValueStr;
  end;

  {: the module variable symbol. }
  TTurboVariableSymbol = class(TTurboConstSymbol)
  private
    FAddr: tsInt;
    FTyped: Boolean;
  protected
    procedure iCompileTo(const aModule: TCustomTurboModule); override;
    procedure iReferenceTo(const aMem: Pointer); override;
  public
    property Addr: tsInt read FAddr write FAddr;
    {: 注明该标识符需要编入RTTI信息。 }
    property Typed: Boolean read FTyped write FTyped;
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
  TTurboWordSymbol = class(TCustomTurboWordSymbol)
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
  public
    constructor Create; override;
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
    function GetItems(Index: Integer): TTurboWordSymbol;
    function GetOwner: TTurboModuleSymbol;
  public
    function Add: TTurboWordSymbol;
    function AddWordCFA(aName: string): Boolean;
    property Items[Index: Integer]: TTurboWordSymbol read GetItems; default;
    property Owner: TTurboModuleSymbol read GetOwner;
  end;


implementation

//Const
  //cReqAlignMemTypes = [ttkUWord, ttkSWord, ttkULong, ttkSLong, ttkPointer, ttkString, ttkLString, ttkInt64, ttkQWord];


{
********************************* TTurboSymbol *********************************
}
constructor TTurboSymbol.Create;
begin
  inherited Create;
  FUnResolvedRefs := TList.Create;
end;

destructor TTurboSymbol.Destroy;
begin
  FreeAndNil(FUnResolvedRefs);
  inherited Destroy;
end;

procedure TTurboSymbol.CompileTo(const aModule: TCustomTurboModule);
begin
  if (FVisibility = fvPublished) or (FRefs > 0) then
  begin
    iCompileTo(aModule);
    FIsCompiled := True;
  end;
end;

function TTurboSymbol.GetProgramSymbol: TTurboProgramSymbol;
begin
  Result := TTurboProgramSymbol(FParent);
  while Assigned(Result) do
  begin
    if Result is TTurboProgramSymbol then exit;
    Result := TTurboProgramSymbol(Result.Parent);
  end;
  Result := nil;
end;

procedure TTurboSymbol.ReferenceTo(const aMem: Pointer);
begin
  Inc(FRefs);
  if FIsCompiled then
  begin
    iReferenceTo(aMem);
  end
  else
    FUnResolvedRefs.Add(aMem);
end;

procedure TTurboSymbol.ResolveRefs;
var
  I: Integer;
begin
  if FIsCompiled then
    with FUnResolvedRefs do
    begin
      for i := 0 to Count -1 do
      begin
        ReferenceTo(Items[i]);
      end;
      Clear;
    end;
end;

{
****************************** TTurboModuleSymbol ******************************
}
constructor TTurboModuleSymbol.Create;
begin
  inherited Create;
  FConstants := TTurboConstSymbolList.Create(Self);
  FVariables := TTurboVariableSymbolList.Create(Self);
  FWords     := TTurboWordSymbolList.Create(Self);
end;

destructor TTurboModuleSymbol.Destroy;
begin
  FreeAndNil(FConstants);
  FreeAndNil(FVariables);
  FreeAndNil(FWords);
  inherited Destroy;
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
          FTypeKind := ttkSByte
        else if (Value.VInt64 >= Low(Byte)) and (Value.VInt64<=High(Byte)) then
          FTypeKind := ttkUByte
        else if (Value.VInt64 >= Low(SmallInt)) and (Value.VInt64<=High(SmallInt)) then
          FTypeKind := ttkSWord
        else if (Value.VInt64 >= Low(Word)) and (Value.VInt64<=High(Word)) then
          FTypeKind := ttkUWord
        else if (Value.VInt64 >= Low(LongInt)) and (Value.VInt64<=High(LongInt)) then
          FTypeKind := ttkSLong
        else if (Value.VInt64 >= Low(LongWord)) and (Value.VInt64<=High(LongWord)) then
          FTypeKind := ttkULong
        else //if (Value.VInt64 >= Low(Int64)) and (Value.VInt64<=High(Int64)) then
          FTypeKind := ttkInt64;
      end;
    except
      FTypeKind := ttkUnknown;
      Result := False;
    end;
  if aTypeKind <> ttkUnknown then begin
    FTypeKind := aTypeKind;
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

procedure TTurboConstSymbol.iCompileTo(const aModule: TCustomTurboModule);
begin
  inherited iCompileTo(aModule);
  SaveString(aModule);
end;

procedure TTurboConstSymbol.iReferenceTo(const aMem: Pointer);
begin
  AssignValueTo(aMem);
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
  FTypeKind := aValue;
  FSize := GetSimpleTurboTypeSize(FTypeKind);
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
procedure TTurboVariableSymbol.iCompileTo(const aModule: TCustomTurboModule);
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

procedure TTurboVariableSymbol.iReferenceTo(const aMem: Pointer);
begin
  PtsInt(aMem)^ := Addr;
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
******************************* TTurboWordSymbol *******************************
}
constructor TTurboWordSymbol.Create;
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

destructor TTurboWordSymbol.Destroy;
begin
  //FreeAndNil(FBody);
  inherited Destroy;
end;

procedure TTurboWordSymbol.iCompileTo(const aModule: TCustomTurboModule);
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

procedure TTurboWordSymbol.iReferenceTo(const aMem: Pointer);
begin
  Move(FCFA, aMem^, SizeOf(Addr));
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
function TTurboWordSymbolList.Add: TTurboWordSymbol;
begin
  Result := TTurboWordSymbol.Create;
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

function TTurboWordSymbolList.GetItems(Index: Integer): TTurboWordSymbol;
begin
  Result := TTurboWordSymbol(inherited Get(Index));
end;

function TTurboWordSymbolList.GetOwner: TTurboModuleSymbol;
begin
  Result := TTurboModuleSymbol(FOwner);
end;



end.
