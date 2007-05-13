{: the turboScript basic metadata for type, constants etc. }
{ Description
这些object 都是供对内存进行 typecast的。

仅供运行时使用，MetaInfo object 使用绝对地址！
}
unit uTurboMetaInfo;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , TypInfo
  , uTurboConsts
  ;

type
  PTurboMetaInfo       = ^ TTurboMetaInfo;
  PTurboModuleRefInfo  = ^ TTurboModuleRefInfo;
  PTurboTypeInfo       = ^ TTurboTypeInfo;
  PTurboOrdinalType    = ^ TTurboOrdinalType; 
  PTurboSetType        = ^ TTurboSetType; 

  PTurboVariableEntry = ^ TTurboVariableEntry;
  PTurboModuleRefEntry = ^ TTurboModuleRefEntry;
  PTurboMethodEntry = ^ TTurboMethodEntry;
  PTurboTypeInfoEntry = ^ TTurboTypeInfoEntry;

   { Description
       @param tfAddrResolved  the address resolved or not
       @param tfInited        the module(static class) initialized or not.
   }
   TTurboModuleFlag = (tfAddrResolved, tfInited);
   TTurboModuleFlags = set of TTurboModuleFlag;

  //the basis metaKind
  TTurboMetaKind = (tmkUnknown
    , tmkClass
    , tmkType //类型其实是从Class上派生出来，一切都是Class了
    , tmkMethod  //Method
    , tmkVariable
    , tmkConstant
    , tmkStatement //new statement structrue.
    , tmkModule
  );
  //the basis TypeKind
  //定义最基本的类型. 所有的用户扩展类型，全部都是由下面的基本类型派生的！！
  TTurboTypeKind = (ttkUnknown
    , ttkOrdinal, ttkSet
    , ttkInt64
    , ttkSequence
    , ttkArray
    , ttkRecord
  );

  //the abstract MetaInfo 
  TTurboMetaInfo = object
  protected
    //如果使用VMT我可以把该字段作为指向VMT的指针！当装入后
    FMetaKind: TTurboMetaKind;
    FIsExternal: Boolean;
    //Priority not used yet.
    FPriority: TTurboPriority;
    FVisibility: TTurboVisibility;
    FParamFieldLength: tsUInt; 
    FName: PChar;  
  public
    //this is an external define.
    //准备废弃: 所有引用的放入模块中。
    property IsExternal: Boolean read FIsExternal write FIsExternal;
    property MetaKind: TTurboMetaKind read FMetaKind write FMetaKind;
    property Name: PChar read FName write FName;
    property ParamFieldLength: tsUInt read FParamFieldLength write FParamFieldLength;
    property Visibility: TTurboVisibility read FVisibility write FVisibility;
  end;  

  PTurboExteralMethodOptions = ^ TTurboExteralMethodOptions;
  //cfsHostFunction, cfsDLLFunction
  TTurboExteralMethodOptions = packed record
    //if it csForth then it is the CFA.
    ProcAddr: Pointer; 
    ModuleRef: PTurboModuleRefInfo;
    ProcTypeEntry: Pointer; 
    //-1 means non-index visits.
    Index: tsInt;  
    //packed the function name in the DLL/Host.
    Name: ShortString; 
  end;

  TTurboMethodInfo = object(TTurboMetaInfo)
  public
    //RVA the offset address of the code-memory.
    CFA: tsInt;
    CallStyle: TTurboCallStyle;
    CodeFieldStyle: TTurboCodeFieldStyle;
    //ExternalWordOptions: TTurboExteralWordOptions;
    function GetExternalOptionsAddr: PTurboExteralMethodOptions; 
  end;
  //this for external word only
  TTurboMethodInfoEx = object(TTurboMethodInfo)
  protected
    ExternalOptions: TTurboExteralMethodOptions;
  end; 

  //it's the Module Variable Info too: TTurboVariableInfo
  TTurboStaticFieldInfo = object(TTurboMetaInfo)
  public
    Size: tsInt;
    Addr: Pointer; //offset address of the FMemory.
    TypeInfo: PTurboTypeInfo; //TODO: relocate it.
    //Value: ....
  end;

  PTurboStaticFieldEntry = ^ TTurboStaticFieldEntry;
  TTurboStaticFieldEntry = packed record
    Prior: PTurboStaticFieldEntry; //前一个, 0 means 为最前面。
    Field: TTurboStaticFieldInfo;
  end;
{ //the Future Version supports
  PTurboStaticFieldList = ^TTurboStaticFieldList; 
  TTurboStaticFieldList = object //cast Mem
  public
    Count: tsUInt; //if Count = 0 then no fields
    Fields: array[0..1023] of TTurboStaticFieldInfo; //array[0..Count-1] of
    //UserDefinedFieldsEntry: PTurboStaticFieldEntry; //if no it's nill.
  end;
}
  TTurboStaticClassInfo = object(TTurboMetaInfo)
  public
    Parent: Pointer;
    //声明的静态字段列表
    //StaticFieldListEntry: PTurboStaticFieldList;
    StaticFieldCount: tsInt; //Preserved! not used
    LastStaticFieldEntry: PTurboStaticFieldEntry; 
    //声明的方法列表
    //the first two mmethod item is : cctor(Class Constructor), cdtor((Class Destructor)) 
    MethodListEntry: Pointer;   
    //声明的类型Meta信息列表
    TypeInfoListEntry: Pointer; 
  end; 

  TTurboModuleRefInfo = object(TTurboMetaInfo)
  public
    ModuleType: TTurboModuleType;
    {: the module handle.
      DLL Module(mtDLL): it's the DLL handle.
      Host Module(mtHost):
      ForthLib module(mtLib): loaded the instance of TCustomTurboModule.
      nil means not assigned(or loaded).
    }
    Handle: Pointer; 
    Revision: LongWord; //the module version
    BuildDate: TTimeStamp;
  end;

  TTurboTypeInfo = object(TTurboMetaInfo)
  public
    TypeKind: TTurboTypeKind;
  end;

  TTurboCustomOrdinalType = object(TTurboTypeInfo)
  public
    {
     @param otSByte: Signed byte
     @param otUByte: Unsigned byte
     @param otSWord: Signed word
     @param otUWord: Unsigned word
     @param otSLong: Signed longword
     @param otULong: Unsigned longword
    }
    OrdType: TOrdType;
  public
  end;

  {: the abstract common Ordinal type info object. }
  {Ordinal: tkInteger, tkChar, tkEnumeration, tkWChar}
  TTurboOrdinalType = object(TTurboCustomOrdinalType)
  protected
    FMinValue: tsInt;
    FMaxValue: tsInt;
  public
    Property MinValue: tsInt read FMinValue write FMinValue;
    Property MaxValue: tsInt read FMaxValue write FMaxValue;
  end; 

  TTurboSetType = object(TTurboCustomOrdinalType)
  public
    //指向该集合是由什么Ordinal类型构成的。
    CompType: PTurboOrdinalType;
  end; 

  {: the Enumeration type info object. }
  TTurboEnumerationType = Object(TTurboOrdinalType)
  protected
    FBaseType: PTurboTypeInfo;
    {NameList: array[0..Count-1] of packed ShortString;
      Packed ShortString
      ...(Count)
      Packed ShortString
    }
    FNameList: Pointer;
    //if ValueList is nil then the index No. of nameList is the value.
    {ValueList: array[0..Count-1] of Integer.
      Integer
      ...(Count)
      Integer
    }
    FValueList: Pointer;
  private 
    function GetEnumIndex(const aName: string): Integer;
  protected
    function GetEnumValue(const aName: string): Integer;
    function GetEnumName(Value: tsInt): PShortString;
  public
    //return the elemnet count
    function Count: Integer; //=FMaxValue - FMinValue + 1
  public
    Property BaseType: PTurboTypeInfo read FBaseType write FBaseType;
    Property EnumName[Index: tsInt]: PShortString read GetEnumName;
    Property EnumValue[const aName: string]: Integer read GetEnumValue;
  end; 

  TTurboVariableInfo = object(TTurboMetaInfo)
  public
    Size: tsInt;
    Addr: Pointer; //offset address of the FMemory.
    TypeInfo: PTurboTypeInfo; //TODO: relocate it.
    //Value: ....
  end;

  //For type-cast the Mem
  TTurboMethodEntry = packed record
    Prior: PTurboMethodEntry; //前一个单词 0 means 为最前面。
    Word: TTurboMethodInfo;
  end;

  TTurboVariableEntry = packed record
    Prior: PTurboVariableEntry;
    Variable: TTurboVariableInfo; 
    //Value: ....
  end;
  
  //the import module for uses.
  TTurboModuleRefEntry = packed record
    Prior: PTurboModuleRefEntry; //nil means no more
    Module: TTurboModuleRefInfo;
  end;

  TTurboTypeInfoEntry = packed record
    Prior: PTurboTypeInfoEntry; //nil means no more
    TypeInfo: TTurboTypeInfo;
  end;
{
  TTurboVirtualMethodTable = record
    Init: Pointer; //= Initialization
    Destroy: Pointer; //= fiinalizaiotn
    ParentClass: Pointer;
    ClassName: PShortString;
    //the user defined Virtual Methods:
    //....
    //the last is nil!!
  end;
//}
implementation

{ TTurboMethodInfo }
function TTurboMethodInfo.GetExternalOptionsAddr: PTurboExteralMethodOptions; 
begin
  if IsExternal then
  begin
    Result := PTurboExteralMethodOptions(Integer(@Self) + SizeOf(TTurboMethodInfo)); 
  end
  else
    Result := nil;
end;

{ TTurboEnumerationType }
function TTurboEnumerationType.Count: Integer;
begin
  Result := FMaxValue - FMinValue + 1;
end;

function TTurboEnumerationType.GetEnumIndex(const aName: string): Integer;
var
  P: PShortString;
begin
  P := FNameList;
  For Result := FMinValue to FMaxValue do
  begin
    if aName = P^ then
    begin
      exit;
    end;
    Inc(Integer(P), Length(P^) + 1);
  end;
  Result := -1;
end;

function TTurboEnumerationType.GetEnumValue(const aName: string): Integer;
begin
  Result := GetEnumIndex(aName);
  if (Result <> -1) and (FValueList <> nil) then
   begin
      Result := PTsIntArray(FValueList)^[Result-FMinValue]; 
   end;
end;

function TTurboEnumerationType.GetEnumName(Value: tsInt): PShortString;
var
  I: tsInt;
  vIndex: tsInt;
  P: PShortString;
begin
  Result := nil;
  vIndex := Value;
  if FValueList <> nil then
  begin
    vIndex := -1;
    Pointer(P) := FValueList; 
    for I := 0 to FMaxValue - FMinValue do
    begin
      if PTsIntArray(P)^[I] = Value then
      begin 
        vIndex := I;
        break;
      end;
    end;
    if vIndex = -1 then exit;
  end;

  P := FNameList;
  while vIndex <> 0 do
  begin
    Inc(Integer(P), Length(P^) + 1);
    Dec(vIndex);
  end;
  Result := P;
end;


end.
