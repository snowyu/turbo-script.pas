{: the turboScript basic metadata for type, constants etc. }
{ Description
��Щobject ���ǹ����ڴ���� typecast�ġ�

��������ʱʹ�ã�MetaInfo object ʹ�þ��Ե�ַ��
}
unit uTurboMetaInfo;

interface

{$I TurboScript.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeTypes
  , uTurboConsts
  ;

type
  PTurboMetaInfo       = ^ TTurboMetaInfo;
  PTurboModuleRefInfo  = ^ TTurboModuleRefInfo;
  PTurboTypeInfo       = ^ TTurboTypeInfo;

  //PTurboVariableEntry = ^ TTurboVariableEntry;
  PTurboStaticFieldEntry = ^ TTurboStaticFieldEntry;
  PTurboStaticFieldInfo = ^ TTurboStaticFieldInfo;
  PTurboModuleRefEntry = ^ TTurboModuleRefEntry;
  PTurboMethodInfo = ^ TTurboMethodInfo;
  PTurboMethodInfoEx = ^ TTurboMethodInfoEx;
  PTurboMethodEntry = ^ TTurboMethodEntry;
  PTurboTypeInfoEntry = ^ TTurboTypeInfoEntry;
  PTurboTypeRefEntry = ^ TTurboTypeRefEntry;

   { Description
       @param tfAddrResolved  the address resolved or not
       @param tfInited        the module(static class) initialized or not.
   }
   TTurboModuleFlag = (tfAddrResolved, tfInited);
   TTurboModuleFlags = set of TTurboModuleFlag;

  //the basis metaKind
  TTurboMetaKind = (tmkUnknown
    , tmkClass
    , tmkType //������ʵ�Ǵ�Class������������һ�ж���Class��
    , tmkMethod  //Method
    , tmkVariable
    , tmkConstant
    , tmkStatement //new statement structrue.
    , tmkModule
  );

  //the abstract MetaInfo 
  TTurboMetaInfo = object(TMeDynamicObject)
  protected
    //���ʹ��VMT�ҿ��԰Ѹ��ֶ���Ϊָ��VMT��ָ�룡��װ���
    //not used yet
    //FMetaKind: TTurboMetaKind;
    //Priority not used yet.
    FPriority: TTurboPriority;
    FVisibility: TTurboVisibility;
    FParamFieldLength: tsUInt; 
    FName: PChar;  
  public
    //property MetaKind: TTurboMetaKind read FMetaKind write FMetaKind;
    property Name: PChar read FName write FName;
    property ParamFieldLength: tsUInt read FParamFieldLength write FParamFieldLength;
    property Visibility: LongWord read FVisibility write FVisibility;
  end;  

  PTurboExteralMethodOptions = ^ TTurboExteralMethodOptions;
  //cfsHostFunction, cfsDLLFunction
  TTurboExteralMethodOptions = packed record
    //if it csForth then it is the CFA.
    //ProcAddr: Pointer; in TTurboExteralMethod.MethodAddr 
    ModuleRef: PTurboModuleRefInfo;
    //-1 means non-index visits.
    Index: tsInt;  
    //packed the function name in the DLL/Host.
    Name: ShortString; 
  end;

  TTurboMethodInfo = object(TTurboMetaInfo)
  protected
    //FIsExternal: Boolean;
  public
    //RVA the offset address of the code-memory. CFA
    //if isExternal then the really ProcAddr assigned for speedup. 
    MethodAddr: tsInt;
    CallStyle: TCallingConvention;
    CodeFieldStyle: TTurboCodeFieldStyle;
    //���ط��������ͱ�ȻҲ�����뱾�أ�
    TypeInfo: PTurboTypeInfo; 
    //ExternalWordOptions: TTurboExteralWordOptions;
    function GetExternalOptionsAddr: PTurboExteralMethodOptions; 

    //this is an external define: Host or DLL functions.
    //property IsExternal: Boolean read FIsExternal write FIsExternal;
    function IsExternal: Boolean;
  end;
  //this for external word only
  TTurboMethodInfoEx = object(TTurboMethodInfo)
  public
    ExternalOptions: TTurboExteralMethodOptions;
  public
    procedure RequireDLLProcAddress;
  end; 

  //it's the Module Variable Info too: TTurboVariableInfo
  TTurboStaticFieldInfo = object(TTurboMetaInfo)
  public
    Size: tsInt;
    Addr: Pointer; //offset address of the FMemory.
    {
    in the stream(not resolved):
    0 = nil 
     1..GRegisteredTypes.Count = internal types
     > GRegisteredTypes.Count = the offset of the user defined types in the module
           PTurboTypeInfo
     < 0 = the Abs(TypeInfo) is the offset of the external defined types in the module
           PTurboTypeRefInfo
    in the memory(resolved):
           PMeType
    }
    TypeInfo: PTurboTypeInfo; //TODO: relocate it.
    //Value: ....
  end;

  TTurboStaticFieldEntry = packed record
    Prior: PTurboStaticFieldEntry; //ǰһ��, 0 means Ϊ��ǰ�档
    Variable: TTurboStaticFieldInfo;
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
    //�����ľ�̬�ֶ��б�
    //StaticFieldListEntry: PTurboStaticFieldList;
    StaticFieldCount: tsInt; //Preserved! not used
    LastStaticFieldEntry: PTurboStaticFieldEntry; 
    //�����ķ����б�
    //the first two mmethod item is : cctor(Class Constructor), cdtor((Class Destructor)) 
    MethodListEntry: Pointer;   
    //����������Meta��Ϣ�б�
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
  public
    procedure RequireDLLHandle;
    procedure FreeDLLHandle;
  end;

  TTurboTypeInfo = object(TTurboMetaInfo)
  public
    TurboType: PMeType;
  end;

  TTurboTypeRefInfo = object(TTurboTypeInfo)
    ModuleRef: PTurboModuleRefInfo; //this is related addr!!
    {the index of the ModuleRef.Module.RegisteredTypes}
    {
      Note: ���ModuleRef������������ͣ���ôthe TypeIndex is invalid.
      so Must check the ModuleRef.Revision and BuildDate.
    }
    TypeIndex: tsInt;     
  end;


{  TTurboVariableInfo = object(TTurboMetaInfo)
  public
    Size: tsInt;
    Addr: Pointer; //offset address of the FMemory.
    TypeInfo: PTurboTypeInfo; //TODO: relocate it.
    //Value: ....
  end;
}
  //For type-cast the Mem
  TTurboMethodEntry = packed record
    Prior: PTurboMethodEntry; //ǰһ������ 0 means Ϊ��ǰ�档
    Word: TTurboMethodInfo;
  end;

{  TTurboVariableEntry = packed record
    Prior: PTurboVariableEntry;
    Variable: TTurboVariableInfo; 
    //Value: ....
  end;
}  
  //the import module for uses.
  TTurboModuleRefEntry = packed record
    Prior: PTurboModuleRefEntry; //nil means no more
    Module: TTurboModuleRefInfo;
  end;

  TTurboTypeInfoEntry = packed record
    Prior: PTurboTypeInfoEntry; //nil means no more
    TypeInfo: TTurboTypeInfo;
  end;

  TTurboTypeRefEntry = packed record
    Prior: PTurboTypeRefEntry; //nil means no more
    TypeRef: TTurboTypeRefInfo;
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
function TTurboMethodInfo.IsExternal: Boolean;
begin
  Result := CodeFieldStyle in cTurboExternalFunctions; 
end;

function TTurboMethodInfo.GetExternalOptionsAddr: PTurboExteralMethodOptions; 
begin
  if IsExternal then
  begin
    Result := PTurboExteralMethodOptions(Integer(@Self) + SizeOf(TTurboMethodInfo)); 
  end
  else
    Result := nil;
end;

{ TTurboMethodInfoEx }
procedure TTurboMethodInfoEx.RequireDLLProcAddress;
var
  s: string;
begin
  if (MethodAddr = 0) and (CodeFieldStyle = cfsDLLFunction) then
  begin
    ExternalOptions.ModuleRef.RequireDLLHandle;
    Assert(Assigned(ExternalOptions.ModuleRef.Handle), 'RequireDLLHandle failed.');
    if ExternalOptions.Name <> '' then
      s := ExternalOptions.Name
    else if ExternalOptions.Index <> -1 then
      s := ''
    else
      s := Name;
    if s <> '' then
    begin
      Pointer(MethodAddr) := GetProcAddress(LongWord(ExternalOptions.ModuleRef.Handle)
        , PChar(s));
    end
    else
      Pointer(MethodAddr) := GetProcAddress(LongWord(ExternalOptions.ModuleRef.Handle)
        , Pointer(ExternalOptions.Index));
  end;
end;

procedure TTurboModuleRefInfo.RequireDLLHandle;
begin
  if not Assigned(Handle) and (ModuleType = mtDLL) then
  begin
    LongWord(Handle) := LoadLibrary(Name);
    //if not Assigned(Handle) then
     //error
  end;  
end;

procedure TTurboModuleRefInfo.FreeDLLHandle;
begin
  if Assigned(Handle) and (ModuleType = mtDLL) then
  begin
    if FreeLibrary(LongWord(Handle)) then
      Handle := nil;
  end;
    
end;

end.
