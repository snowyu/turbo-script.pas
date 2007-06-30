{: the pure pascal script interpreter }
{ Description
堆栈:记住压入减少，弹出增加地址。
}
unit TurboInterpreter;

interface

{$I TurboScript.inc}

uses
  Windows, //QueryPerformanceCounter
  SysUtils, Classes
  , uMeObject
  , uMeTypes
  , uMeProcType
  , uTurboConsts
  , uTurboMetaInfo
  , uTurboExecutor
  ;

type
  TTurboInterpreter = class(TCustomTurboExecutor)
  protected
    function iExecuteCFA(const aCFA: Integer): Integer; override;
  public
    destructor Destroy; override;
  end;

  TTurboInstructionProc   = procedure(const FGlobalOptions: PTurboGlobalOptions);
  TTurboCoreWords = array [TTurboVMInstruction] of TTurboInstructionProc;

var
  GTurboCoreWords: TTurboCoreWords;

implementation

procedure iVMNext(const FGlobalOptions: PTurboGlobalOptions);forward;
procedure _iVMHalt(const FGlobalOptions: PTurboGlobalOptions; ErrorCode: TTurboProcessorErrorCode);forward;
procedure iVMHalt(const FGlobalOptions: PTurboGlobalOptions);forward;
procedure iVMEnter(const FGlobalOptions: PTurboGlobalOptions);forward;
procedure vEmitLString(const FGlobalOptions: PTurboGlobalOptions);forward;
procedure _iVMErrorAt(const FGlobalOptions: PTurboGlobalOptions; ErrorCode: tsInt; ErrorAddr: Pointer);forward;

type
  TTurboMemoryModuleAccess = class(TCustomTurboModule);
  TTurboExecutorAccess = class(TCustomTurboExecutor);
  TTurboMetaInfoAccess = object(TTurboMetaInfo)
  end;

{
****************************** TTurboInterpreter *******************************
}
destructor TTurboInterpreter.Destroy;
begin
  inherited Destroy;
end;

function TTurboInterpreter.iExecuteCFA(const aCFA: Integer): Integer;
begin
  FGlobalOptions._Mem := TTurboMemoryModuleAccess(FMemory).FDataMemory;
  {_PC: the current VM code address. }
  Integer(FGlobalOptions._PC) := Integer(TTurboMemoryModuleAccess(FMemory).FMemory) + aCFA;
  FGlobalOptions.ReturnStackBottom := FGlobalOptions._RP;
  FGlobalOptions.ParamStackBottom  := FGlobalOptions._SP;
  iVMNext(FGlobalOptions);

  {
  if FGlobalOptions._SP <> FGlobalOptions.ParamStackBottom then
  begin
    Dec(FGlobalOptions._SP, SizeOf(tsPointer));
  end;
  //}

  Result := Integer(FGlobalOptions.LastErrorCode);
end;


{----Helper functions ----}


//the interpreter core here:
procedure iVMNext(const FGlobalOptions: PTurboGlobalOptions);
var
  vInstruction: TTurboVMInstruction; //the instruction.
  vProc: TTurboInstructionProc;
begin
  {$IFDEF FPC}
  While (psRunning in TTurboProcessorStates(LongWord(FGlobalOptions.States))) do
  {$ELSE Borland}
  While (psRunning in TTurboProcessorStates(FGlobalOptions.States)) do
  {$ENDIF}
  begin
    vInstruction := PTurboVMInstruction(FGlobalOptions._PC)^;
    Inc(FGlobalOptions._PC);
    vProc := GTurboCoreWords[vInstruction];
    if Assigned(vProc) then
    begin
      vProc(FGlobalOptions);
    end
    else begin
      //BadOpError
      _iVMHalt(FGlobalOptions, errBadInstruction);
    end;
  end;
end;

//input EAX the word CFA.
procedure _DoVMEnter(const FGlobalOptions: PTurboGlobalOptions; aCFA: tsInt);
begin
  aCFA := aCFA + Integer(FGlobalOptions._Mem.Code);
  //Push the current PC to return stack.
  Dec(FGlobalOptions._RP, SizeOf(tsPointer));
  PtsInt(FGlobalOptions._RP)^ := FGlobalOptions._PC; 

  //Update the new PC
  FGlobalOptions._PC := aCFA;    
end;

procedure iVMEnter(const FGlobalOptions: PTurboGlobalOptions);
var
  vCFA: tsInt;
begin
  vCFA := PtsInt(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(vCFA));
  _DoVMEnter(FGlobalOptions, vCFA);
{  //the new PC:
  p := p + Integer(FGlobalOptions._Mem.Code);
  //Push the current PC to return stack.
  Dec(FGlobalOptions._RP, SizeOf(tsPointer));
  PtsInt(FGlobalOptions._RP)^ := FGlobalOptions._PC; 

  //Update the new PC
  FGlobalOptions._PC := p;
}    
end;

//(ErrorCode -- )
procedure iVMHalt(const FGlobalOptions: PTurboGlobalOptions);
var
  vErrorCode: TTurboProcessorErrorCode;  
begin
  vErrorCode := PTurboProcessorErrorCode(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsPointer));
  _iVMHalt(FGlobalOptions, vErrorCode); 
end;

procedure iVMNoop(const FGlobalOptions: PTurboGlobalOptions);
begin
end;

procedure _iVMHalt(const FGlobalOptions: PTurboGlobalOptions; ErrorCode: TTurboProcessorErrorCode);
{$IFDEF FPC}
var
  vStates: TTurboProcessorStates;
{$ENDIF}
begin
  with FGlobalOptions^ do
  begin 
    LastErrorCode := ErrorCode;
    {$IFDEF FPC}
    vStates := TTurboProcessorStates(LongWord(States)); 
    Exclude(vStates, psRunning);
    Include(vStates, psHalt);
    {$ELSE Borland}
    Exclude(TTurboProcessorStates(States), psRunning);
    Include(TTurboProcessorStates(States), psHalt);
    {$ENDIF}
    //cmpare the RP and ReturnStackBottom, it should be equ.
    if _RP <> ReturnStackBottom then //not same so halt error
    begin
      {$IFDEF FPC}
      Include(vStates, psHaltError);
      {$ELSE Borland}
      Include(TTurboProcessorStates(States), psHaltError);
      {$ENDIF}
    end
    else  
      {$IFDEF FPC}
      Exclude(vStates, psHaltError);
      {$ELSE Borland}
      Exclude(TTurboProcessorStates(States), psHaltError);
      {$ENDIF}
    {$IFDEF FPC}
    States := Byte(LongWord(vStates)); 
    {$ENDIF}
  end;
end;

//(ErrorCode, ErrorAdr -- )
procedure iVMErrorAt(const FGlobalOptions: PTurboGlobalOptions);
var
  vErrCode: tsInt; 
  vErrAddr: Pointer;
begin
  with FGlobalOptions^ do
  begin
    vErrAddr := PPointer(_SP)^;
    Inc(_SP, SizeOf(tsPointer));
    vErrCode := PtsInt(_SP)^;
    Inc(_SP, SizeOf(tsPointer));
  end;
  _iVMErrorAt(FGlobalOptions, vErrCode, vErrAddr);
end;

procedure _iVMErrorAt(const FGlobalOptions: PTurboGlobalOptions; ErrorCode: tsInt; ErrorAddr: Pointer);
begin
  FGlobalOptions.ErrorAddr := ErrorAddr;
  _iVMHalt(FGlobalOptions, TTurboProcessorErrorCode(ErrorCode));
end;

//(ErrorCode -- )
procedure iVMError(const FGlobalOptions: PTurboGlobalOptions);
var
  vErrCode: tsInt; 
  vErrAddr: Pointer;
begin
  vErrCode := PtsInt(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsPointer));
  vErrAddr := PPointer(FGlobalOptions._RP)^;

  _iVMErrorAt(FGlobalOptions, vErrCode, vErrAddr);
end;

procedure iVMExit(const FGlobalOptions: PTurboGlobalOptions);
begin
  FGlobalOptions._PC := PtsInt(FGlobalOptions._RP)^;
  Inc(FGlobalOptions._RP, SizeOf(tsPointer));
end;

procedure iVMExitFar(const FGlobalOptions: PTurboGlobalOptions);
begin
  FGlobalOptions._PC := PtsInt(FGlobalOptions._RP)^;
  Inc(FGlobalOptions._RP, SizeOf(tsPointer));
  FGlobalOptions._Mem := PPointer(FGlobalOptions._RP)^;
  Inc(FGlobalOptions._RP, SizeOf(tsPointer));
end;

//save the old MemoryBase, pass the CPUStates to the new MemoryBase.
//EnterFar MemBase, CFA
procedure iVMEnterFar(const FGlobalOptions: PTurboGlobalOptions);
var
  p: Pointer;
begin
  //Push the current MemoryBase to return stack.
  Dec(FGlobalOptions._RP, SizeOf(tsPointer));
  PPointer(FGlobalOptions._RP)^ := FGlobalOptions._Mem;
  p := PPointer(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(p));
  if Assigned(p) then
  begin
    //load the new MemoryBase
    FGlobalOptions._Mem := p;
  end;
  iVMEnter(FGlobalOptions);

{//@@LocalEnter:
  p := PPointer(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(p));
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem.Code); 

  //push the current IP.
  Dec(FGlobalOptions._RP, SizeOf(tsPointer));
  PtsInt(FGlobalOptions._RP)^ := FGlobalOptions._PC;
  FGlobalOptions._PC := tsInt(p);
}
end;

function _DoVMCallFarMemBase(const FGlobalOptions: PTurboGlobalOptions; 
  aModuleRefInfo: PTurboModuleRefInfo): Boolean;
var
  p: Pointer;
begin
  Result := True;
  //Push the current MemoryBase to return stack.
  Dec(FGlobalOptions._RP, SizeOf(tsPointer));
  PPointer(FGlobalOptions._RP)^ := FGlobalOptions._Mem;

  if Assigned(aModuleRefInfo) then
  begin
    p := aModuleRefInfo.Handle;
    if not Assigned(p) then
    begin
      //@@RequireModule
      //find and load the module into the memory.
      p := FGlobalOptions._Mem.ModuleHandle.RequireModule(aModuleRefInfo.Name);
      if not Assigned(p) then
      begin
        //@@NotFoundError
        //POP  current MemoryBase
        Inc(FGlobalOptions._RP, SizeOf(tsPointer));
        _iVMHalt(FGlobalOptions, errModuleNotFound);
        Result := False;
        Exit;
      end;
      aModuleRefInfo.Handle := p;
    end;
    FGlobalOptions._Mem := TTurboMemoryModuleAccess(p).FDataMemory;
  end;
end;

//CALLFAR PTurboModuleInfo cfa-addr
//if PTurboModuleInfo = nil means it's self, do not lookup.
//在返回栈中保存EDI(旧的 FMemory 基址),
//根据 PTurboModuleInfo 查找模块内存基址，如果找到就设置EDI成新的 FDataMemory 基址,
//然后装入该函数的地址，其它就和VMEnter一样了，转去VMEnter。
procedure iVMCallFar(const FGlobalOptions: PTurboGlobalOptions);
var
  vModuleRefInfo: PTurboModuleRefInfo;
begin
  vModuleRefInfo := PPointer(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(vModuleRefInfo));
  if Assigned(vModuleRefInfo) then
    //PTurboModuleRefInfo real addr
    Integer(vModuleRefInfo) := Integer(vModuleRefInfo) + Integer(FGlobalOptions._Mem);

  if _DoVMCallFarMemBase(FGlobalOptions, vModuleRefInfo) then
    iVMEnter(FGlobalOptions);
end;

procedure RunExternalFunc(const aMethod: PTurboMethodInfoEx; 
  const aModule: TCustomTurboModule;
  var aStack: Integer);
begin
  if not Assigned(aMethod.ExternalOptions.ProcInstance) then
  begin
    New(aMethod.ExternalOptions.ProcInstance, Create);
    aMethod.ExternalOptions.ProcInstance.ProcType := PMeProcType(aMethod.TurboType);
    aModule.MeObjects.Add(aMethod.ExternalOptions.ProcInstance);
  end;
  with aMethod.ExternalOptions.ProcInstance^ do
  begin
    AssignFromStack(Pointer(aStack), nil, 0);
    Execute(Pointer(aMethod.MethodAddr));
    //pop params 
    Inc(aStack, ProcType.GetStackTypeSizeIn(0));
  //TODO: push result here.
  //how to allocate the string space. make a Heap?? 
    if Assigned(ResultParam) then
    begin
      with ResultParam^ do 
      if IsByRef or (DataType.ParamType.Kind in [mtkInteger, mtkChar, mtkEnumeration, mtkSet, mtkWChar]) then
      begin
        Dec(aStack, SizeOf(tsInt));
        PPointer(aStack)^ := ParamValue.VPointer;   
      end;
    end;
  end; 
end;

{ opCallExt<PTurboMethodInfo> }
procedure iVMCallExt(const FGlobalOptions: PTurboGlobalOptions);
var
  vMethodInfo: PTurboMethodInfoEx;
  p: tsPointer;
  vMeProc: PMeProcParams;
begin
  //Push the current MemoryBase to return stack.
  //Dec(FGlobalOptions._RP, SizeOf(tsPointer));
  //PPointer(FGlobalOptions._RP)^ := FGlobalOptions._Mem;

  vMethodInfo := PPointer(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(vMethodInfo));
  if Assigned(vMethodInfo) then
  begin
    Integer(vMethodInfo) := Integer(vMethodInfo) + Integer(FGlobalOptions._Mem);
    case vMethodInfo.CodeFieldStyle of
      cfsDLLFunction:
        begin
          if (vMethodInfo.MethodAddr = 0) then
          begin
            vMethodInfo.RequireDLLProcAddress;
            if (vMethodInfo.MethodAddr = 0) then
            begin
              _iVMHalt(FGlobalOptions, errMethodNotFound);
              Exit;
            end;
          end;
          if not Assigned(vMethodInfo.TurboType) then
          begin
            _iVMHalt(FGlobalOptions, errTypeInfoNotFound);
            Exit;
          end;
          RunExternalFunc(vMethodInfo, FGlobalOptions._Mem.ModuleHandle, FGlobalOptions._SP);
        end; 
      cfsExternalFunction: 
        begin
          if _DoVMCallFarMemBase(FGlobalOptions, vMethodInfo.ExternalOptions.ModuleRef) then
            _DoVMEnter(FGlobalOptions, vMethodInfo.MethodAddr);
        end;
      cfsFunction: 
        begin
          _DoVMEnter(FGlobalOptions, vMethodInfo.MethodAddr);
        end;
    end;//case
  end
  else
  begin
    _iVMHalt(FGlobalOptions, errInstructionBadParam);
    Exit;
  end;
end;

procedure _DoAssert(const FGlobalOptions: PTurboGlobalOptions; vMsg: PShortString);
begin
  TTurboExecutorAccess(FGlobalOptions.Executor).DoPrintShortString(vMsg^);
  _iVMErrorAt(FGlobalOptions, tsInt(errAssertionFailed), PPointer(FGlobalOptions._RP)^);
end;

//(ShortString, Bool -- )
procedure iVMAssert(const FGlobalOptions: PTurboGlobalOptions);
var
  vMsg: PShortString;
  vIsAssertionSuccessful: tsInt; //LongBool
begin
  vIsAssertionSuccessful := PtsInt(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsPointer));
  vMsg := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsPointer));
  if vIsAssertionSuccessful = 0 then
  begin 
    //Assertion failed:
    Integer(vMsg) := Integer(vMsg) + Integer(FGlobalOptions._Mem);
    _DoAssert(FGlobalOptions, vMsg);
  end;
end;

//call(EXECUTE) the user defined word
//(CFA --- )
procedure iVMExecute(const FGlobalOptions: PTurboGlobalOptions);
begin
  //push the current IP.
  Dec(FGlobalOptions._RP, SizeOf(tsPointer));
  PtsInt(FGlobalOptions._RP)^ := FGlobalOptions._PC;
  //set the new IP 
  FGlobalOptions._PC := PtsInt(FGlobalOptions._SP)^ + tsInt(FGlobalOptions._Mem.Code);
  Inc(FGlobalOptions._SP, SizeOf(tsPointer));  
end;

//this is a Push Integer(立即操作数) directive
//(-- int8)
procedure iVMPushInt(const FGlobalOptions: PTurboGlobalOptions);
begin
  Dec(FGlobalOptions._SP, SizeOf(tsPointer));
  PtsInt(FGlobalOptions._SP)^ := PtsInt(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(tsInt));
end;

//this is a Push Byte(立即操作数) directive
procedure iVMPushByte(const FGlobalOptions: PTurboGlobalOptions);
begin
  Dec(FGlobalOptions._SP, SizeOf(tsPointer));
  PtsInt(FGlobalOptions._SP)^ := PByte(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(Byte));
end;

//this is a Push Word(立即操作数) directive
procedure iVMPushWord(const FGlobalOptions: PTurboGlobalOptions);
begin
  Dec(FGlobalOptions._SP, SizeOf(tsPointer));
  PtsInt(FGlobalOptions._SP)^ := PWord(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(Word));
end;

procedure iVMDropInt(const FGlobalOptions: PTurboGlobalOptions);
begin
  Inc(FGlobalOptions._SP, SizeOf(tsPointer));
end;

procedure iVMPushInt64(const FGlobalOptions: PTurboGlobalOptions);
begin
  Dec(FGlobalOptions._SP, SizeOf(Int64));
  PInt64(FGlobalOptions._SP)^ := PInt64(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(Int64));
end;

procedure iVMDropInt64(const FGlobalOptions: PTurboGlobalOptions);
begin
  Inc(FGlobalOptions._SP, SizeOf(Int64));
end;
//(n, n1) -- (n = n + n1)
procedure iVMAddInt(const FGlobalOptions: PTurboGlobalOptions);
var
  I: tsInt;
begin
  I := PtsInt(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsInt));
   
  PtsInt(FGlobalOptions._SP)^ := I + PtsInt(FGlobalOptions._SP)^;
end;

//(int64, int64-1) -- (int64 = int64 + int64-1)
procedure iVMAddInt64(const FGlobalOptions: PTurboGlobalOptions);
var
  I: Int64;
begin
  I := PInt64(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(Int64));
   
  PInt64(FGlobalOptions._SP)^ := I + PInt64(FGlobalOptions._SP)^;
end;


//(Doublea, Doubleb) -- (Double = Doubleb + Doublea)
procedure iVMAddDouble(const FGlobalOptions: PTurboGlobalOptions);
var
  I: Double;
begin
  I := PDouble(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(Double));
   
  PDouble(FGlobalOptions._SP)^ := I + PDouble(FGlobalOptions._SP)^;
end;

//(n, n1) -- (n = n1 - n)
procedure iVMSubInt(const FGlobalOptions: PTurboGlobalOptions);
var
  I: tsInt;
begin
  I := PtsInt(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsInt));
   
  PtsInt(FGlobalOptions._SP)^ := I - PtsInt(FGlobalOptions._SP)^;
end;

//(int64a, int64b) -- (int64 = int64b - int64a)
procedure iVMSubInt64(const FGlobalOptions: PTurboGlobalOptions);
var
  I: Int64;
begin
  I := PInt64(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(Int64));
   
  PInt64(FGlobalOptions._SP)^ := I - PInt64(FGlobalOptions._SP)^;
end;

// Unsigned multiply
//(n, n1) -- (DoubleWord = n * n1)
//EAX(n): the result of Low orders; n1 the result of high orders
procedure iVMMulUnsignedInt(const FGlobalOptions: PTurboGlobalOptions);
var
  I: tsUInt;
begin
  I := PtsUInt(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsUInt));
   
  PtsUInt(FGlobalOptions._SP)^ := I * PtsUInt(FGlobalOptions._SP)^;
end;

// multiply
//(n, n1) -- total
//total = n* n1(int)
procedure iVMMulInt(const FGlobalOptions: PTurboGlobalOptions);
var
  I: tsInt;
begin
  I := PtsInt(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsInt));
   
  PtsInt(FGlobalOptions._SP)^ := I * PtsInt(FGlobalOptions._SP)^;
end;

//assignment the AnsiString (src, dest -- )
procedure iVMLStrAsg(const FGlobalOptions: PTurboGlobalOptions);
var
  src, dest: PString; //point to string
  //srcStr,destStr: Pointer;  //string
begin
  dest := PPointer(FGlobalOptions._SP)^;
  Integer(dest) := Integer(dest) + Integer(FGlobalOptions._Mem);
  //the value is the offset address of the string, so I need to add the _Mem
  //Integer(destStr) := Integer(dest^) + Integer(FGlobalOptions._Mem);
  
  Inc(FGlobalOptions._SP, SizeOf(tsInt));
  src := PPointer(FGlobalOptions._SP)^;
  if Assigned(src) then
  begin
    Integer(src) := Integer(src) + Integer(FGlobalOptions._Mem);
    //Integer(srcStr) := Integer(src^) + Integer(FGlobalOptions._Mem);
    //AnsiString(destStr) := AnsiString(srcStr);
    dest^ := src^; 
  end
  else 
  begin
    //AnsiString(destStr) := '';
    dest^  := '';
  end;
  Inc(FGlobalOptions._SP, SizeOf(tsInt));
end;

//(intAddr -- int)
procedure vFetchInt(const FGlobalOptions: PTurboGlobalOptions);
var
  p: PtsInt;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem);
  PtsInt(FGlobalOptions._SP)^ := p^;   
end;

procedure vFetchByte(const FGlobalOptions: PTurboGlobalOptions);
var
  p: PByte;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem);
  PtsInt(FGlobalOptions._SP)^ := p^;   
end;

procedure vFetchWord(const FGlobalOptions: PTurboGlobalOptions);
var
  p: PWord;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem);
  PtsInt(FGlobalOptions._SP)^ := p^;   
end;

//(addr -- int64)
procedure vFetchInt64(const FGlobalOptions: PTurboGlobalOptions);
var
  p: PInt64;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem);
  Dec(FGlobalOptions._SP, SizeOf(tsPointer));
  PInt64(FGlobalOptions._SP)^ := p^;   
end;

//(int addr -- )
procedure vStoreInt(const FGlobalOptions: PTurboGlobalOptions);
var
  i: tsInt;
  p: PtsInt;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(P));
  i := PtsInt(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(i));
  //convert the related addr to absolute addr
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem); 
  p^ := i;
end;

procedure vStoreInt64(const FGlobalOptions: PTurboGlobalOptions);
var
  i: Int64;
  p: PInt64;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(P));
  i := PInt64(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(i));
  //convert the related addr to absolute addr
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem); 
  p^ := i;
end;

procedure vStoreWord(const FGlobalOptions: PTurboGlobalOptions);
var
  i: Word;
  p: PWord;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(P));
  i := PWord(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(i));
  //convert the related addr to absolute addr
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem); 
  p^ := i;
end;

procedure vStoreByte(const FGlobalOptions: PTurboGlobalOptions);
var
  i: Byte;
  p: PByte;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(P));
  i := PByte(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(i));
  //convert the related addr to absolute addr
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem); 
  p^ := i;
end;

//print a char
//{c -- }
procedure vEmitChar(const FGlobalOptions: PTurboGlobalOptions);
var
  c: Char;
begin
  c := PChar(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(tsInt));
  TTurboExecutorAccess(FGlobalOptions.Executor).DoPrintChar(c);
end;

//print ShortString
//(PShortString -- )
procedure vEmitString(const FGlobalOptions: PTurboGlobalOptions);
var
  p: PShortString;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(p));
  //Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem); 
  TTurboExecutorAccess(FGlobalOptions.Executor).DoPrintShortString(p^);
end;

//print AnsiString
procedure vEmitLString(const FGlobalOptions: PTurboGlobalOptions);
var
  p: Pointer;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(pointer));
  //Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem); 
  TTurboExecutorAccess(FGlobalOptions.Executor).DoPrintString(AnsiString(p));
end;

//(-- int64)
procedure vGetTickCount(const FGlobalOptions: PTurboGlobalOptions);
var
  i: Int64;
begin
  QueryPerformanceCounter(i);
  Dec(FGlobalOptions._SP, SizeOf(Int64));
  PInt64(FGlobalOptions._SP)^ := i;
end;

//(int64Addr --)
procedure vStoreTickCount(const FGlobalOptions: PTurboGlobalOptions);
var
  p: PInt64;
begin
  p := PPointer(FGlobalOptions._SP)^;
  //convert the related addr to absolute addr
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem);
  Inc(FGlobalOptions._SP, SizeOf(Pointer));
  QueryPerformanceCounter(P^);
end;

procedure InitTurboCoreWordList;
begin
  GTurboCoreWords[opNoop] := iVMNoop;
  GTurboCoreWords[opNext] := iVMNext;
  GTurboCoreWords[opHalt] := iVMHalt;
  GTurboCoreWords[opAssert] := iVMAssert;

  GTurboCoreWords[opEnter] := iVMEnter;
  GTurboCoreWords[opExit] := iVMExit;
  GTurboCoreWords[opCallFar] := iVMCallFar;
  GTurboCoreWords[opCallExt] := iVMCallExt;

  GTurboCoreWords[opEnterFar] := iVMEnterFar;
  GTurboCoreWords[opExitFar] := iVMExitFar;

  GTurboCoreWords[opAddInt] := iVMAddInt;
  GTurboCoreWords[opSubInt] := iVMSubInt;
  GTurboCoreWords[opUMULInt] := iVMMulUnsignedInt;
  GTurboCoreWords[opMULInt] := iVMMulInt;
  GTurboCoreWords[opAddInt64] := iVMAddInt64;
  GTurboCoreWords[opSubInt64] := iVMSubInt64;
  GTurboCoreWords[opLStrAsg] := iVMLStrAsg;

  //Memory Operation Instruction with Param Stack
  GTurboCoreWords[opFetchInt] := vFetchInt;
  GTurboCoreWords[opStoreInt] := vStoreInt;
  GTurboCoreWords[opFetchInt64] := vFetchInt64;
  GTurboCoreWords[opStoreInt64] := vStoreInt64;
  GTurboCoreWords[opFetchWord] := vFetchWord;
  GTurboCoreWords[opStoreWord] := vStoreWord;
  GTurboCoreWords[opFetchByte] := vFetchByte;
  GTurboCoreWords[opStoreByte] := vStoreByte;

  GTurboCoreWords[opPushInt] := iVMPushInt;
  GTurboCoreWords[opPushByte] := iVMPushByte;
  GTurboCoreWords[opPushWord] := iVMPushWord;
  GTurboCoreWords[opDropInt] := iVMDropInt;
  GTurboCoreWords[opPushInt64] := iVMPushInt64;
  GTurboCoreWords[opDropInt64] := iVMDropInt64;
  GTurboCoreWords[opEmit] := vEmitChar;
  GTurboCoreWords[opEmitString] := vEmitString;
  GTurboCoreWords[opEmitLString] := vEmitLString;
  GTurboCoreWords[opGetTickCount] := vGetTickCount;
  GTurboCoreWords[opStoreTickCount] := vStoreTickCount;

end;

initialization
  InitTurboCoreWordList;
end.
