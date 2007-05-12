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
  While (psRunning in FGlobalOptions.States) do
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

procedure iVMEnter(const FGlobalOptions: PTurboGlobalOptions);
var
  p: tsInt;
begin
  p := PtsInt(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(p));
  //the new PC:
  p := p + Integer(FGlobalOptions._Mem.Code);
  //Push the current PC to return stack.
  Dec(FGlobalOptions._RP, SizeOf(tsInt));
  PtsInt(FGlobalOptions._RP)^ := FGlobalOptions._PC; 

  //Update the new PC
  FGlobalOptions._PC := p;    
end;

//input EAX the word CFA.
procedure _DoVMEnter(const FGlobalOptions: PTurboGlobalOptions; aCFA: tsInt);
begin
  aCFA := aCFA + Integer(FGlobalOptions._Mem.Code);
  //Push the current PC to return stack.
  Dec(FGlobalOptions._RP, SizeOf(tsInt));
  PtsInt(FGlobalOptions._RP)^ := FGlobalOptions._PC; 

  //Update the new PC
  FGlobalOptions._PC := aCFA;    
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
begin
  with FGlobalOptions^ do
  begin 
    LastErrorCode := ErrorCode;
    Exclude(States, psRunning);
    Include(States, psHalt);
    //cmpare the RP and ReturnStackBottom, it should be equ.
    if _RP <> ReturnStackBottom then //not same so halt error
    begin
      Include(States, psHaltError);
    end
    else  
      Exclude(States, psHaltError);
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
    Inc(_SP, SizeOf(vErrAddr));
    vErrCode := PtsInt(_SP)^;
    Inc(_SP, SizeOf(vErrCode));
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
  Inc(FGlobalOptions._SP, SizeOf(vErrAddr));
  vErrAddr := PPointer(FGlobalOptions._RP)^;

  _iVMErrorAt(FGlobalOptions, vErrCode, vErrAddr);
end;

procedure iVMExit(const FGlobalOptions: PTurboGlobalOptions);
begin
  FGlobalOptions._PC := PtsInt(FGlobalOptions._RP)^;
  Inc(FGlobalOptions._RP, SizeOf(Pointer));
end;

procedure iVMExitFar(const FGlobalOptions: PTurboGlobalOptions);
begin
  FGlobalOptions._PC := PtsInt(FGlobalOptions._RP)^;
  Inc(FGlobalOptions._RP, SizeOf(Pointer));
  FGlobalOptions._Mem := PPointer(FGlobalOptions._RP)^;
  Inc(FGlobalOptions._RP, SizeOf(Pointer));
end;

//save the old MemoryBase, pass the CPUStates to the new MemoryBase.
//EnterFar MemBase, CFA
procedure iVMEnterFar(const FGlobalOptions: PTurboGlobalOptions);
var
  p: Pointer;
begin
  //Push the current MemoryBase to return stack.
  Dec(FGlobalOptions._RP, SizeOf(Pointer));
  PPointer(FGlobalOptions._RP)^ := FGlobalOptions._Mem;
  p := PPointer(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(p));
  if Assigned(p) then
  begin
    //load the new MemoryBase
    FGlobalOptions._Mem := p;
  end;

//@@LocalEnter:
  p := PPointer(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(p));
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem.Code); 

  //push the current IP.
  Dec(FGlobalOptions._RP, SizeOf(Pointer));
  PtsInt(FGlobalOptions._RP)^ := FGlobalOptions._PC;
  FGlobalOptions._PC := tsInt(p);
end;

//CALLFAR PTurboModuleInfo cfa-addr
//if PTurboModuleInfo = nil means it's self, do not lookup.
//在返回栈中保存EDI(旧的 FMemory 基址),
//根据 PTurboModuleInfo 查找模块内存基址，如果找到就设置EDI成新的 FDataMemory 基址,
//然后装入该函数的地址，其它就和VMEnter一样了，转去VMEnter。
procedure iVMCallFar(const FGlobalOptions: PTurboGlobalOptions);
var
  vModuleRefInfo: PTurboModuleRefInfo;
  p: Pointer;
begin
  //Push the current MemoryBase to return stack.
  Dec(FGlobalOptions._RP, SizeOf(Pointer));
  PPointer(FGlobalOptions._RP)^ := FGlobalOptions._Mem;

  vModuleRefInfo := PPointer(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(vModuleRefInfo));
  if Assigned(vModuleRefInfo) then
  begin
    //PTurboModuleRefInfo real addr
    Integer(vModuleRefInfo) := Integer(vModuleRefInfo) + Integer(FGlobalOptions._Mem);
    p := vModuleRefInfo.Handle;
    if not Assigned(p) then
    begin
      //@@RequireModule
      //find and load the module into the memory.
      p := FGlobalOptions._Mem.ModuleHandle.RequireModule(vModuleRefInfo.Name);
      if not Assigned(p) then
      begin
        //@@NotFoundError
        //POP  current MemoryBase
        Inc(FGlobalOptions._RP, SizeOf(Pointer));
        _iVMHalt(FGlobalOptions, errModuleNotFound);
        Exit;
      end;
      vModuleRefInfo.Handle := p;
    end;
    FGlobalOptions._Mem := TTurboMemoryModuleAccess(p).FDataMemory;
    iVMEnter(FGlobalOptions);
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
  Inc(FGlobalOptions._SP, SizeOf(vIsAssertionSuccessful));
  vMsg := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(vMsg));
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
  Dec(FGlobalOptions._RP, SizeOf(tsInt));
  PtsInt(FGlobalOptions._RP)^ := FGlobalOptions._PC;
  //set the new IP 
  FGlobalOptions._PC := PtsInt(FGlobalOptions._SP)^ + tsInt(FGlobalOptions._Mem.Code);
  Inc(FGlobalOptions._SP, SizeOf(tsInt));  
end;

//this is a Push Integer(立即操作数) directive
//(-- int8)
procedure iVMPushInt(const FGlobalOptions: PTurboGlobalOptions);
begin
  Dec(FGlobalOptions._SP, SizeOf(tsInt));
  PtsInt(FGlobalOptions._SP)^ := PtsInt(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(tsInt));
end;

//this is a Push Byte(立即操作数) directive
procedure iVMPushByte(const FGlobalOptions: PTurboGlobalOptions);
begin
  Dec(FGlobalOptions._SP, SizeOf(tsInt));
  PtsInt(FGlobalOptions._SP)^ := PByte(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(Byte));
end;

//this is a Push Word(立即操作数) directive
procedure iVMPushWord(const FGlobalOptions: PTurboGlobalOptions);
begin
  Dec(FGlobalOptions._SP, SizeOf(tsInt));
  PtsInt(FGlobalOptions._SP)^ := PWord(FGlobalOptions._PC)^;
  Inc(FGlobalOptions._PC, SizeOf(Word));
end;

procedure iVMDropInt(const FGlobalOptions: PTurboGlobalOptions);
begin
  Inc(FGlobalOptions._SP, SizeOf(tsInt));
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
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem); 
  TTurboExecutorAccess(FGlobalOptions.Executor).DoPrintShortString(p^);
end;

//print AnsiString
procedure vEmitLString(const FGlobalOptions: PTurboGlobalOptions);
var
  p: Pointer;
begin
  p := PPointer(FGlobalOptions._SP)^;
  Inc(FGlobalOptions._SP, SizeOf(pointer));
  Integer(p) := Integer(p) + Integer(FGlobalOptions._Mem); 
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

  GTurboCoreWords[opEnterFar] := iVMEnterFar;
  GTurboCoreWords[opExitFar] := iVMExitFar;

  GTurboCoreWords[opAddInt] := iVMAddInt;
  GTurboCoreWords[opSubInt] := iVMSubInt;
  GTurboCoreWords[opUMULInt] := iVMMulUnsignedInt;
  GTurboCoreWords[opMULInt] := iVMMulInt;
  GTurboCoreWords[opAddInt64] := iVMAddInt64;
  GTurboCoreWords[opSubInt64] := iVMSubInt64;

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
