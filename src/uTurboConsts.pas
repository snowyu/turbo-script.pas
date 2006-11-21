{: the turbo script type and constants }
unit uTurboConsts;

interface

uses
  SysUtils, Classes
  ;

const
  cFORTHHeaderMagicId = 'TURBO4TH';
  cFORTHHeaderMagicIdLen = 8; 
  //cFORTHMagicWordSize = SizeOf(cFORTHHeaderMagicWord);
  cDefaultReturnStackSize = 127;
  cDefaultParamStackSize = 127; 
  cDefaultFreeMemSize = 1024 * 8; //the Free Memory 8kb
  cDefaultDataMemSize = 1024;
  cMAXTIBCount = 1024;
  cTurboCompiledProgramFileExt = '.tpc';
  cTurboCompiledUnitFileExt = '.tpc';
  cTurboForthFileExt = '.tf';
  
resourcestring
  rsMissFileHeaderError = 'Error: The file header is missed';
  rsTurboScriptAlreayRunningError = 'Error: The Turbo Script is already running.';
  rsTurboScriptNotLoadedError = 'Error: The Turbo Script is not loaded yet.';
  rsInvalidTurboScriptStreamError = 'Turbo Script LoadStream Error: the stream invalid.';
  rsReturnStackUnderflowError = 'Return Stack underflow.';
  rsReturnStackOverflowError = 'Return Stack overflow';
  rsParamStackUnderflowError = 'Parameter Stack underflow.';
  rsParamStackOverflowError = 'Parameter Stack overflow';
  rsBadOpCodeError = 'Error: Bad OpCode Found.';
  rsLabelRedeclarationSyntaxError = 'Error: The Label name is redeclareted!.';
  rsWordNameIsNilDeclarationError = 'Error: the declaration word name is null!';
  rsUnknownWordError = 'Error: Unknown Word: no such word defined.';   
  rsVarRedeclarationSyntaxError = 'Error: The Variable name is redeclareted!.';
  rsConstRedeclarationSyntaxError = 'Error: The Constant name is redeclareted!.';
  rsRedeclarationSyntaxError  = 'Error: The Identifier is redeclareted!.';
  rsDLLModuleMissSyntaxError = 'Error: No DLL Module assigned!';
  rsFileNotFoundError = 'Fatal: File not found:';
  rsWordNotFoundError = 'Fatal: Word not found:';

type
  PTsInt = ^ tsInt;
  tsInt = LongInt;
  tsPointer = Pointer;
  PTsIntArray = ^tsIntArray; 
  tsIntArray = array [0..(High(tsInt) div 8)] of tsInt;

  ETurboScriptError = class(Exception);
  {: the Module Type }
  {
    @param mtFunction         the script function(word).
    @param mtHost             the module is in the host application
    @param mtDLL              the DLL module
  }
  TTurboModuleType = (mtUnknown, mtProgram, mtLib, mtObject, mtFunction, mtHost, mtDLL);

  TTurboScriptOption = (soOptimize, soLoadOnDemand, soBindingRuntime, soBindingCompileTime);
  TTurboScriptOptions = set of TTurboScriptOption;

  {
    fvHide, fvPrivate: only can view in the module
    fvProtected-fvPublished: ����Զ���÷�ʽ������ģ��ɼ���
  }
  TTurboVisibility = (fvHide, fvPrivate, fvProtected, fvPublic, fvPublished);
  //the Forth Execution priority fpHighest means cfsImmediately
  TTurboPriority = (fpLowest, fpLower, fpLow, fpNormal, fpHigh, pfHigher, fpHighest);
  TTurboCallStyle = (csForth, csRegister, csPascal, csCdecl, csStdCall, csFastCall);
  //cfsExternalFunction is external function. see Also ExternalOptions.ModuleType
  TTurboCodeFieldStyle = (cfsFunction, cfsExternalFunction);
  TTurboWordOptions = packed record //a DWORD
      //���ȼ�, highest means an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: TTurboPriority; 
      Visibility: TTurboVisibility; 
      CallStyle: TTurboCallStyle;
      CodeFieldStyle: TTurboCodeFieldStyle;
  end;

  PTurboVMInstruction = ^TTurboVMInstruction;
  { Summary the FORTH Virtual Mache Codes}
  TTurboVMInstruction = (
    opNoop,
    {## The FORTH CORE opstructions }
    opHalt,
    opEnter, //inEnter Addr
    opExit,
    opNext,
    //Far Call, ���ڹ����Ĺ�����ģ����õĺ���ȫ��ʹ�ø÷�ʽ
    //ForthDLL��ģ�����ӷ�ʽ����
    opEnterFar, //inEnterFar ModuleMemBase-addr Addr
    opExitFar,  //����Զ���õķ���ָ������Ǹ�ָ��! R: (MemoryBase PC -- )
    
    opPushByte, //in fact it will be expand to opt, then opPush Byte (-- n)
    opPushWord,
    opPushInt,  // opPushInt opt (-- n)
    opPushInt64, //two opteger(int64). (-- opt64)
    opPopByte, // opPopByte ByteVar-Addr  (n --)
    opPopWord, // opPopWord WordVar-Addr  (n --)
    opPopInt, // opPopInt   optVar-Addr  (n --)
    opPopInt64, //opPopInt64 QWordVar-Addr (n --)

    {## Memory Operation opstruction }
    opStoreInt,  //! Store a opteger,pop data to memory (aInt offsetAddr --)
    opStoreByte, //C! CStore
    opStoreWord, //inStoreWord  (aWord aWord-addr --)
    opStoreInt64, //int64, opStoreQWord (aQWord aQWord-addr --)
    opStoreRP, //RP! Set return stack pointer (offsetAddr -- )
    opFetchInt,  //Fetch, push a opteger from memory. (offsetAddr -- aInt)
    opFetchByte, //CFetch
    opFetchWord, //inFetchWord (aWord-addr -- aWord) 
    opFetchInt64,
    opFetchRP, //RP@ Push current RP(returnStacck Poiinter) as data (-- RP)
    //Copies bytes from a source to a destination.
    opMove, //Move(dest-addr, src-addr, size --)
    opALLOT, //Allocate n bytes : usedMemory+n (n --) 

    {## Arithmatic opstructions }
    {## for opteger}
    opAddInt, //Add
    opAddInt64, //Add opt64
    opSubInt, //subtract
    opSubInt64, //subtract opt64 (int64a, opt64b) -- (int64 = opt64b - opt64a)
    opIncInt, //add 1
    opDecInt, //subtract 1
    opUMULInt, //UM* Unsigned multiply(un1, un2 -- u-int64 ) 
    opMULInt, //(n n -- n)
    opMULInt64, //(n n -- opt64)
    opDIVInt, //divide (n n -- q)
    opModInt, //(n n -- r)
    opDivModInt, //(n n -- r q)
    opDivModInt64, //M/Mod (int64 n -- r q) opt64/n
    opUDivModInt64, //UM/Mod (unsigned-int64 n -- unsigned-r unsigned-q)
    opMULDIVMODInt, //n1*n2/n3 (n1 n2 n3 -- r q)
    opMULDIVInt, // n1*n2/n3 (n1 n2 n3 -- q)
    opIncNInt, //add N
    opDecNInt, //subtract N
    opMinInt, //Return the smaller of top two n1<n2 (n1,n2 -- n1)
    opMaxInt, //Return the bigger of top two n1<n2 (n1,n2 -- n2)
    opWithinUnsignedInt, //( u ul uh -- t )          Return true if ul <= u < uh �^�g��
    opAddStr, //(pShortString1 pShortString2 ---- pResult)
    opAddLStr, //(pAnsiString1 pAnsiString2 ---- pAnsiResult)

    {## Logical opstructions }
    {## for opteger}
    opLess0, //return true if n < 0 (n -- t)
    opEQUInt, // return true if n1 = n2 (n1 n2 -- t)
    opNEQInt, // not equ
    opLESInt, //less than
    opLEQInt, //less than and equ
    opGERInt, //greater than
    opGEQInt, //greater than and equ
    opNOTInt, //logic NOT n (n -- n1)
    opANDInt,
    opORInt,
    opXORInt,
    opNEGATEInt, // Two's complement of top of stack (n -- -n)
    opNegateInt64, // Two's complement of top of stack (int64 -- -int64)
    opABSInt, //(n -- |n|)
    opABSInt64,
    
    {## Proc Operation opstruction: Flow Control }
    opJMP, //JUMP Absolute address(related to FMemory)
    //inJMPByte, //JMP aByteInt(shortint offset) 
    //inJMPWord, //JMP aWordInt(smallint offset)
    opJMPOffset,  //JMP aInt(offset) the real addr = FMemory + PC + offset
    opGoto,   //(goto-addr -- )
    opJZ, //jmp absolute address with condition(the TOS is 0) (n -- )
    //inJZByte,
    //inJZWord,
    opJZOffset,
    opJNZ,
    opJNZOffset,
    opTestIfFalse,//(bool -- bool) Jump if current data-stack is false, without removing value from stack
    opTestIfTrue, //(bool -- bool) Jump if current data-stack is true, without removing value from stack
    opExecute, //call(EXECUTE) the private word (CFA -- )
    opCallFar,  //inCallFar PTurboModuleEntry(offset of Memory) cfa-addr
    //inReturn, //= opExit
    opSwitch, //opSwitch<n[u-int32], t1,...,tn>  t1..tn is offsets(positive or negative) address 
              //(i -- ) i = 0..n-1   
    opWhile, //inWhile whileEnd-addr (bool -- )
    opRepeat, //inRepeat RepeatEnd-addr (bool -- )
    opFor,  //inFor ForEnd-addr (start, end -- )
    opTryFinally, // opTryFinally opTryEnd-addr opFinallyEnd-addr
    opTryExcept,  // opTryExcept opTryEnd-addr opExceptEnd-addr
    //Call other module subroutes.

    {## data Stack Operation opstuction }
    opDropInt,  //Discard top of stack (int --) 
    opDropInt64,  //Discard top of stack (int64 --) 
    opDUPInt,  //Duplicate the TOS (int -- opt opt)
    opDUPInt64,  //Duplicate the TOS (int64 -- opt64 opt64)
    opSWAPInt, //Exchange top two of stack (int1 opt2 -- opt2 opt1)
    opSWAPInt64, //Exchange top two of stack (int641 opt642 -- opt642 opt641)
    opOVERInt,  //Duplicate second of stack (i1 i2 -- i1 i2 i1)
    opOVERInt64,  //Duplicate second of stack (i641 i642 -- i641 i642 i641)
    opROTInt,
    opROTInt64

    {## Return Stack Operation opstuction }
    , opRPushInt //>R: push to return stack  (int --) R(-- opt)
    , opRPopInt  //R>: Pop from return stack (-- opt) R(int --)
    , opRCopyInt //R@: Copy the TOS of return stack (-- opt) R (int -- opt)
    , opEMIT  //(c --): send char out.
    , opEmitString // (ShortStringAddr -- )
    , opEmitLString // (AnsiStringAddr -- )
    , opGetTickCount //(-- opt64)
    , opStoreTickCount //(int64Addr -- ) (int64Addr)^ = tickcount

  ); 

  //the Core procedure List, maybe procedure or method.
  {: ��������ָ��� } 
  TTurboCoreWords = array [TTurboVMInstruction] of TProcedure;

  {
  @param errHalt there are some data stil in return stack when Halt, the ESP should be point to
    the stack bottom! put the current ESP to TPreservedCodeMemory.ReturnStackBottom.   

  Note: the state Must be a Byte for speed!!!
  }
  TTurboProcessorState = (psHalt, psRunning, psStepping, psCompiling
    //, errHalt
  );
  TTurboProcessorStates = set of TTurboProcessorState;
  TTurboProcessorErrorCode = (errNone, errBadInstruction, errHalt, errDizZero
    , errModuleNotFound
    , errOutOfMem, errOutOfDataStack, errOutOfReturnStack 
  );

  
  TStaticMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(const Ptr: Pointer; const Size: LongInt); reintroduce;
  end;

  {: FreeNotify }
  TCustomTurboObject = class(TObject)
  private
    FFreeNotifies: TList;
  protected
    procedure SendFreeNotification;
  public
    constructor Create;
    destructor Destroy; override;
    function FindFreeNotification(aProc: TNotifyEvent): Integer;
    procedure FreeNotification(aProc: TNotifyEvent);
    procedure RemoveFreeNotification(aProc: TNotifyEvent);
  end;


const
  //For TTurboForthProcessorStates (put it into EBX Status Register).
  {the psLoaded is used for the Executor, not for the VM processor.}
  //cTurboScriptIsLoadedBit       = psLoaded;
  cTurboScriptIsRunningBit       = psRunning;
  cTurboScriptIsSteppingBit      = psStepping;
  //cTurboScriptBadInstructionBit  = [psBadInstruction];
  cMaxTurboVMInstructionCount = SizeOf(TTurboCoreWords) div SizeOf(TProcedure); //the max turbo VM code directive count
  
implementation

{
***************************** TStaticMemoryStream ******************************
}
constructor TStaticMemoryStream.Create(const Ptr: Pointer; const Size: LongInt);
begin
  inherited Create;
  SetPointer(Ptr, Size);
end;

{
****************************** TCustomTurboObject ******************************
}
constructor TCustomTurboObject.Create;
begin
  inherited Create;
  FFreeNotifies := TList.Create;
end;

destructor TCustomTurboObject.Destroy;
begin
  SendFreeNotification;
  FFreeNotifies.Free;
  FFreeNotifies := nil;
  inherited Destroy;
end;

function TCustomTurboObject.FindFreeNotification(aProc: TNotifyEvent): Integer;
var
  ProcMethod: TMethod;
begin
  for Result := 0 to FFreeNotifies.Count div 2 - 1 do
  begin
    ProcMethod.Code := FFreeNotifies.Items[Result * 2];
    ProcMethod.Data := FFreeNotifies.Items[Result * 2 + 1];
    if (ProcMethod.Code = TMethod(aProc).Code) and (ProcMethod.Data = TMethod(aProc).Data) then
      Exit;
  end;
  Result := -1;
end;

procedure TCustomTurboObject.FreeNotification(aProc: TNotifyEvent);
begin
  if FindFreeNotification(aProc) < 0 then
  begin
    FFreeNotifies.Insert(0, Pointer(TMethod(aProc).Data));
    FFreeNotifies.Insert(0, Pointer(TMethod(aProc).Code));
  end;
end;

procedure TCustomTurboObject.RemoveFreeNotification(aProc: TNotifyEvent);
var
  I: Integer;
begin
  i := FindFreeNotification(aProc);
  if i >= 0 then
    FFreeNotifies.Delete(i);
end;

procedure TCustomTurboObject.SendFreeNotification;
var
  I: Integer;
  ProcMethod: TMethod;
  Proc: TNotifyEvent Absolute ProcMethod;
begin
  for I := 0 to FFreeNotifies.Count div 2 - 1 do
  begin
    ProcMethod.Code := FFreeNotifies.Items[I * 2];
    ProcMethod.Data := FFreeNotifies.Items[I * 2 + 1];
    Proc(Self);
  end;
end;


end.
