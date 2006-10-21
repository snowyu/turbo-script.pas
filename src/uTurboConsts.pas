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
  cMAXTIBCount = 1024;
  cTurboCompiledProgramFileExt = '.tpc';
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

type
  ETurboScriptError = class(Exception);
  {: the Module Type }
  {
    @param mtFunction         the script function(word).
  }
  TTurboModuleType = (mtUnknown, mtProgram, mtLib, mtObject, mtFunction);

  TTurboScriptOption = (soOptimize, soLoadOnDemand, soBindingRuntime, soBindingCompileTime);
  TTurboScriptOptions = set of TTurboScriptOption;

  TTurboVisibility = (fvHide, fvPrivate, fvProtected, fvPublic, fvPublished);
  //the Forth Execution priority fpHighest means cfsImmediately
  TTurboPriority = (fpLowest, fpLower, fpLow, fpNormal, fpHigh, pfHigher, fpHighest);
  TTurboCallStyle = (csForth, csRegister, csPascal, csCdecl, csStdCall, csFastCall);
  TTurboCodeFieldStyle = (cfsFunction, cfsVariable);
  TTurboCodeFieldStyles = set of TTurboCodeFieldStyle;
  TTurboWordOptions = packed record //a DWORD
      //���ȼ�, highest means an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: TTurboPriority; 
      Visibility: TTurboVisibility; 
      CallStyle: TTurboCallStyle;
      CodeFieldStyle: TTurboCodeFieldStyles;
  end;

  { Summary the FORTH Virtual Mache Codes}
  TTurboVMInstruction = (
    inNone,
    {## The FORTH CORE instructions }
    inHalt,
    inEnter,
    inExit,
    inNext,
    //Far Call, ���ڹ����Ĺ�����ģ����õĺ���ȫ��ʹ�ø÷�ʽ
    //ForthDLL��ģ�����ӷ�ʽ����
    inEnterFar, //inEnterFar ModuleMemBase-addr Addr
    inExitFar,  //����Զ���õķ���ָ������Ǹ�ָ��! R: (MemoryBase PC -- )
    
    inPushByte, //in fact it will be expand to int, then inPush Byte (-- n)
    inPushWord,
    inPushInt,  // inPushInt Int (-- n)
    inPushQWord, //two integer(int64).
    inPopByte, //inPopByte ByteVar-Addr (n --)
    inPopWord, //inPopWord WordVar-Addr (n --)
    inPopInt, //inPopInt IntVar-Addr (n --)
    inPopQWord, //inPopQWord QWordVar-Addr (n --)

    {## Memory Operation Instruction }
    inStoreInt,  //! Store a Integer,pop data to memory (aInt addr --)
    inStoreByte, //C! CStore
    inStoreWord, //inStoreWord  (aWord aWord-addr --)
    inStoreQWord, //int64, inStoreQWord (aQWord aQWord-addr --)
    inStoreRP, //RP! Set return stack pointer (addr -- )
    inFetchInt,  //Fetch, push a integer from memory. (addr -- aInt)
    inFetchByte, //CFetch
    inFetchWord, //inFetchWord (aWord-addr -- aWord) 
    inFetchQWord,
    inFetchRP, //RP@ Push current RP(returnStacck Poiinter) as data (-- RP)
    //Copies bytes from a source to a destination.
    inMove, //Move(src-addr, dest-addr, count --)
    inALLOT, //Allocate n bytes : usedMemory+n (n --) 

    {## Arithmatic instructions }
    {## for Integer}
    inAddInt, //Add
    inSubInt, //subtract
    inIncInt, //add 1
    inDecInt, //subtract 1
    inUMULInt, //UM* Unsigned multiply(un1, un2 -- u-int64 ) 
    inMULInt, //(n n -- n)
    inMULInt64, //(n n -- int64)
    inDIVInt, //divide (n n -- q)
    inModInt, //(n n -- r)
    inDivModInt, //(n n -- r q)
    inDivModInt64, //M/Mod (int64 n -- r q) int64/n
    inUDivModInt64, //UM/Mod (unsigned-int64 n -- unsigned-r unsigned-q)
    inMULDIVMODInt, //n1*n2/n3 (n1 n2 n3 -- r q)
    InMULDIVInt, // n1*n2/n3 (n1 n2 n3 -- q)
    inIncNInt, //add N
    inDecNInt, //subtract N
    inMinInt, //Return the smaller of top two n1<n2 (n1,n2 -- n1)
    inMaxInt, //Return the bigger of top two n1<n2 (n1,n2 -- n2)
    inWithinUnsignedInt, //( u ul uh -- t )          Return true if ul <= u < uh �^�g��
    inAddStr, //(pShortString1 pShortString2 ---- pResult)
    inAddLStr, //(pAnsiString1 pAnsiString2 ---- pAnsiResult)

    {## Logical instructions }
    {## for Integer}
    inLess0, //return true if n < 0 (n -- t)
    inEQUInt, // return true if n1 = n2 (n1 n2 -- t)
    inNEQInt, // not equ
    inLESInt, //less than
    inLEQInt, //less than and equ
    inGERInt, //greater than
    inGEQInt, //greater than and equ
    inNOTInt, //logic NOT n (n -- n1)
    inANDInt,
    inORInt,
    inXORInt,
    inNEGATEInt, // Two's complement of top of stack (n -- -n)
    inNegateInt64, // Two's complement of top of stack (int64 -- -int64)
    inABSInt, //(n -- |n|)
    inABSInt64,
    
    {## Proc Operation Instruction: Flow Control }
    inJMP, //JUMP Absolute address(related to FMemory)
    //inJMPByte, //JMP aByteInt(shortint offset) 
    //inJMPWord, //JMP aWordInt(smallint offset)
    inJMPOffset,  //JMP aInt(offset) the real addr = FMemory + PC + offset
    inGoto,   //(goto-addr -- )
    inJZ, //jmp absolute address with condition(the TOS is 0) (n -- )
    //inJZByte,
    //inJZWord,
    inJZOffset,
    inJNZ,
    inJNZOffset,
    inTestIfFalse,//(bool -- bool) Jump if current data-stack is false, without removing value from stack
    inTestIfTrue, //(bool -- bool) Jump if current data-stack is true, without removing value from stack
    inExecute, //call(EXECUTE) the private word (CFA -- )
    inCallFar,  //inCallFar PTurboModuleEntry(offset of Memory) cfa-addr
    //inReturn, //= inExit
    inWhile, //inWhile whileEnd-addr (bool -- )
    inRepeat, //inRepeat RepeatEnd-addr (bool -- )
    inFor,  //inFor ForEnd-addr (start, end -- )
    inNoop,
    inTryFinally, // inTryFinally inTryEnd-addr inFinallyEnd-addr
    inTryExcept,  // inTryExcept inTryEnd-addr inExceptEnd-addr
    //Call other module subroutes.

    {## data Stack Operation Instuction }
    inDropInt,  //Discard top of stack (int --) 
    inDropQWord,  //Discard top of stack (int64 --) 
    inDUPInt,  //Duplicate the TOS (int -- int int)
    inSWAPInt, //Exchange top two of stack (int1 int2 -- int2 int1)
    inOVERInt,  //Duplicate second of stack (i1 i2 -- i1 i2 i1)
    inROTInt

    {## Return Stack Operation Instuction }
    , inRPushInt //>R: push to return stack  (int --) R(-- int)
    , inRPopInt  //R>: Pop from return stack (-- int) R(int --)
    , inRCopyInt //R@: Copy the TOS of return stack (-- int) R (int -- int)
    , inEMIT  //(c --): send char out.
    , inEmitString // (ShortStringAddr -- )
    , inEmitLString // (AnsiStringAddr -- )

  ); 

  //the Core procedure List, maybe procedure or method.
  {: ��������ָ��� }
  TTurboCoreWords = array [TTurboVMInstruction] of TProcedure;

  {
  @param errHalt there are some data stil in return stack when Halt, the ESP should be point to
    the stack bottom! put the current ESP to TPreservedCodeMemory.ReturnStackBottom.   

  Note: the state Must be a Byte for speed!!!
  }
  TTurboProcessorState = (psRunning, psStepping, psCompiling
    , errHalt
  );
  TTurboProcessorStates = set of TTurboProcessorState;
  TTurboProcessorErrorCode = (errNone, errBadInstruction, errDizZero
    , errModuleNotFound
    , errOutOfMem, errOutOfDataStack, errOutOfReturnStack 
  );

  
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
