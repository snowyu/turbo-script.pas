{: the turbo script type and constants }
unit uTurboConsts;

interface

{$I TurboScript.inc}

uses
  SysUtils, Classes
  , uMeTypes
  ;

const
  cFORTHHeaderMagicId = 'TURBO4TH';
  cFORTHHeaderMagicIdLen = 8; 
  //cFORTHMagicWordSize = SizeOf(cFORTHHeaderMagicWord);
  cDefaultReturnStackSize = 4096; //4096 the bytes is 1024 * SizeOf(Pointer)
  cDefaultParamStackSize = 1024; 
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
  rsTurboScriptNoMemError = 'Error: No The turbo Memory assigned to the executor engine.';
  rsTurboScriptNoGlobalOptionsError = 'Error: No The turbo Global Options assigned to the executor engine.';

type
  PTsInt = ^ tsInt;
  PTsUInt= ^ tsUInt;
  tsInt  = LongInt;
  tsUInt = LongWord; 
  tsPointer = Pointer; //the min unit for stack.
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

  {
    @param soSymbolPublic the module is SymbolPublic. all the indentities are linked to the module.
                        ��ģ�������������ʱ�򣬱�������ǿ�ư����б�ʶ���ĵ�������Ϣ�����ڴ档
    @param soSymbolTyped the module is TypeSafety. all the indentities are linked to the module and owned the TypeInfo.
                        ��ģ�������������ʱ�򣬱�������ǿ�ư����б�ʶ���ĵ�������Ϣ�����ڴ档
    @param soSymbolNamed  all the indentities are linked to the module and owned the published name!
  }
  //## the set is LongWord in Delphi
  TTurboScriptOption = (soOptimize, soLoadOnDemand 
    , soBindingRuntime, soBindingCompileTime
    , soAssertSupport
    , soSymbolPublic
    , soSymbolTyped
    , soSymbolNamed
    , soShortBooleanEval
    , soRangeChecks
    , soPreserved4
    , soPreserved5
    , soPreserved6
    , soPreserved7
    , soPreserved8
    , soPreserved9
    , soPreserved10
    , soPreserved11
  );
  TTurboScriptOptions = set of TTurboScriptOption;

  {
    @param fvHidden    Word:ֻ���ɱ�ģ����ã������ã��ù��̲��ᱻ���ӵ�LastWordEntry�У�û��Name��Ϣ��
                       Module: Ƕ�뵽����ģ��
    @param fvPrivate   Word:ֻ���ɱ�ģ����ã������ã��ù��̻ᱻ���ӵ�LastWordEntry�У�һ��û��Name��Ϣ��
                       Module: Ƕ�뵽����ģ��
    @param fvProtected Word:ֻ���ɱ�ģ���Լ��Ӹ�ģ�����ģ����ã�Զ���ã��ù��̻ᱻ���ӵ�LastWordEntry�У�һ��û��Name��Ϣ��
    @param fvPublic    Word:����ģ����ɵ��ã�Զ���ã��ù��̻ᱻ���ӵ�LastWordEntry�У�һ��û��Name��Ϣ��
    @param fvPublished Word:����ģ����ɵ��ã�Զ���ã��ù��̻ᱻ���ӵ�LastWordEntry�У���Name��Ϣ��

  ��һ�� LongWord��DWord������ţ�TTurboVisibility
  tvPrivateVisibilty = $00000000;
  tvPublicVisibilty  = $00000001;
  tvPublicNamedVisibilty   = $01000000;
  tvPublicTypedVisibilty   = $02000000;
  }
  //TTurboVisibility = (fvHidden, fvPrivate, fvProtected, fvPublic, fvPublished);
  
  //See Also consstants: tvXXXXVisibilty
  TTurboVisibility = LongWord;

  //the Forth Execution priority fpHighest means cfsImmediately
  TTurboPriority = (fpLowest, fpLower, fpLow, fpNormal, fpHigh, pfHigher, fpHighest);
  //TTurboCallStyle = (csForth, csRegister, csPascal, csCdecl, csStdCall, csFastCall);
  {: the Method Style. }
  {
  @param cfsFunction          It's the local Lib function.          
  @param cfsExternalFunction  It's the external Lib function. 
  @param cfsHostFunction      It's the external Host function.
  @param cfsDLLFunction       It's the external DLL function.
  see Also ExternalOptions.ModuleType
  }
  TTurboCodeFieldStyle = (cfsFunction, cfsExternalFunction, cfsHostFunction, cfsDLLFunction);
{  TTurboWordOptions = packed record //a DWORD
      //���ȼ�, highest means an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: TTurboPriority; 
      Visibility: TTurboVisibility; 
      CallStyle: TCallingConvention;
      CodeFieldStyle: TTurboCodeFieldStyle;
  end; //}

  PTurboVMInstruction = ^TTurboVMInstruction;
  { Summary the FORTH Virtual Mache Codes}
  TTurboVMInstruction = (
    opNoop,
    {## The FORTH CORE instructions }
    //ErrorCode = 0 means no error
    opHalt, //(ErrorCode -- )
    //Ҫ���ö��ԣ�����ʹ�ò���
    //�����к��Ǳ�����ѹ���
    opAssert, //(ShortString(Msg), expr -- )
    opError,  //(ErrorCode -- )
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
    //the float types
    opFAdd, //(double, double -- double)

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
    
    {## Assignment opstructions }
    opLStrAsg, //assignment the AnsiString (src, dest -- )

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
    opCallFar,  //inCallFar<PTurboModuleRefInfo>(offset of Memory) cfa-addr
    opCallExt,  ////call external function: opCallExt<PTurboMethodInfo>(offset of Memory)
    //inReturn, //= opExit
    opSwitch, //opSwitch<n[uint32], t1,...,tn>  t1..tn is offsets(positive or negative) address 
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

  //the Core VM instructions List
  {: ��������ָ��� } 
  TTurboCoreWords = array [TTurboVMInstruction] of TProcedure;

  {
  @param psHaltError there are some data still in return stack when Halt, the ESP should be point to
    the stack bottom! put the current ESP to TPreservedCodeMemory.ReturnStackBottom.   

  Note: the state Must be a Byte for speed!!!
  }
  TTurboProcessorState = (psHalt, psRunning, psStepping, psCompiling
     , psHaltError
  );
  TTurboProcessorStates = set of TTurboProcessorState;

  PTurboProcessorErrorCode = ^ TTurboProcessorErrorCode;
  {
    @param errOutMem �������ڴ��޿��õĿռ�
    @param errOutOfMetaData MetaData�����޿��õĿռ�
  }
  TTurboProcessorErrorCode = (errNone, errBadInstruction, errInstructionBadParam, errDizZero
    , errModuleNotFound, errMethodNotFound, errTypeInfoNotFound
    , errStaticFieldNotFound, errFieldNotFound
    , errOutOfMem
    , errOutOfMetaData
    , errOutOfDataStack, errOutOfReturnStack
    , errAssertionFailed  
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
  
  cTurboExternalFunctions = [cfsExternalFunction, cfsHostFunction, cfsDLLFunction];
  cTurboNativeModuleTypes = [mtProgram, mtLib, mtObject, mtFunction];
  cTurboExternalModuleTypes = [mtHost, mtDLL];
  cTurboNativeFunctionTypes = [cfsFunction, cfsExternalFunction];

  //��һ�� LongWord��DWord������ţ�TTurboVisibility
  {: �ñ�ʶ��˽�У�û�����֣�û�����ͣ�}
  tvPrivateVisibilty = $00000000;
  {: �ñ�ʶ���������ӵ�������}
  tvPublicVisibilty  = $00000001;
  {: �ñ�ʶ���������ӵ�������,����д���ʶ������}
  tvPublicNamedVisibilty   = $01000001;
  {: �ñ�ʶ���������ӵ�������,���Ҹñ�ʶ��������Ҳ����д�룡}
  tvPublicTypedVisibilty   = $02000001;
  tvPublicNameTypedVisibilty   = $03000001;
  
function IsInteger(const aValue: string): Boolean;
{$IFDEF FPC}
function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
function AnsiDequotedStr(const S: string; AQuote: Char): string;
{$ENDIF}

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


function IsInteger(const aValue: string): Boolean;
begin
  Result := Length(aValue) > 0;
  if Result and (aValue[1] = '$') then
  
end;

{$IFDEF FPC}
function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
var
  P, Dest: PChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := AnsiStrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := AnsiStrScan(Src, Quote);
  end;
  if Src = nil then Src := StrEnd(P);
  if ((Src - P) <= 1) or ((Src - P - DropCount) = 0) then Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PChar(Result);
    Src := AnsiStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := AnsiStrScan(Src, Quote);
    end;
    if Src = nil then Src := StrEnd(P);
    Move(P^, Dest^, Src - P - 1);
  end;
end;

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if ((Result = '') or (LText^ = #0)) and
     (Length(S) > 0) and ((S[1] <> AQuote) or (S[Length(S)] <> AQuote)) then
    Result := S;
end;
{$ENDIF}

end.
