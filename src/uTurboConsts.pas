{: the turbo script type and constants }
unit uTurboScriptConsts;

interface

uses
  SysUtils, Classes
  ;

const
  cFORTHHeaderMagicWord = 'TURBO4TH';
  //cFORTHMagicWordSize = SizeOf(cFORTHHeaderMagicWord);
  cDefaultStackSize = 127;
  cDefaultParamStackSize = 127; 
  cDefaultFreeMemSize = 1024 * 8; //the Free Memory 8kb
  cMAXTIBCount = 1024;
  
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
  
type
  ETurboScriptError = class(Exception);
  {: the Module Type }
  {
    @param mtFunction         the script function(word).
  }
  TTurboModuleType = (mtUnknown, mtProgram, mtLib, mtFunction);

  TTurboForthVisibility = (fvDefault, fvHide, fvPrivate, fvProtected, fvPublic, fvPublished);
  //the Forth Execution priority fpHighest means cfsImmediately
  TTurboForthPriority = (fpLowest, fpLower, fpLow, fpNormal, fpHigh, pfHigher, fpHighest);
  TTurboForthCallStyle = (csForth, csRegister, csPascal, csStdCall, csFastCall);
  TTurboForthCodeFieldStyle = (cfsFunction, cfsVariable);
  TTurboForthCodeFieldStyles = set of TTurboForthCodeFieldStyle;

  { Summary the FORTH Virtual Mache Codes}
  TTurboVMInstruction = (
    inNone,
    {## The FORTH CORE instructions }
    inHalt,
    inEnter,
    inExit,
    inNext,
    //Far Call, 对于公开的供其他模块调用的函数全部使用该方式
    //ForthDLL的模块链接方式调用
    inEnterFar, //inEnterFar ModuleIndex Addr
    inExitFar,  //对于远调用的返回指令必须是该指令! R: (MemoryBase PC -- )
    
    {## Memory Operation Instruction }
    inStoreInt, 
    inStoreByte, //CStore
    inFetchInt,
    inFetchByte, //CFetch

    {## Arithmatic instructions }
    {## for Integer}
    inAddInt, //Add
    inSubInt, //subtract
    inIncInt, //add 1
    inDecInt, //subtract 1
    inMULUnsignedInt, //Unsigned multiply 
    inDIVInt, //divide
    inIncNInt, //add N
    inDecNInt, //subtract N

    {## Logical instructions }
    {## for Integer}
    inEQUInt,
    inNEQInt, // not equ
    inLESInt, //less than
    inLEQInt, //less than and equ
    inGERInt, //greater than
    inGEQInt, //greater than and equ
    inNOTInt, //Negate(NOT)
    inANDInt,
    inORInt,
    inXORInt,

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
    inExecute, //call(EXECUTE) the (User defined Forth word) (CFA -- )
    //inReturn, //= inExit
    inNoop,
    //Call other module subroutes.

    {## Stack Operation Instuction }
    inPushInt,
    inPopInt,
    inDropInt,
    inDUPInt,
    inSWAPInt,
    inOVERInt,
    inROTInt
  ); 

  //the Core procedure List, maybe procedure or method.
  {: 核心虚拟指令表 }
  TTurboCoreWords = array [TTurboVMInstruction] of TProcedure;

  TTurboForthWordOptions = packed record //a DWORD
      //优先级, highest means an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: TTurboForthPriority; 
      Visibility: TTurboForthVisibility; 
      CallStyle: TTurboForthCallStyle;
      CodeFieldStyle: TTurboForthCodeFieldStyles;
  end;
  {
  @param errHalt there are some data stil in return stack when Halt, the ESP should be point to
    the stack bottom! put the current ESP to TPreservedCodeMemory.ReturnStackBottom.   

  Note: the state Must be a Byte for speed!!!
  }
  TTurboForthProcessorState = (psRunning, psStepping, psCompiling
    //这些错误可能会同时出现，所以放在这里
    , errHalt, errMemOverFlow, errDataStackOverflow, errReturnStackOverflow 
  );
  TTurboForthProcessorStates = set of TTurboForthProcessorState;
  TTurboForthProcessorErrorCode = (errNone, errBadInstruction, errDizZero
    , errModuleIndex
  );

  

const
  //For TTurboForthProcessorStates (put it into EBX Status Register).
  {the psLoaded is used for the Executor, not for the VM processor.}
  //cTurboScriptIsLoadedBit       = psLoaded;
  cTurboScriptIsRunningBit       = psRunning;
  cTurboScriptIsSteppingBit      = psStepping;
  //cTurboScriptBadInstructionBit  = [psBadInstruction];
  cMaxTurboVMInstructionCount = SizeOf(TTurboCoreWords) div SizeOf(TProcedure); //the max turbo VM code directive count
  
implementation


end.
