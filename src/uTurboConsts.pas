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
  TTurboScriptModuleType = (mtUnknown, mtProgram, mtLib, mtFunction);

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
    //Far Call, ���ڹ����Ĺ�����ģ����õĺ���ȫ��ʹ�ø÷�ʽ
    //ForthDLL��ģ�����ӷ�ʽ����
    inEnterFar, //inEnterFar ModuleIndex Addr
    inExitFar,  //����Զ���õķ���ָ������Ǹ�ָ��! R: (MemoryBase PC -- )
    
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
  {: ��������ָ��� }
  TTurboCoreWords = array [TTurboVMInstruction] of TProcedure;

  PTurboForthWord = ^ TTurboForthWord;
  TTurboForthWordOptions = packed record //a DWORD
      //���ȼ�, highest means an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: TTurboForthPriority; 
      Visibility: TTurboForthVisibility; 
      CallStyle: TTurboForthCallStyle;
      CodeFieldStyle: TTurboForthCodeFieldStyles;
  end;
  //For type-cast the Mem
  TTurboForthWord = packed record //ITC (Indirect Threaded Code)
    PriorWord: PTurboForthWord; //PForthWord; //ǰһ������ 0 means Ϊ��ǰ�档

    Options: TTurboForthWordOptions;
    //the Param Field Length
    //�ú�������ĳ��� 
    ParamFieldLength: LongWord;
    {NameLen: Byte;
    Name: array [0..255] of char;}
    Name: ShortString; //packed
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //��ʵ����ֱ��ָ���ĳ�����ʵ�PFA�������Ǹ����ʵ�PFA����ֱ��ִ�еĻ�������ѡ�
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //������������PForthWord��������������������ݻ�VM Codes
  end;
  TTurboForthProcessorState = (psLoaded, psRunning, psStepping, psCompiling
    , psFinished 
  );
  TTurboForthProcessorStates = set of TTurboForthProcessorState;
  TTurboForthProcessorErrorCode = (errNone, errBadInstruction, errDizZero
    , errMemOverFlow, errDataStackOverflow, errReturnStackOverflow
    , errModuleIndex
  );
  

const
  //For TTurboForthProcessorStates (put it into EBX Status Register).
  {the psLoaded is used for the Executor, not for the VM processor.}
  cTurboScriptIsLoadedBit       = psLoaded;
  cTurboScriptIsRunningBit       = psRunning;
  cTurboScriptIsSteppingBit      = psStepping;
  //cTurboScriptBadInstructionBit  = [psBadInstruction];
  cMaxTurboVMInstructionCount = SizeOf(TTurboCoreWords) div SizeOf(TProcedure); //the max turbo VM code directive count
  
implementation


end.
