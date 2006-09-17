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
  cMaxTurboVMDirectiveCount = 256; //the max turbo VM code directive count
  
const
  cParameterStackBaseOffset = 0;
  //the size is bytes
  cParameterStackSizeOffset = cParameterStackBaseOffset + SizeOf(Pointer);
  cReturnStackBaseOffset = cParameterStackSizeOffset + SizeOf(Integer);
  //the size is bytes
  cReturnStackSizeOffset = cReturnStackBaseOffset + SizeOf(Pointer);
  cTIBLengthOffset = cReturnStackSizeOffset + SizeOf(Integer);
  cToINOffset = cTIBLengthOffset + SizeOf(Integer);
  cTIBOffset = cToINOffset + SizeOf(Integer);
  cMAXTIBCount = 1024; //Bytes
  cLastWordEntryOffset = cTIBOffset + cMAXTIBCount;

type
  PPreservedCodeMemory = ^ TPreservedCodeMemory;
  //the typecast for code memory area to get the parameters
  TPreservedCodeMemory = packed record
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    TIBLength: Integer; //the text buffer length
    ToIn: Integer; //the text buffer current index
    TIB: array [0..1023] of char;
    LastWordEntry: Pointer;
  end;
  
resourcestring
  rsMissFileHeaderError = 'Error: The file header is missed';
  rsTurboScriptAlreayRunningError = 'Error: The Turbo Script is already running.';
  rsReturnStackUnderflowError = 'Return Stack underflow.';
  rsReturnStackOverflowError = 'Return Stack overflow';
  rsParamStackUnderflowError = 'Parameter Stack underflow.';
  rsParamStackOverflowError = 'Parameter Stack overflow';

type
  ETurboScriptError = class(Exception);
  {: the Module Type }
  {
    @param mtFunction         the script function(word).
  }
  TTurboScriptModuleType = (mtUnknown, mtProgram, mtLib, mtFunction);
  PByte = ^Byte;
  {: The Code(Word) Field }
  TCodeField = packed record
    {: ���÷�ʽ }
    CallStyle: Byte;
    NextCFA: Pointer;
  end;

  TForthVisibility = (fvDefault, fvHide, fvPrivate, fvProtected, fvPublic, fvPublished);
  //the Forth Execution priority fpHighest means cfsImmediately
  TForthPriority = (fpLowest, fpLower, fpLow, fpNormal, fpHigh, pfHigher, fpHighest);
  TForthCallStyle = (csForth, csRegister, csPascal, csStdCall, csFastCall);
  TForthCodeFieldStyle = (cfsSysWord);
  TForthCodeFieldStyles = set of TForthCodeFieldStyle;

  { Summary the FORTH Virtual Mache Codes}
  TVMInstruction = (
    inNone,
    {## The FORTH CORE instructions }
    inHalt,
    inEnter,
    inExit,
    inNext,
    
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

    {## Proc Operation Instruction }
    inJMP,
    inJZ,
    inJNZ,
    inCall,
    inReturn,
    inNoop,

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
  TVMMethodList = array [TVMInstruction] of Pointer;

  PForthWord = ^ TForthWord;
  TForthWordOptions = packed record //a DWORD
      //���ȼ�, 0=low, 1=high equals 1 for an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: TForthPriority; 
      Visibility: TForthVisibility; 
      CallStyle: TForthCallStyle;
      CodeFieldStyle: TForthCodeFieldStyles;
  end;
  //For cast the Mem
  TForthWord = packed record //ITC (Indirect Threaded Code)
    PriorWord: Integer; //PForthWord; //ǰһ������ 0 means Ϊ��ǰ�档

    Options: TForthWordOptions;
    //the Param Field Length 
    ParamFieldLength: LongWord;
    NameLen: Byte;
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //��ʵ����ֱ��ָ���ĳ�����ʵ�PFA�������Ǹ����ʵ�PFA����ֱ��ִ�еĻ�������ѡ�
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //������������PForthWord��������������������ݻ�VM Codes
  end;
  TForthProcessorState = (psRunning, psCompiling, psFinished, psNoData, psBadData, 
    psBadOp, psDivZero, psOverFlow);
  TForthProcessorStates = set of TForthProcessorState;


implementation


end.
