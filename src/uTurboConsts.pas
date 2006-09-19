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

  TTurboForthVisibility = (fvDefault, fvHide, fvPrivate, fvProtected, fvPublic, fvPublished);
  //the Forth Execution priority fpHighest means cfsImmediately
  TTurboForthPriority = (fpLowest, fpLower, fpLow, fpNormal, fpHigh, pfHigher, fpHighest);
  TTurboForthCallStyle = (csForth, csRegister, csPascal, csStdCall, csFastCall);
  TTurboForthCodeFieldStyle = (cfsSysWord);
  TTurboForthCodeFieldStyles = set of TTurboForthCodeFieldStyle;

  { Summary the FORTH Virtual Mache Codes}
  TTurboVMInstruction = (
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
  {: 核心虚拟指令表 }
  TTurboCoreWords = array [TTurboVMInstruction] of TProcedure;

  PTurboForthWord = ^ TTurboForthWord;
  TTurboForthWordOptions = packed record //a DWORD
      //优先级, 0=low, 1=high equals 1 for an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: TTurboForthPriority; 
      Visibility: TTurboForthVisibility; 
      CallStyle: TTurboForthCallStyle;
      CodeFieldStyle: TTurboForthCodeFieldStyles;
  end;
  //For cast the Mem
  TTurboForthWord = packed record //ITC (Indirect Threaded Code)
    PriorWord: PTurboForthWord; //PForthWord; //前一个单词 0 means 为最前面。

    Options: TTurboForthWordOptions;
    //the Param Field Length
    //该函数主体的长度 
    ParamFieldLength: LongWord;
    {NameLen: Byte;
    Name: array [0..255] of char;}
    Name: ShortString; //packed
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //其实就是直接指向的某个单词的PFA，不过那个单词的PFA就是直接执行的机器码而已。
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //大多数情况下是PForthWord，但是少数情况下是数据或VM Codes
  end;
  {TForthProcessorState = (psRunning, psCompiling, psFinished, psNoData, psBadData, 
    psBadOp, psDivZero, psOverFlow);
  TForthProcessorStates = set of TForthProcessorState;
  }

implementation


end.
