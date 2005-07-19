unit uSuperScriptConsts;

interface

uses
  SysUtils, Classes
  ;

const
  cFORTHHeaderMagicWord = 'SUPER4TH';
  //cFORTHMagicWordSize = SizeOf(cFORTHHeaderMagicWord);
  cDefaultStackSize = 127;
  cDefaultParamStackSize = 127; 
  cDefaultFreeMemSize = 1024 * 8; //the Free Memory 8kb

resourcestring
  rsMissFileHeaderError = 'Error: The file header is missed';
  rsReturnStackUnderflowError = 'Return Stack underflow.';
  rsReturnStackOverflowError = 'Return Stack overflow';
  rsParamStackUnderflowError = 'Parameter Stack underflow.';
  rsParamStackOverflowError = 'Parameter Stack overflow';

type
  ESuperScriptError = class(Exception);
  TSuperForthFileType = (ftProgram, ftLib);
  PByte = ^Byte;
  {: The Code(Word) Field }
  TCodeField = packed record
    {: 调用方式 }
    CallStyle: Byte;
    NextCFA: Pointer;
  end;

  TForthVisibility = (fvDefault, fvHide, fvPrivate, fvProtected, fvPublic, fvPublished);
  TForthPriority = (fpLowest, fpLower, fpLow, fpNormal, fpHigh, pfHigher, fpHighest);
  TForthCallStyle = (csForth, csRegister, csPascal, csStdCall, csFastCall);
  TForthCodeFieldStyle = (cfsImmediately);
  TForthCodeFieldStyles = set of TForthCodeFieldStyle;

  { Summary the FORTH Virtual Mache Codes}
  TVMInstruction = (
    {## The FORTH CORE instructions }
    inEnter,
    inExit,
    inNext,
    
    {## Arithmatic instructions }
    {## for Integer}
    inAddInt, //Add
    inSubInt, //subtract
    inIncInt, //add 1
    inDecInt, //subtract 1
    inMULInt, //multiply 
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

    {## Memory Operation Instruction }
    inSetIntValue,
    inGetIntValue,
    
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
      //优先级, 0=low, 1=high equals 1 for an IMMEDIATE word
      //1=True; 0=False; Smudge bit. used to prevent FIND from finding this word
      //this can be extent to private, protected, public, etc
      Precedence: TForthPriority; 
      Visibility: TForthVisibility; 
      CallStyle: TForthCallStyle;
      CodeFieldStyle: TForthCodeFieldStyles;
  end;
  //For cast the Mem
  TForthWord = packed record //ITC (Indirect Threaded Code)
    PriorWord: Integer; //PForthWord; //前一个单词 0 means 为最前面。

    Options: TForthWordOptions;
    //the Param Field Length 
    ParamFieldLength: LongWord;
    NameLen: Byte;
    //Name: String; it's a PChar, the last char is #0
    //the following is ParameterFields   
    //其实就是直接指向的某个单词的PFA，不过那个单词的PFA就是直接执行的机器码而已。
    //CFA = ParameterFields[0]
    //CFA: LongWord;
    //ParameterFields: array of Integer; //大多数情况下是PForthWord，但是少数情况下是数据或VM Codes
  end;
  TForthProcessorState = (psRunning, psCompiling, psFinished, psNoData, psBadData, 
    psBadOp, psDivZero, psOverFlow);
  TForthProcessorStates = set of TForthProcessorState;


implementation


end.
