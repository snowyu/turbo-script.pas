unit uSuperScriptConsts;

interface

uses
  SysUtils, Classes
  ;

const
  cFORTHHeaderMagicWord = 'SUPER4TH';
  cDefaultStackSize = 127;
  cDefaultParamStackSize = 127; 

resourcestring
  rsMissFileHeaderError = 'Error: The file header is missed';

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
  { Summary the FORTH Virtual Mache Codes}
  TVMInstruction = (
    {## Arithmatic instuctions }
    {## for Integer}
    inAddInt, //Add
    inSubInt, //subtract
    inIncInt, //add 1
    inDecInt, //subtract 1
    inMULInt, //multiply 
    inDIVInt, //divide
    inIncNInt, //add N
    inDecNInt, //subtract N

    {## Logical instuctions }
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

implementation


end.
