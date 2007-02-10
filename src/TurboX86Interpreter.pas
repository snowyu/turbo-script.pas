{: the fastest script
        ����x86ָ���Ż�������ָ����ʵ�֣��Ĵ�������x86�ļĴ���.��Ӧ��ϵ���£�
        }
{ Description
��Ӧ��ϵ���£�
ESP: ���ض�ջָ��.��סѹ����٣��������ӵ�ַ��
EBP: ����ջָ�룬��ַָ������ڴ�ĳ����Ԫ�С�����EBP����ָ���ջ����
EBX: Ϊ����ջջ���� 
ESI: ָ��ǰָ���ַ
EDX: ״̬�Ĵ���(
������������ָ�TurboScript_FullSpeed����ʱʹ�ã�����Ϊ��ʱ�Ĵ���)
TTurboForthProcessorStates, Ϊ���ܿ���ֹͣ,״̬�Ĵ������ڱ����ڴ����ˡ�
EAX: W Register ��ʱ�Ĵ���
ECX:  ��ʱ�Ĵ���

EDI: FDataMemory��ַ

��Щ���Ĺ��������޲����Ĺ���ʵ�֡�

����ʲô��ʽ THREADING TECHNIQUE ��ʵ���أ���������ָ����ò���ֵ�ķ�ʽ��
�û��Զ���ָ�������Ե�ַ��������ռ���˴�����ǰ�������1024���ֽڣ����Ե�ַ������С��255����ʾ��
��ô�ҵĺ�������ָ������������أ�ȫ�ֱ�������ʽ��

�������ڴ澵��
FParameterStackBase(Pointer:������ջ��ַ) FParameterStackSize(Integer:
������ջ��С)
ReturnStackBase(Pointer: ����ջ��ַ) ReturnStackSize(Integer: ����ջ��С)
TIBLength(Integer) ToIn(Integer) TIB(PChar: 1024) LastWordEntry(Pointer:
�û��Զ��嵥�������)
type //in uTurboScriptConsts
  PPreservedCodeMemory = ^ TPreservedCodeMemory;
  //the typecast for code memory area to get the parameters
  TPreservedCodeMemory = packed record
    Executor: TCustomTurboExecutor;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    TIBLength: Integer; //the text buffer length
    ToIn: Integer; //the text buffer current index
    TIB: array [0..cMAXTIBCount-1] of char;
    LastWordEntry: Pointer; //�����ֵĺ�������
     LastVarEntry: Pointer; //�����ֵı�������
  end;
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
  TTurboX86Interpreter = class(TCustomTurboExecutor)
  private
    FOldEBP: Integer;
    FOldEBX: Integer;
    FOldEDI: Integer;
    FOldEDX: Integer;
    FOldESI: Integer;
    FOldESP: Integer;
  protected
    function iExecuteCFA(const aCFA: Integer): Integer; override;
  public
    destructor Destroy; override;
  end;


var
  GTurboCoreWords: TTurboCoreWords;

implementation

procedure iVMInit;forward;
procedure iVMNext;forward;
procedure _iVMHalt(ErrorCode: TTurboProcessorErrorCode);forward;
procedure iVMHalt;forward;
procedure iVMEnter;forward;
procedure vEmitLString;forward;
procedure _iVMErrorAt(ErrorCode: tsInt; ErrorAddr: Pointer);forward;

type
  TTurboMemoryModuleAccess = class(TCustomTurboModule);
  TTurboExecutorAccess = class(TCustomTurboExecutor);
  TTurboMetaInfoAccess = object(TTurboMetaInfo)
  end;

{
***************************** TTurboX86Interpreter *****************************
}
destructor TTurboX86Interpreter.Destroy;
begin
  inherited Destroy;
end;

function TTurboX86Interpreter.iExecuteCFA(const aCFA: Integer): Integer;

  {$ifdef NoSuchDef}

begin
  {$else}
  asm
  {$endif}
  {  PUSH EAX
    PUSH EDX
    CALL TCustomTurboModule.ExecuteCFA
    POP  EDX
    POP  EAX
  }
    MOV  Self.FOldESP, ESP
    MOV  Self.FOldEBP, EBP
    MOV  Self.FOldESI, ESI
    MOV  Self.FOldEDI, EDI
    MOV  Self.FOldEBX, EBX
    MOV  ESP, Self.FRP //return stack pointer: RP
    //PUSHAD
    //PUSH EAX
    //PUSH [EAX].FMemory
    MOV  EDI, [EAX].FMemory
    MOV  EDI, [EDI].TTurboMemoryModuleAccess.FDataMemory
    //PUSH [EAX].FParameterStack

    MOV  ESI, [EAX].FMemory  //ESI: IP
    MOV  ESI, [ESI].TTurboMemoryModuleAccess.FMemory  //ESI: IP
    ADD  ESI, aCFA
    //BTS  EDX, psRunning
    //MOV  [EDI].TPreservedCodeMemory.States, DL
    MOV  EBP, [EAX].FSP //SP the data stack pointer.
    XOR  EBX, EBX //clear the TOS
    //MOV  EDX, EAX
    //STD  //the EDI will be decremented.
    CLD //the esi will be incremented.
    //PUSH @@ReturnAdr
    CALL  iVMInit
  @@ReturnAdr:
    MOV  EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
    CMP  EBP, [EAX].TTurboGlobalOptions.ParamStackBottom
  //  CMP  EBP, [EDI].TPreservedCodeMemory.ParamStackBottom
    JE   @@skipStoreTOS
    //��������ָ��ջ��
    //����ջ�У�ջ�׵�������������ġ�
    XCHG ESP, EBP
    PUSH EBX
    XCHG ESP, EBP
  @@skipStoreTOS:

    //MOV [ESI]
    //POP EAX
    //POP EAX
    //POP EAX
    MOV  EAX, [EDI].TTurboPreservedDataMemory.Executor
    MOV  [EAX].TTurboX86Interpreter.FRP, ESP
    MOV  [EAX].TTurboX86Interpreter.FSP, EBP
    MOV  [EAX].TTurboX86Interpreter.FPC, ESI

    MOV  ESP, [EAX].TTurboX86Interpreter.FOldESP
    MOV  EBP, [EAX].TTurboX86Interpreter.FOldEBP
    MOV  EBX, [EAX].TTurboX86Interpreter.FOldEBX
    MOV  ESI, [EAX].TTurboX86Interpreter.FOldESI
    MOV  EDI, [EAX].TTurboX86Interpreter.FOldEDI
  //end;
end;


{----Helper functions ----}

  
procedure iVMInit;
asm
  MOV  EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
 
  MOV  [EAX].TTurboGlobalOptions.ReturnStackBottom, ESP
  MOV  [EAX].TTurboGlobalOptions.ParamStackBottom, EBP
  //LEA  ECX, GTurboCoreWords
  //MOV  DL, [EDI].TPreservedCodeMemory.States
  JMP  iVMNext
end;

//the interpreter core here:
procedure iVMNext;
asm
  MOV  EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  MOV  DL, [EAX].TTurboGlobalOptions.States

  //TODO: BT is a 486 directive.
  BT EDX, psRunning //cTurboScriptIsRunningBit
  JNC @@Exit

@@DoEnter:
  //MOV EAX, [ESI]  //the current instruction in W register
  //ADD ESI, Type(tsInt) //4 = INC PC INC PC INC PC INC PC
  XOR EAX, EAX
  LODSB
  //MOVZX EAX, AL //the XOR EAX,EAX is faster! 
  
@@ExecInstruction:
  //CMP  EAX, cMaxTurboVMInstructionCount
  //JAE   @@IsUserWord
@@IsVMCode:
  MOV  ECX, OFFSET GTurboCoreWords
  MOV  EAX, [ECX+EAX*Type(tsInt)]
  TEST EAX, EAX
  //CMP  EAX, 0
  JZ   @@BadOpError
  JMP  EAX
@@BadOpError:
  //MOV  EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  //MOV  [EAX].TTurboGlobalOptions.LastErrorCode, errBadInstruction
  MOV  EAX, errBadInstruction
  JMP  _iVMHalt
  //Bad OpCode: no procedure assigned to the OpCode.
{@@IsUserWord:
  ADD  EAX, EDI //ָ���û������word���
  
  //JMP  iVMEnter
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP  @@DoEnter
}

@@Exit:
{ //move to iVMHalt
  MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
  CMP  ESP, [EAX].TTurboGlobalOptions.ReturnStackBottom
  JZ   @@byebye  //ok
@@HaltErr:
  BTS  EDX, errHalt
  MOV  [EDI].TPreservedCodeMemory.States, DL
  MOV  ECX, ESP
  MOV  ESP, [EAX].TTurboGlobalOptions.ReturnStackBottom
  MOV  [EAX].TTurboGlobalOptions.ReturnStackBottom, ECX
@@byebye:
//}
end;

procedure iVMEnter;
asm
  LODSD
@DoVMEnter:
  ADD  EAX, [EDI].TTurboPreservedDataMemory.Code
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
end;

//input EAX the word CFA. 
procedure _DoVMEnter;
asm
  ADD  EAX, [EDI].TTurboPreservedDataMemory.Code
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
end;

//(ErrorCode -- )
procedure iVMHalt;
asm
  MOV EAX, EBX

  //move the top in stack to EAX 
  MOV  EBX, [EBP] 
  //Increment the data stack pointer.
  //ADD  EBP, Type(tsInt)
  INC EBP
  INC EBP
  INC EBP
  INC EBP
  
  JMP _iVMHalt 
end;

procedure iVMNoop;
asm
  JMP  iVMNext
end;

procedure _iVMHalt(ErrorCode: TTurboProcessorErrorCode);assembler;
asm
{  MOV DL, [EDI].TPreservedCodeMemory.States
  BTR EDX, psRunning //cTurboScriptIsRunningBit  //clear the cIsRunningBit to 0.
  MOV [EDI].TPreservedCodeMemory.States, DL
  JMP iVMNext
}
  MOV  ECX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  MOV  [ECX].TTurboGlobalOptions.LastErrorCode, ErrorCode  
  //cmpare the ESP and ReturnStackBottom, it should be equ.
  CMP  ESP, [ECX].TTurboGlobalOptions.ReturnStackBottom
  MOV  DL, [ECX].TTurboGlobalOptions.States
  BTR  EDX, psRunning //cTurboScriptIsRunningBit  //clear the cIsRunningBit to 0.
  BTS  EDX, psHalt
  BTR  EDX, psHaltError
  JZ   @@byebye  //ok
@@HaltErr:
  BTS  EDX, psHaltError
  //MOV  [ECX].TTurboGlobalOptions.LastErrorCode, errHalt
  MOV  EAX, ESP
  MOV  ESP, [ECX].TTurboGlobalOptions.ReturnStackBottom
  MOV  [ECX].TTurboGlobalOptions.ReturnStackBottom, EAX
@@byebye:
  MOV  [ECX].TTurboGlobalOptions.States, DL
end;

//(ErrorCode, ErrorAdr -- )
procedure iVMErrorAt;
asm
  MOV  EAX, [EBP]
  MOV  EDX, EBX
   INC  EBP
   INC  EBP
   INC  EBP
   INC  EBP
  MOV  EBX, [EBP]
   INC  EBP
   INC  EBP
   INC  EBP
   INC  EBP
  JMP  _iVMErrorAt
end;

procedure _iVMErrorAt(ErrorCode: tsInt; ErrorAddr: Pointer);
asm
  MOV  ECX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  MOV  [ECX].TTurboGlobalOptions.ErrorAddr, ErrorAddr
  JMP  _iVMHalt
end;

//(ErrorCode -- )
procedure iVMError;
asm
  MOV  EAX, EBX
  MOV  EDX, [ESP]  //the TOS is ErrorAddr Now.
  MOV  EBX, [EBP]
  INC  EBP
  INC  EBP
  INC  EBP
  INC  EBP

  JMP  _iVMErrorAt
end;

procedure iVMExit;
asm
  POP  ESI
  JMP  iVMNext
end;

procedure iVMExitFar;
asm
  //���������������Ļ�������Լ���������������˳���ʱ��ͻ�ֹͣ
  //���⣬������������ʱ�򷢲�ֹͣ����������ȴ������״̬����

  POP  ESI
  POP  EDI

  JMP  iVMNext
end;

//save the old MemoryBase, pass the CPUStates to the new MemoryBase.
procedure iVMEnterFar;
asm
  PUSH EDI //save the current MemoryBase.
  LODSD
  TEST EAX, EAX //CMP EAX, 0
  JZ @@LocalEnter 
  MOV  EDI, EAX //load the new MemoryBase

@@LocalEnter:
  //JMP iVMEnter
  LODSD
  ADD EAX, [EDI].TTurboPreservedDataMemory.Code
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
end;

//CALLFAR PTurboModuleInfo cfa-addr
//if PTurboModuleInfo = nil means it's self, do not lookup.  
//�ڷ���ջ�б���EDI(�ɵ� FMemory ��ַ), 
//���� PTurboModuleInfo ����ģ���ڴ��ַ������ҵ�������EDI���µ� FDataMemory ��ַ,
//Ȼ��װ��ú����ĵ�ַ�������ͺ�VMEnterһ���ˣ�תȥVMEnter��
procedure iVMCallFar;
asm
  PUSH EDI //save the current MemoryBase.
  LODSD    //EAX= PTurboModuleInfo
  TEST EAX, EAX //CMP EAX, 0
  JZ  @@DoLocalEnterFar
  ADD  EAX, EDI //PTurboModuleRefInfo real addr
  MOV  ECX, [EAX].TTurboModuleRefInfo.Handle
  TEST ECX, ECX //CMP ECX, 0
  JZ   @@RequireModule
  MOV  EDI, [ECX].TTurboExecutorAccess.FDataMemory
  JMP  @@exit

@@RequireModule: //find and load the module into the memory.
  PUSH EAX  //keep the PTurboModuleInfo 
  PUSH EBX
  PUSH ESI
  PUSH EBP
  
  //MOV  EDX, EAX
  MOV  EDX, [EAX].TTurboMetaInfoAccess.FName
  MOV  EAX, [EDI].TTurboPreservedDataMemory.Executor
  //function TCustomTruboExecutor.GetModuleMemoryAddr(aModuleIndex: Integer): Pointer;
  CALL TCustomTurboExecutor.RequireModule
  POP EBP
  POP ESI
  POP EBX
  POP ECX  //restore the PTurboModuleInfo

  TEST  EAX, EAX
  JZ   @@NotFoundError

  MOV [ECX].TTurboModuleRefInfo.Handle, EAX
  //Copy CPU States to the New Module Memory.
  //MOV EDI, [ESP] //restore the old Module MemoryBase in TOS
  //MOV  CL, [EDI].TPreservedCodeMemory.States
  MOV  EDI, [EAX].TTurboExecutorAccess.FDataMemory
  //MOV  [EDI].TPreservedCodeMemory.States, CL
  JMP @@Exit

@@NotFoundError:
  POP  EDI
  //MOV  EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  MOV  EAX, errModuleNotFound
  JMP  _iVMHalt

@@DoLocalEnterFar:

@@Exit:
  //JMP iVMEnter
  LODSD
  ADD EAX, [EDI].TTurboPreservedDataMemory.Code
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
end;

procedure _DoAssert;
asm
  MOV  EDX, [EBP]  //EDX <- the second Stack TOp
  ADD  EDX, EDI 
  MOV  EAX, [EDI].TTurboPreservedDataMemory.Executor

  PUSH EDI
  PUSH EBX
  PUSH ESI
  PUSH EBP
  //MOV  ESI, [EAX]
  CALL TTurboX86Interpreter.DoPrintShortString
  POP  EBP
  POP  ESI
  POP  EBX
  POP  EDI

   INC  EBP
   INC  EBP
   INC  EBP
   INC  EBP
   MOV  EBX, [EBP]
   INC  EBP
   INC  EBP
   INC  EBP
   INC  EBP

   MOV  EAX, errAssertionFailed
   MOV  EDX, [ESP]
   JMP  _iVMErrorAt
end;

procedure iVMAssert;
asm
   CMP  EBX, 0 
   JZ   _DoAssert
@SkipAssert:
   INC  EBP
   INC  EBP
   INC  EBP
   INC  EBP

   MOV  EBX, [EBP]
   INC  EBP
   INC  EBP
   INC  EBP
   INC  EBP

   JMP  iVMNext
end;

//call(EXECUTE) the user defined word
//(CFA --- )
procedure iVMExecute;
asm
  //push the current IP.
  PUSH ESI        
  //set the new IP in the TOS
  ADD  EBX, [EDI].TTurboPreservedDataMemory.Code //EBX: TOS
  MOV  ESI, EBX   

  {XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP}
  MOV EBX, [EBP]
  ADD EBP, Type(tsInt)

  JMP  iVMNext
end;

//this is a Push Integer(����������) directive
procedure iVMPushInt;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  {XCHG ESP, EBP
  PUSH EBX
  XCHG ESP, EBP //}
  SUB  EBP, Type(tsInt)
  MOV  [EBP], EBX
  LODSD
  MOV  EBX, EAX
  JMP  iVMNext
end;

//this is a Push Byte(����������) directive
procedure iVMPushByte;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  {XCHG ESP, EBP
  PUSH EBX
  XCHG ESP, EBP //}
  SUB  EBP, Type(tsInt)
  MOV  [EBP], EBX
 
  XOR EAX, EAX
  LODSB
  MOV  EBX, EAX
  JMP  iVMNext
end;

//this is a Push Word(����������) directive
procedure iVMPushWord;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  {XCHG ESP, EBP
  PUSH EBX
  XCHG ESP, EBP //}
  SUB  EBP, Type(tsInt)
  MOV  [EBP], EBX

  XOR EAX, EAX
  LODSW
  MOV  EBX, EAX
  JMP  iVMNext
end;

procedure iVMDropInt;
asm
  {XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP //}
  //move the top in stack to EAX 
  MOV  EBX, [EBP] 
  //Increment the data stack pointer.
  ADD  EBP, Type(tsInt)

  JMP  iVMNext
end;

procedure iVMPushInt64;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  XCHG ESP, EBP
  PUSH EBX
  LODSD
  MOV  EBX, EAX
  LODSD
  PUSH EAX
  XCHG ESP, EBP
  JMP  iVMNext
end;

procedure iVMDropInt64;
asm
  XCHG ESP, EBP
  POP  EBX
  POP  EBX
  XCHG ESP, EBP
  {//move the top in stack to EAX 
  MOV  EDX, [EBP] 
  //Increment the data stack pointer.
  ADD  EBP, Type(Integer)
  }
  JMP  iVMNext
end;
//(n, n1) -- (n = n + n1)
procedure iVMAddInt;
asm
  ADD EBX, [EBP]
  ADD EBP, Type(tsInt)
  JMP  iVMNext
end;

//(int64, int64-1) -- (int64 = int64 + int64-1)
procedure iVMAddInt64;
asm
  ADD EBX, [EBP+Type(tsInt)]
  MOV EAX, [EBP]
  ADC EAX, [EBP+(Type(tsInt)*2)]
  ADD EBP, Type(tsInt)*2
  MOV [EBP], EAX
  JMP  iVMNext
end;


//(n, n1) -- (n = n1 - n)
procedure iVMSubInt;
asm
  SUB EBX, [EBP]
  ADD EBP, Type(tsInt)
  JMP  iVMNext
end;

//(Doublea, Doubleb) -- (Double = Doubleb + Doublea)
procedure iVMAddDouble;
asm
  DEC  EBP
  DEC  EBP
  DEC  EBP
  DEC  EBP
  MOV  [EBP], EBX
  FLD  qword ptr [EBP]
  FADD qword ptr [EBP+Type(qword)]
  SHL  EBP, 3 //=Add EBP, 8 2^3
  FSTP [EBP]
  MOV  EBX, [EBP]
  SHL  EBP, 2 //=Add EBP, 4 2^2 
  JMP  iVMNext
end;

//(int64a, int64b) -- (int64 = int64b - int64a)
procedure iVMSubInt64;
asm
  SUB EBX, [EBP+Type(tsInt)]
  MOV EAX, [EBP]
  SBB EAX, [EBP+(Type(tsInt)*2)]
  ADD EBP, Type(tsInt)*2
  MOV [EBP], EAX
  JMP  iVMNext
end;

// Unsigned multiply
//(n, n1) -- (DoubleWord = n * n1)
//EAX(n): the result of Low orders; n1 the result of high orders
procedure iVMMulUnsignedInt;
asm
  MOV  EAX, EBX
  MUL  [EBP] //EDX:EAX = EAX * [EBP]
  MOV  [EBP], EDX
  MOV  EBX, EAX
  JMP  iVMNext
end;

// multiply
//(n, n1) -- total
//total = n* n1(int)
procedure iVMMulInt;
asm
  MOV  EAX, EBX
  //IMUL  EAX, [EBP] //EDX:EAX = EAX * [EBP]
  IMUL  EBX, [EBP] //EBX = EBX * [EBP]
  //MOV  [EBP], EDX
  //MOV  EBX, EAX
  ADD  EBP, TYPE(tsInt)
  JMP  iVMNext
end;

procedure vFetchInt;
asm
  //EBX is TOS
  MOV EAX, EDI
  MOV EBX, [EAX+EBX] //[TOS + FDataMemory]
  JMP  iVMNext
end;

procedure vFetchByte;
asm
  //EBX is TOS
  MOV EAX, EBX
  XOR EBX, EBX
  MOV ECX, EDI
  MOV BL, [ECX+EAX]
  JMP  iVMNext
end;

procedure vFetchWord;
asm
  //EBX is TOS
  MOV EAX, EBX
  XOR EBX, EBX
  MOV ECX, EDI
  MOV BX, [ECX+EAX]
  JMP  iVMNext
end;

procedure vFetchInt64;
asm
  //EBX is TOS
  //MOV EAX, [EDI].TPreservedCodeMemory.Data
  ADD EBX, EDI //TOS <- TOS + FDataMem
  MOV EAX, [EBX+4]
  MOV EBX, [EBX]

{ //push EAX the high 32bit of the int64
  SUB EBP, Type(tsInt)
  MOV [EBP], EAX
}
  XCHG ESP, EBP
  PUSH EAX
  XCHG ESP, EBP
  JMP  iVMNext
end;

//(int addr -- )
procedure vStoreInt;
asm
  ADD EBX, EDI 
  MOV EAX, [EBP]
  MOV [EBX], EAX
  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP
  MOV EBX, [EBP]
  //check whether is stackbottom
  MOV EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  CMP EBP, [EAX].TTurboGlobalOptions.ParamStackBase
  JLE @@exit //already is bottom then exit.
  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP
@@exit:  
  JMP  iVMNext
end;

procedure vStoreInt64;
asm
  ADD EBX, EDI 
  MOV EAX, [EBP]
  MOV [EBX], EAX
  MOV EAX, [EBP+4]
  MOV [EBX+4], EAX
  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP

  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP
  MOV EBX, [EBP]
  //check whether is stackbottom
  MOV EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  CMP EBP, [EAX].TTurboGlobalOptions.ParamStackBase
  JLE @@exit //already is bottom then exit.
  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP
@@exit:  
  JMP  iVMNext
end;

procedure vStoreWord;
asm
  ADD EBX, EDI 
  MOV EAX, [EBP]
  MOV [EBX], AX
  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP
  MOV EBX, [EBP]
  //check whether is stackbottom
  MOV EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  CMP EBP, [EAX].TTurboGlobalOptions.ParamStackBase
  JLE @@exit //already is bottom then exit.
  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP
@@exit:  
  JMP  iVMNext
end;

procedure vStoreByte;
asm
  ADD EBX, EDI 
  MOV EAX, [EBP]
  MOV [EBX], AL
  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP
  MOV EBX, [EBP]
  //check whether is stackbottom
  MOV EAX, [EDI].TTurboPreservedDataMemory.GlobalOptions
  CMP EBP, [EAX].TTurboGlobalOptions.ParamStackBase
  JLE @@exit //already is bottom then exit.
  INC EBP 
  INC EBP 
  INC EBP 
  INC EBP
@@exit:  
  JMP  iVMNext
end;

//print a char
//{c -- }
procedure vEmitChar;
asm
  MOV  DL, BL  //DL <- TOS
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  MOV  EAX, [EDI].TTurboPreservedDataMemory.Executor

  PUSH EDI
  PUSH EBX
  PUSH ESI
  PUSH EBP
  MOV  ESI, [EAX]
  CALL DWORD PTR [ESI + VMTOFFSET TTurboX86Interpreter.DoPrintChar]
  POP  EBP
  POP  ESI
  POP  EBX
  POP  EDI
  JMP  iVMNext
end;

//print ShortString
procedure vEmitString;
asm
  MOV  EDX, EBX  //EDX <- TOS
  ADD  EDX, EDI 
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  MOV  EAX, [EDI].TTurboPreservedDataMemory.Executor

  PUSH EDI
  PUSH EBX
  PUSH ESI
  PUSH EBP
  //MOV  ESI, [EAX]
  CALL TTurboX86Interpreter.DoPrintShortString
  POP  EBP
  POP  ESI
  POP  EBX
  POP  EDI
  JMP  iVMNext
end;

//print AnsiString
procedure vEmitLString;
asm
  MOV  EDX, EBX  //EDX <- TOS
  ADD  EDX, EDI 
  //XCHG ESP, EBP
  //POP  EBX
  //XCHG ESP, EBP
  MOV  EBX, [EBP]
  //ADD EBP, Type(tsInt)
  INC  EBP
  INC  EBP
  INC  EBP
  INC  EBP
  MOV  EAX, [EDI].TTurboPreservedDataMemory.Executor

  PUSH EDI
  PUSH EBX
  PUSH ESI
  PUSH EBP
  MOV  ESI, [EAX]  //ESI <--- VMT For VMT Lookup.
  CALL DWORD PTR [ESI + VMTOFFSET TTurboX86Interpreter.DoPrintString]
  POP  EBP
  POP  ESI
  POP  EBX
  POP  EDI
@Skip:
  JMP  iVMNext
end;

//(-- int64)
procedure vGetTickCount;
asm
  XCHG ESP, EBP
  PUSH EBX
  PUSH 0
  PUSH 0
  XCHG ESP, EBP
  PUSH EBP
  CALL QueryPerformanceCounter
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  JMP  iVMNext
end;

//(int64Addr --)
procedure vStoreTickCount;
asm
  ADD  EBX, EDI
  PUSH EBX
  CALL QueryPerformanceCounter
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  JMP  iVMNext
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
