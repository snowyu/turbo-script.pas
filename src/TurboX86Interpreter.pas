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

EDI: FMemory��ַ

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
procedure iVMHalt;forward;
procedure iVMEnter;forward;

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
    //PUSH [EAX].FParameterStack

    MOV  ESI, EDI  //ESI: IP
    ADD  ESI, aCFA
    //BTS  EDX, psRunning
    //MOV  [EDI].TPreservedCodeMemory.States, DL
    {$ifdef TurboScript_FullSpeed}
    MOV  DL, [EDI].TPreservedCodeMemory.States //EBX: FORTH Processor States
    {$endif}
    MOV  EBP, [EAX].FSP //SP the data stack pointer.
    XOR  EBX, EBX //clear the TOS
    //MOV  EDX, EAX
    //STD  //the EDI will be decremented.
    CLD //the esi will be incremented.
    //PUSH @@ReturnAdr
    CALL  iVMInit
  @@ReturnAdr:
    MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
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
    MOV  EAX, [EDI].TPreservedCodeMemory.Executor
    MOV  [EAX].TTurboX86Interpreter.FRP, ESP
    MOV  [EAX].TTurboX86Interpreter.FSP, EBP
    MOV  [EAX].TTurboX86Interpreter.FPC, ESI
    {$ifdef TurboScript_FullSpeed}
    MOV  [EDI].TPreservedCodeMemory.States, DL
    {$endif}

    MOV  ESP, [EAX].TTurboX86Interpreter.FOldESP
    MOV  EBP, [EAX].TTurboX86Interpreter.FOldEBP
    MOV  EBX, [EAX].TTurboX86Interpreter.FOldEBX
    MOV  ESI, [EAX].TTurboX86Interpreter.FOldESI
    MOV  EDI, [EAX].TTurboX86Interpreter.FOldEDI
  //end;
end;


{----Helper functions ----}

type
  TTurboExecutorAccess = class(TCustomTurboExecutor);
  
procedure iVMInit;
asm
  MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
 
  MOV  [EAX].TTurboGlobalOptions.ReturnStackBottom, ESP
  MOV  [EAX].TTurboGlobalOptions.ParamStackBottom, EBP
  //LEA  ECX, GTurboCoreWords
  //MOV  DL, [EDI].TPreservedCodeMemory.States
  JMP  iVMNext
end;

//the interpreter core here:
procedure iVMNext;
asm
  MOV  DL, [EDI].TPreservedCodeMemory.States

  //TODO: BT is a 486 directive.
  BT EDX, psRunning //cTurboScriptIsRunningBit
  JNC @@Exit

@@DoEnter:
  //MOV EAX, [ESI]  //the current instruction in W register
  //ADD ESI, Type(Pointer) //4 = INC PC INC PC INC PC INC PC
  XOR EAX, EAX
  LODSB
  //MOVZX EAX, AL //the XOR EAX,EAX is faster! 
  
@@ExecInstruction:
  //CMP  EAX, cMaxTurboVMInstructionCount
  //JAE   @@IsUserWord
@@IsVMCode:
  MOV  ECX, OFFSET GTurboCoreWords
  MOV  EAX, [ECX+EAX*4]
  CMP  EAX, 0
  JZ   @@BadOpError
  JMP  EAX
@@BadOpError:
  MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
 
  MOV  [EAX].TTurboGlobalOptions.LastErrorCode, errBadInstruction
  JMP  iVMHalt
  //Bad OpCode: no procedure assigned to the OpCode.
@@IsUserWord:
  ADD  EAX, EDI //ָ���û������word���
  
  //JMP  iVMEnter
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP  @@DoEnter
@@Exit:
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

end;

procedure iVMEnter;
asm
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
end;

procedure iVMCall;
asm
  LODSD
  PUSH ESI        //push the current IP.
  ADD  EAX, EDI
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
end;

procedure iVMHalt;
asm
  MOV DL, [EDI].TPreservedCodeMemory.States
  BTR EDX, psRunning //cTurboScriptIsRunningBit  //clear the cIsRunningBit to 0.
  MOV [EDI].TPreservedCodeMemory.States, DL
  JMP iVMNext
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
{  MOV DL, [EDI].TPreservedCodeMemory.States
  BTR EDX, psRunning //cTurboScriptIsRunningBit  //clear the cIsRunningBit to 0.
  MOV [EDI].TPreservedCodeMemory.States, DL
  //Add this:
  BTS EDX, psRunning //cTurboScriptIsRunningBit  //set the cIsRunningBit to 1.
} 
  POP  ESI
  POP  EDI
  //MOV [EDI].TPreservedCodeMemory.States, DL
  JMP  iVMNext
end;

//save the old MemoryBase, pass the CPUStates to the new MemoryBase.
procedure iVMEnterFar;
asm
  MOV DL, [EDI].TPreservedCodeMemory.States
  PUSH EDI //save the current MemoryBase.
  LODSD
  MOV  EDI, EAX //load the new MemoryBase
  MOV [EDI].TPreservedCodeMemory.States, DL
  LODSD
  ADD EAX, EDI
  JMP iVMEnter
end;

//CALLFAR PTurboModuleEntry cfa-addr
//if PTurboModuleEntry = nil means it's self, do not lookup.  
//�ڷ���ջ�б���EDI(�ɵ� FMemory ��ַ), 
//���� PTurboModuleEntry ����ģ���ڴ��ַ������ҵ�������EDI���µ� FMemory ��ַ,
//Ȼ��װ��ú����ĵ�ַ�������ͺ�VMEnterһ���ˣ�תȥVMEnter��
procedure iVMCallFar;
asm
  PUSH EDI //save the current MemoryBase.
  LODSD    //EAX= PTurboModuleEntry
  CMP  EAX, 0
  JZ  @@DoLocalEnterFar
  ADD  EAX, EDI //PTurboModuleEntry real addr
  MOV  ECX, [EAX].TTurboModuleEntry.Module
  CMP  ECX, 0
  JZ   @@RequireModuleExecutor
  MOV  EDI, [ECX].TTurboExecutorAccess.FMemory
  JMP  @@exit

@@RequireModuleExecutor: //find and load the module into the memory.
  PUSH EBX
  PUSH ESI
  PUSH EBP
  
  MOV  EDX, EAX
  ADD  EDX, Offset TTurboModuleEntry.ModuleName
  MOV  EAX, [EDI].TPreservedCodeMemory.Executor
  //function TCustomTruboExecutor.GetModuleMemoryAddr(aModuleIndex: Integer): Pointer;
  CALL TCustomTurboExecutor.RequireModule
  POP EBP
  POP ESI
  POP EBX

  CMP  EAX, 0
  JZ   @@NotFoundError

  //Copy CPU States to the New Module Memory.
  MOV EDI, [ESP] //restore the old Module MemoryBase in TOS
  MOV  CL, [EDI].TPreservedCodeMemory.States
  MOV  EDI, [EAX].TTurboExecutorAccess.FMemory
  MOV  [EDI].TPreservedCodeMemory.States, CL
  JMP @@Exit

@@NotFoundError:
  POP  EDI
  MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
  MOV  [EAX].TTurboGlobalOptions.LastErrorCode, errModuleNotFound
  JMP  iVMHalt

@@DoLocalEnterFar:

@@Exit:
  LODSD
  ADD EAX, EDI
  JMP iVMEnter
end;


//call(EXECUTE) the user defined word
//(CFA --- )
procedure iVMExecute;
asm
  //push the current IP.
  PUSH ESI        
  //set the new IP in the TOS
  ADD  EBX, EDI //EBX: TOS
  MOV  ESI, EBX   

  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP

  JMP  iVMNext
end;

//this is a Push Integer(����������) directive
procedure iVMPushInt;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  XCHG ESP, EBP
  PUSH EBX
  XCHG ESP, EBP
  {SUB  EBP, Type(tsPointer)
  MOV  [EBP], EDX
  }
 
  {
  MOV  EDX, [ESI]
  ADD  ESI, Type(tsInt)
  }
  LODSD
  MOV  EBX, EAX
  JMP  iVMNext
end;

//this is a Push Byte(����������) directive
procedure iVMPushByte;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  XCHG ESP, EBP
  PUSH EBX
  XCHG ESP, EBP
  {SUB  EBP, Type(tsPointer)
  MOV  [EBP], EDX
  }
 
  {
  MOV  EDX, [ESI]
  ADD  ESI, Type(tsInt)
  }
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
  XCHG ESP, EBP
  PUSH EBX
  XCHG ESP, EBP
  {SUB  EBP, Type(tsPointer)
  MOV  [EBP], EDX
  }
 
  {
  MOV  EDX, [ESI]
  ADD  ESI, Type(tsInt)
  }
  XOR EAX, EAX
  LODSW
  MOV  EBX, EAX
  JMP  iVMNext
end;

procedure iVMDropInt;
asm
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  {//move the top in stack to EAX 
  MOV  EDX, [EBP] 
  //Increment the data stack pointer.
  ADD  EBP, Type(tsInt)
  }
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
  ADD EBX, EDI //TOS <- TOS + FMem
  MOV EBX, [EBX]
  JMP  iVMNext
end;

procedure vFetchByte;
asm
  //EBX is TOS
  MOV EBX, EAX
  XOR EBX, EBX
  ADD EAX, EDI //EAX <- TOS + FMem
  MOV BL, [EAX]
  JMP  iVMNext
end;

procedure vFetchWord;
asm
  //EBX is TOS
  MOV EBX, EAX
  XOR EBX, EBX
  ADD EAX, EDI //EAX <- TOS + FMem
  MOV BX, [EAX]
  JMP  iVMNext
end;

procedure vFetchInt64;
asm
  //EBX is TOS
  ADD EBX, EDI //TOS <- TOS + FMem
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
  MOV EAX, [EDI].TPreservedCodeMemory.GlobalOptions
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
  MOV EAX, [EDI].TPreservedCodeMemory.GlobalOptions
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
  MOV EAX, [EDI].TPreservedCodeMemory.GlobalOptions
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
  MOV EAX, [EDI].TPreservedCodeMemory.GlobalOptions
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
procedure vEmitChar;
asm
  MOV  DL, BL  //DL <- TOS
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  MOV  EAX, [EDI].TPreservedCodeMemory.Executor

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
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  MOV  EAX, [EDI].TPreservedCodeMemory.Executor

  PUSH EDI
  PUSH EBX
  PUSH ESI
  PUSH EBP
  ADD  EDX, EDI
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
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  MOV  EAX, [EDI].TPreservedCodeMemory.Executor

  PUSH EDI
  PUSH EBX
  PUSH ESI
  PUSH EBP
  ADD  EDX, EDI
  MOV  ESI, [EAX]
  CALL DWORD PTR [ESI + VMTOFFSET TTurboX86Interpreter.DoPrintString]
  POP  EBP
  POP  ESI
  POP  EBX
  POP  EDI
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
  GTurboCoreWords[inNext] := iVMNext;
  GTurboCoreWords[inHalt] := iVMHalt;

  GTurboCoreWords[inEnter] := iVMEnter;
  GTurboCoreWords[inExit] := iVMExit;
  GTurboCoreWords[inCall] := iVMCall;

  GTurboCoreWords[inEnterFar] := iVMEnterFar;
  GTurboCoreWords[inExitFar] := iVMExitFar;
  
  GTurboCoreWords[inAddInt] := iVMAddInt;
  GTurboCoreWords[inSubInt] := iVMSubInt;
  GTurboCoreWords[inUMULInt] := iVMMulUnsignedInt;
  GTurboCoreWords[inMULInt] := iVMMulInt;
  GTurboCoreWords[inAddInt64] := iVMAddInt64;
  GTurboCoreWords[inSubInt64] := iVMSubInt64;

  //Memory Operation Instruction with Param Stack
  GTurboCoreWords[inFetchInt] := vFetchInt;
  GTurboCoreWords[inStoreInt] := vStoreInt;
  GTurboCoreWords[inFetchInt64] := vFetchInt64;
  GTurboCoreWords[inStoreInt64] := vStoreInt64;
  GTurboCoreWords[inFetchWord] := vFetchWord;
  GTurboCoreWords[inStoreWord] := vStoreWord;
  GTurboCoreWords[inFetchByte] := vFetchByte;
  GTurboCoreWords[inStoreByte] := vStoreByte;

  GTurboCoreWords[inPushInt] := iVMPushInt;
  GTurboCoreWords[inPushByte] := iVMPushByte;
  GTurboCoreWords[inPushWord] := iVMPushWord;
  GTurboCoreWords[inDropInt] := iVMDropInt;
  GTurboCoreWords[inPushInt64] := iVMPushInt64;
  GTurboCoreWords[inDropInt64] := iVMDropInt64;
  GTurboCoreWords[inEmit] := vEmitChar;
  GTurboCoreWords[inEmitString] := vEmitString;
  GTurboCoreWords[inEmitLString] := vEmitLString;
  GTurboCoreWords[inGetTickCount] := vGetTickCount;
  GTurboCoreWords[inStoreTickCount] := vStoreTickCount;

end;

initialization
  InitTurboCoreWordList;
end.
