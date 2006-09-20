{: the fastest script
        ����x86ָ���Ż�������ָ����ʵ�֣��Ĵ�������x86�ļĴ���.��Ӧ��ϵ���£�
        }
{ Description
��Ӧ��ϵ���£�
ESP: ���ض�ջָ��.��סѹ����٣��������ӵ�ַ��
EBP: ����ջָ�룬��ַָ������ڴ�ĳ����Ԫ�С�����EBP����ָ���ջ����
EDX: Ϊ����ջջ���� 
ESI: ָ��ǰָ���ַ
EBX: ״̬�Ĵ��� (0Bit: �Ƿ����У�1Bit:�Ƿ����) TTurboForthStates = set of
TTurboForthState; TTurboForthState = (tfsRunning, tfsDebugging, tfsCompiling)
EAX: W Register ��ʱ�Ĵ���
ECX: ��ʱ�Ĵ���
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
  SysUtils, Classes
  , uTurboScriptConsts
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
  public
    destructor Destroy; override;
    function ExecuteCFA(const aCFA: Integer): Integer; override;
    procedure InitExecution; override;
  end;


var
  GTurboCoreWords: TTurboCoreWords;

implementation

procedure iVMNext;forward;

{
***************************** TTurboX86Interpreter *****************************
}
destructor TTurboX86Interpreter.Destroy;
begin
  inherited Destroy;
end;

function TTurboX86Interpreter.ExecuteCFA(const aCFA: Integer): Integer;

  {$ifdef NoSuchDef}

begin
  {$else}
  asm
  {$endif}
    PUSH EAX
    PUSH EDX
    CALL TCustomTurboExecutor.ExecuteCFA
    POP  EDX
    POP  EAX

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
    MOV  EBX, cTurboScriptIsRunningBit //EBX: FORTH Processor States
    MOV  EBP, [EAX].FSP //SP the data stack pointer.
    XOR  EDX, EDX //the TOS
    //MOV  EDX, EAX
    //STD  //the EDI will be decremented.
    CLD //the esi will be incremented.
    //PUSH @@ReturnAdr
    CALL  iVMNext
  @@ReturnAdr:
    //��������ָ��ջ��
    XCHG ESP, EBP
    PUSH EDX
    XCHG ESP, EBP
    //�Լ��ж��Ƿ�����ջΪ�ա�


    //MOV [ESI]
    //POP EAX
    //POP EAX
    //POP EAX
    MOV  EAX, [EDI].TPreservedCodeMemory.Executor
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

procedure TTurboX86Interpreter.InitExecution;
begin
  inherited InitExecution;
  //
end;


{----Helper functions ----}

procedure iVMEnter;
asm
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
end;

//the interpreter core here:
procedure iVMNext;
asm
  TEST EBX, cTurboScriptIsRunningBit
  JZ @@Exit

  //MOV EAX, [ESI]  //the current instruction in W register
  //ADD ESI, Type(Pointer) //4 = INC PC INC PC INC PC INC PC
  LODSD
  
@@ExecInstruction:
  CMP  EAX, cMaxTurboVMInstructionCount
  LEA  ECX, GTurboCoreWords
  JAE   @@IsUserWord
@@IsVMCode:
  MOV  EAX, [ECX+EAX*4]
  JMP  EAX
@@IsUserWord:
  ADD  EAX, EDI //ָ���û������word���
  JMP  iVMEnter
@@Exit:
end;

procedure iVMHalt;
asm
  BTR EBX, cTurboScriptIsRunningBit  //clear the cIsRunningBit to 0.
  JMP iVMNext
end;

procedure iVMExit;
asm
  POP  ESI
  JMP  iVMNext
end;

//call(EXECUTE) the user defined word
//(CFA --- )
procedure iVMExecute;
asm
  PUSH ESI        //push the current IP.
  MOV  ESI, EDI   //set the new IP in the TOS

  XCHG ESP, EBP
  POP  EDX
  XCHG ESP, EBP

  JMP  iVMNext
end;

//this is a Push Integer(����������) directive
procedure iVMPushInt;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  XCHG ESP, EBP
  PUSH EDX
  XCHG ESP, EBP
  {SUB  EBP, Type(Pointer)
  MOV  [EBP], EDX
  }
 
  {
  MOV  EDX, [ESI]
  ADD  ESI, Type(Integer)
  }
  LODSD
  MOV  EDX, EAX
  JMP  iVMNext
end;

procedure iVMPopInt;
asm
  XCHG ESP, EBP
  POP  EDX
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
  ADD EDX, [EBP]
  ADD EBP, Type(Integer)
end;

//(n, n1) -- (n = n - n1)
procedure iVMSubInt;
asm
  SUB EDX, [EBP]
  ADD EBP, Type(Integer)
end;

// Unsigned multiply
//(n, n1) -- (DoubleWord = n * n1)
//EAX(n): the result of Low orders; n1 the result of high orders
procedure iVMMulUnsignedInt;
asm
  MOV  EAX, EDX
  MUL  EAX, [EBP] //EDX:EAX = EAX * [EDI]
  MOV  [EBP], EDX
  MOV  EDX, EAX
end;

procedure vFetch;
begin
end;

procedure vStore;
begin
end;

procedure vCFetch;
begin
end;

procedure vCStore;
begin
end;

procedure InitTurboCoreWordList;
begin
  GTurboCoreWords[inEnter] := iVMEnter;
  GTurboCoreWords[inNext] := iVMNext;
  GTurboCoreWords[inHalt] := iVMHalt;
  GTurboCoreWords[inExit] := iVMExit;
  
  GTurboCoreWords[inAddInt] := iVMAddInt;
  GTurboCoreWords[inSubInt] := iVMSubInt;
  GTurboCoreWords[inMULUnsignedInt] := iVMMulUnsignedInt;

  //Memory Operation Instruction with Param Stack
  GTurboCoreWords[inFetchInt] := vFetch;
  GTurboCoreWords[inStoreInt] := vStore;
  GTurboCoreWords[inFetchByte] := vCFetch;
  GTurboCoreWords[inStoreByte] := vCStore;

  GTurboCoreWords[inPushInt] := iVMPushInt;
  GTurboCoreWords[inPopInt] := iVMPopInt;

end;

initialization
  InitTurboCoreWordList;
end.
