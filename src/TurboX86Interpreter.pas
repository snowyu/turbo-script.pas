{: the fastest script
        ����x86ָ���Ż�������ָ����ʵ�֣��Ĵ�������x86�ļĴ���.��Ӧ��ϵ���£�
        }
{ Description
��Ӧ��ϵ���£�
ESP,EBP: ���ض�ջ
EDI��ջָ�룩: ����ջ����ַָ������ڴ�ĳ����Ԫ�С�EAX Ϊ����ջջ����������
STOS EAX, ����EDI����ָ��ջ���ĵ�Ԫ��
ESI: ָ��ǰָ���ַ
EBX: ״̬�Ĵ��� (0Bit: �Ƿ����У�1Bit:�Ƿ����)
ECX: W Register ��ʱ�Ĵ���
EDX: ��ʱ�Ĵ�����

��Щ���Ĺ��������޲����Ĺ���ʵ�֡�
�涨������ջ����
  EBP+4: ��ָ����TTurboX86Interpreterʵ�����ڵ�ַ��
  EBP: ��ָ���Ǵ����ڴ��ַ;
//  EBP-4(SizeOf(Pointer)): ��������ջ��ַ.

PUSH EAX      '�������ʵ��ָ���ַ      
PUSH FMemory  '��������ڴ��ַָ��
MOV  EBP, ESP ' EBP ָ���Ǵ����ڴ��ַ now.
//PUSH FParameterStack

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
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    TIBLength: Integer; //the text buffer length
    ToIn: Integer; //the text buffer current index
    TIB: array [0..1023] of char;
    LastWordEntry: Pointer;
  end;
}
unit TurboInterpreter;

interface

{.$I Setting.inc}

uses
  SysUtils, Classes
  , uTurboScriptConsts
  , uTurboExecutor
  ;

const
  //For EBX Status Register.
  cIsRunningBit = 1;
  cIsSteppedBit = 2; 
  
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
    MOV  [EAX].FOldESP, ESP
    MOV  [EAX].FOldEBP, EBP
    MOV  Self.FOldESI, ESI
    MOV  Self.FOldEDI, EDI
    MOV  Self.FOldEBX, EBX
    MOV  ESP, Self.FRP //return stack pointer: RP
    //PUSHAD
    PUSH EAX
    PUSH [EAX].FMemory
    MOV  EBP, ESP
    PUSH [EAX].FParameterStack

    MOV  ESI, [EAX].FMemory
    ADD  ESI, aCFA
    MOV  EBX, cIsRunningBit
    MOV  EDI, [EAX].FSP //SP the data stack pointer.
    XOR  EAX, EAX
    //MOV  EDX, EAX
    //STD  //the EDI will be decremented.

    //PUSH @@ReturnAdr
    CALL  iVMNext
  @@ReturnAdr:
    //��������ָ��ջ��
    SUB EDI, Type(Integer)
    MOV [EDI], EAX
    //STOS EAX //STOSD  the EDI will be decremented automatically.
    //�Լ��ж��Ƿ�����ջΪ�ա�


    //MOV [ESI]
    POP EAX
    POP EAX
    POP EAX
    MOV  [EAX].TTurboX86Interpreter.FRP, ESP
    MOV  [EAX].TTurboX86Interpreter.FSP, EDI
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
  MOV  ESI, ECX   //set the new IP
  JMP iVMNext
end;

//the interpreter core here:
procedure iVMNext;
asm
  TEST EBX, cIsRunningBit
  JZ @@Exit

  MOV ECX, [ESI]  //the current instruction in W register
  ADD ESI, Type(Pointer) //4 = INC PC INC PC INC PC INC PC
  
@@ExecInstruction:
  CMP  ECX, cMaxTurboVMDirectiveCount
  LEA  EDX, GTurboCoreWords
  JAE   @@IsUserWord
@@IsVMCode:
  MOV  ECX, [EDX+ECX*4]
  JMP  ECX
@@IsUserWord:
  ADD  ECX, [EBP] //ָ���û������word���
  JMP  iVMEnter
@@Exit:
end;

procedure iVMHalt;
asm
  BTR EBX, cIsRunningBit  //clear the cIsRunningBit to 0.
  JMP iVMNext
end;

procedure iVMExit;
asm
  POP  ESI
  JMP  iVMNext
end;

//this is a Push Integer(����������) directive
procedure iVMPushInt;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  SUB  EDI, Type(Pointer)
  MOV  [EDI], EAX
  
  MOV  EAX, [ESI]
  ADD  ESI, Type(Integer)
  JMP  iVMNext
end;

procedure iVMPopInt;
asm
  //move the top in stack to EAX 
  MOV  EAX, [EDI] 
  //Increment the data stack pointer.
  ADD  EDI, Type(Integer)
  
  JMP  iVMNext
end;

//(n, n1) -- (n = n + n1)
procedure iVMAddInt;
asm
  ADD EAX, [EDI]
  ADD EDI, Type(Integer)
end;

//(n, n1) -- (n = n - n1)
procedure iVMSubInt;
asm
  SUB EAX, [EDI]
  ADD EDI, Type(Integer)
end;

// Unsigned multiply
//(n, n1) -- (DoubleWord = n * n1)
//EAX(n): the result of Low orders; n1 the result of high orders
procedure iVMMulUnsignedInt;
asm
  MUL  EAX, [EDI] //EDX:EAX = EAX * [EDI]
  MOV  [EDI], EDX
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
