{1 the fastest script
        ����x86ָ���Ż�������ָ����ʵ�֣��Ĵ�������x86�ļĴ���.��Ӧ��ϵ���£�
        }
{{
��Ӧ��ϵ���£�
ESP,EBP: ���ض�ջ�� EAX Ϊ���ض�ջջ������ջ����.
EDI��ջָ�룩: ����ջ����ַָ������ڴ�ĳ����Ԫ�С����� STOS EAX,
����EDI����ָ��ջ������һ���յ�Ԫ��
ESI: ָ��ǰָ���ַ
EBX: ״̬�Ĵ��� (0Bit: �Ƿ����У�1Bit:�Ƿ����)
ECX: W Register ��ʱ�Ĵ���
EDX: 

�� EDI = 

������PUSHAD ����Щͨ�üĴ��������ڶ�ջ������������ϵͳ�Ĺ���ʱ���á�Ȼ��POPAD.
�����ҵ����⣬��Щ���Ĺ������÷���ʵ�ֻ��Ǻ�������ʵ�֣����޲����Ĺ���ʵ�֡�
�涨��
EBP-4(SizeOf(Pointer)): ��ָ���Ǵ����ڴ��ַ;
EBP-8(SizeOf(Pointer)*2): ��������ջ��ַ.

PUSH EBP
MOV  EBP, ESP
PUSH FMemory
PUSH FParameterStack

����ʲô��ʽ THREADING TECHNIQUE ��ʵ���أ����ڲ���ֵ�ķ�ʽô��
��ô�ҵĺ��������������أ�ȫ�ֱ�������ʽ��
}
unit TurboInterpreter;

interface

{$I Setting.inc}

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
  TTurboCoreWords = array [Byte] of TProcedure;

  TTurboX86Interpreter = class(TCustomTurboExecutor)
  private
    FOldEBP: Integer;
    FOldEBX: Integer;
    FOldEDI: Integer;
    FOldEDX: Integer;
    FOldESI: Integer;
    FOldESP: Integer;
  protected
    function ExecuteCFA(const aCFA: Integer): Integer; override;
    procedure InitExecution; override;
  public
    destructor Destroy; override;
  end;
  

var
  GTurboCoreWords: TTurboCoreWords;

implementation


{
***************************** TTurboX86Interpreter *****************************
}
destructor TTurboX86Interpreter.Destroy;
begin
  inherited Destroy;
end;

function TTurboX86Interpreter.ExecuteCFA(const aCFA: Integer): Integer;
  
  {$ifdef NoSuch_Def}
  
begin
  Result := inherited ExecuteCFA(aCFA);
  {$else}
  asm
  {$endif}
    MOV  FOldESP, ESP
    MOV  FOldEBP, EBP
    MOV  FOldESI, ESI
    MOV  FOldEDI, EDI
    MOV  FOldEBX, EBX
    MOV  ESP, FRP //return stack pointer: RP
    //PUSHAD
    PUSH EAX
    PUSH FMemory
    MOV  EBP, ESP
    PUSH FParameterStack
  
    MOV  ESI, FMemory
    ADD  ESI, aCFA
    MOV  EBX, cIsRunningBit
    MOV  EDI, FSP //SP the data stack pointer.
    XOR  EAX, EAX
    //MOV  EDX, EAX
    STD  //the EDI will be decremented.
  
    //PUSH @@ReturnAdr
    CALL  iVMNext
  @@ReturnAdr:
    //��������ָ��ջ����
    STOS EAX //STOSD  the EDI will be decremented automatically.
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
begin
  
end;

procedure iVMNext;
asm
  MOV ECX, [ESI]
  ADD ESI, Type(Pointer)
  
end;

procedure iVMHalt;
begin
end;

procedure iVMExit;
begin
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
  
  //Memory Operation Instruction with Param Stack
  GTurboCoreWords[inFetchInt] := vFetch;
  GTurboCoreWords[inStoreInt] := vStore;
  GTurboCoreWords[inFetchByte] := vCFetch;
  GTurboCoreWords[inStoreByte] := vCStore;
end;

initialization

end.
