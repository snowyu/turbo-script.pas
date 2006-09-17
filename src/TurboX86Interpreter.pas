{1 the fastest script
        基于x86指令优化。核心指令汇编实现，寄存器采用x86的寄存器.对应关系如下：
        }
{{
对应关系如下：
ESP,EBP: 返回堆栈： EAX 为返回堆栈栈顶，次栈顶。.
EDI（栈指针）: 数据栈，基址指针放在内存某个单元中。采用 STOS EAX,
所以EDI总是指向栈顶的下一个空单元。
ESI: 指向当前指令地址
EBX: 状态寄存器 (0Bit: 是否运行；1Bit:是否调试)
ECX: W Register 临时寄存器
EDX: 

当 EDI = 

可以用PUSHAD 将这些通用寄存器保存于堆栈，供调用其他系统的过程时采用。然后POPAD.
现在我的问题，这些核心过程是用方法实现还是函数过程实现？用无参数的过程实现。
规定：
EBP-4(SizeOf(Pointer)): 所指的是代码内存地址;
EBP-8(SizeOf(Pointer)*2): 则是数据栈基址.

PUSH EBP
MOV  EBP, ESP
PUSH FMemory
PUSH FParameterStack

采用什么形式 THREADING TECHNIQUE 来实现呢？基于查表字典的方式么！
那么我的函数表放在哪里好呢？全局变量的形式。
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
    //数据总是指向栈顶空
    STOS EAX //STOSD  the EDI will be decremented automatically.
    //自己判断是否数据栈为空。
  
  
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
