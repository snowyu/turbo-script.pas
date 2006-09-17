{1 the fastest script
        基于x86指令优化。核心指令汇编实现，寄存器采用x86的寄存器.对应关系如下：
        }
{{
对应关系如下：
ESP,EBP: 返回堆栈
EDI（栈指针）: 数据栈，基址指针放在内存某个单元中。EAX 为数据栈栈顶。不采用
STOS EAX, 所以EDI总是指向栈顶的单元。
ESI: 指向当前指令地址
EBX: 状态寄存器 (0Bit: 是否运行；1Bit:是否调试)
ECX: W Register 临时寄存器
EDX: 临时寄存器。

这些核心过程是用无参数的过程实现。
规定（返回栈）：
  EBP+4: 所指的是TTurboX86Interpreter实例所在地址。
  EBP: 所指的是代码内存地址;
//  EBP-4(SizeOf(Pointer)): 则是数据栈基址.

PUSH EAX      '保存对象实例指针地址      
PUSH FMemory  '保存代码内存地址指针
MOV  EBP, ESP ' EBP 指的是代码内存地址 now.
//PUSH FParameterStack

采用什么形式 THREADING TECHNIQUE 来实现呢？核心虚拟指令采用查表字典的方式！
用户自定义指令采用相对地址（由于我占用了代码区前面的至少1024个字节，所以地址不可能小于255）表示。
那么我的核心虚拟指令表放在哪里好呢？全局变量的形式。

代码区内存镜像：
FParameterStackBase(Pointer:是数据栈基址) FParameterStackSize(Integer:
是数据栈大小)
ReturnStackBase(Pointer: 返回栈基址) ReturnStackSize(Integer: 返回栈大小)
TIBLength(Integer) ToIn(Integer) TIB(PChar: 1024) LastWordEntry(Pointer:
用户自定义单词链入口)
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
  {: 核心虚拟指令表 }
  TTurboCoreWords = array [0..cMaxTurboVMDirectiveCount-1] of TProcedure;

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
procedure iVMNext;forward;

procedure iVMEnter;
asm
  PUSH ESI        //push the current IP.
  MOV  ESI, ECX   //set the new IP
  JMP iVMNext
end;

procedure iVMNext;
asm
  TEST EBX, cIsRunningBit
  JZ @@Exit

  MOV ECX, [ESI]  //the current instruction in W register
  ADD ESI, Type(Pointer) //4 = INC PC INC PC INC PC INC PC
  
@@ExecInstruction:
  CMP  ECX, cMaxTurboVMDirectiveCount
  MOV  EDX, PTR GTurboCoreWords
  JAE   @@IsUserWord
@@IsVMCode:
  MOV  ECX, [EDX+ECX]
  JMP  [ECX]
@@IsUserWord:
  ADD  ECX, [EBP] //指向用户定义的word入口
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

//this is a Push Integer directive
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
end;

initialization

end.
