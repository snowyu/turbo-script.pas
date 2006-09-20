{: the fastest script
        基于x86指令优化。核心指令汇编实现，寄存器采用x86的寄存器.对应关系如下：
        }
{ Description
对应关系如下：
ESP: 返回堆栈指针.记住压入减少，弹出增加地址。
EBP: 数据栈指针，基址指针放在内存某个单元中。所以EBP总是指向次栈顶。
EDX: 为数据栈栈顶。 
ESI: 指向当前指令地址
EBX: 状态寄存器 (0Bit: 是否运行；1Bit:是否调试) TTurboForthStates = set of
TTurboForthState; TTurboForthState = (tfsRunning, tfsDebugging, tfsCompiling)
EAX: W Register 临时寄存器
ECX: 临时寄存器
EDI: FMemory基址

这些核心过程是用无参数的过程实现。

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
    Executor: TCustomTurboExecutor;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    TIBLength: Integer; //the text buffer length
    ToIn: Integer; //the text buffer current index
    TIB: array [0..cMAXTIBCount-1] of char;
    LastWordEntry: Pointer; //有名字的函数链表
     LastVarEntry: Pointer; //有名字的变量链表
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
    //数据总是指向栈顶
    XCHG ESP, EBP
    PUSH EDX
    XCHG ESP, EBP
    //自己判断是否数据栈为空。


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
  ADD  EAX, EDI //指向用户定义的word入口
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

//this is a Push Integer(立即操作数) directive
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
