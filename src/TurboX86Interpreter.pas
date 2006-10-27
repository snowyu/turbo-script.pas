{: the fastest script
        基于x86指令优化。核心指令汇编实现，寄存器采用x86的寄存器.对应关系如下：
        }
{ Description
对应关系如下：
ESP: 返回堆栈指针.记住压入减少，弹出增加地址。
EBP: 数据栈指针，基址指针放在内存某个单元中。所以EBP总是指向次栈顶。
EBX: 为数据栈栈顶。 
ESI: 指向当前指令地址
EDX: 状态寄存器(
仅当条件编译指令：TurboScript_FullSpeed开启时使用，否则为临时寄存器)
TTurboForthProcessorStates, 为了能控制停止,状态寄存器放于保留内存中了。
EAX: W Register 临时寄存器
ECX:  临时寄存器

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
    CMP  EBP, [EDI].TPreservedCodeMemory.ParamStackBottom
    JE   @@skipStoreTOS
    //数据总是指向栈顶
    //数据栈中，栈底的数据是无意义的。
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
  MOV  [EDI].TPreservedCodeMemory.ReturnStackBottom, ESP
  MOV  [EDI].TPreservedCodeMemory.ParamStackBottom, EBP
  JMP  iVMNext
end;

//the interpreter core here:
procedure iVMNext;
asm
  MOV  DL, [EDI].TPreservedCodeMemory.States

  //TODO: BT is a 486 directive.
  BT EDX, psRunning //cTurboScriptIsRunningBit
  JNC @@Exit

  //MOV EAX, [ESI]  //the current instruction in W register
  //ADD ESI, Type(Pointer) //4 = INC PC INC PC INC PC INC PC
  LODSD
  
@@ExecInstruction:
  CMP  EAX, cMaxTurboVMInstructionCount
  LEA  ECX, GTurboCoreWords
  JAE   @@IsUserWord
@@IsVMCode:
  MOV  EAX, [ECX+EAX*4]
  CMP  EAX, 0
  JZ   @@BadOpError
  JMP  EAX
@@BadOpError:
  MOV  [EDI].TPreservedCodeMemory.LastErrorCode, errBadInstruction
  JMP  iVMHalt
  //Bad OpCode: no procedure assigned to the OpCode.
@@IsUserWord:
  ADD  EAX, EDI //指向用户定义的word入口
  JMP  iVMEnter
@@Exit:
  CMP  ESP, [EDI].TPreservedCodeMemory.ReturnStackBottom
  JZ   @@byebye  //ok
@@HaltErr:
  BTS  EDX, errHalt
  MOV  EAX, ESP
  MOV  ESP, [EDI].TPreservedCodeMemory.ReturnStackBottom
  MOV  [EDI].TPreservedCodeMemory.ReturnStackBottom, EAX
@@byebye:

end;

procedure iVMEnter;
asm
  PUSH ESI        //push the current IP.
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
  MOV DL, [EDI].TPreservedCodeMemory.States
  BTR EDX, psRunning //cTurboScriptIsRunningBit  //clear the cIsRunningBit to 0.
  MOV [EDI].TPreservedCodeMemory.States, DL
  POP  ESI
  POP  EDI
  JMP  iVMNext
end;

procedure iVMEnterFar;
asm
  PUSH EDI //save the current MemoryBase.
  LODSD
  MOV  EDI, EAX //load the new MemoryBase
  LODSD
  ADD EAX, EDI
  JMP iVMEnter
end;

//CALLFAR PTurboModuleEntry cfa-addr
//if PTurboModuleEntry = nil means it's self, do not lookup.  
//在返回栈中保存EDI(旧的 FMemory 基址), 
//根据 PTurboModuleEntry 查找模块内存基址，如果找到就设置EDI成新的 FMemory 基址,
//然后装入该函数的地址，其它就和VMEnter一样了，转去VMEnter。
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
  MOV  [EDI].TPreservedCodeMemory.LastErrorCode, errModuleNotFound
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

//this is a Push Integer(立即操作数) directive
procedure iVMPushInt;
asm
  //Decrement the data stack pointer.
  //push the second data to the data stack.
  XCHG ESP, EBP
  PUSH EBX
  XCHG ESP, EBP
  {SUB  EBP, Type(Pointer)
  MOV  [EBP], EDX
  }
 
  {
  MOV  EDX, [ESI]
  ADD  ESI, Type(Integer)
  }
  LODSD
  MOV  EBX, EAX
  JMP  iVMNext
end;

procedure iVMPopInt;
asm
  XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP
  {//move the top in stack to EAX 
  MOV  EDX, [EBP] 
  //Increment the data stack pointer.
  ADD  EBP, Type(Integer)
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

procedure iVMPopInt64;
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
  ADD EBP, Type(Integer)
  JMP  iVMNext
end;

//(n, n1) -- (n = n - n1)
procedure iVMSubInt;
asm
  SUB EBX, [EBP]
  ADD EBP, Type(Integer)
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
  ADD  EBP, TYPE(Integer)
  JMP  iVMNext
end;

procedure vFetchInt;
asm
  //EBX is TOS
  ADD EBX, EDI //TOS <- TOS + FMem
  MOV EBX, [EBX]
  JMP  iVMNext
end;

procedure vStore;
begin
end;

procedure vCFetchInt;
asm
  //EBX is TOS
  ADD EBX, EDI //TOS <- TOS + FMem
  MOV BL, [EBX]
  JMP  iVMNext
end;

procedure vCStore;
begin
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

procedure InitTurboCoreWordList;
begin
  GTurboCoreWords[inNext] := iVMNext;
  GTurboCoreWords[inHalt] := iVMHalt;

  GTurboCoreWords[inEnter] := iVMEnter;
  GTurboCoreWords[inExit] := iVMExit;

  GTurboCoreWords[inEnterFar] := iVMEnterFar;
  GTurboCoreWords[inExitFar] := iVMExitFar;
  
  GTurboCoreWords[inAddInt] := iVMAddInt;
  GTurboCoreWords[inSubInt] := iVMSubInt;
  GTurboCoreWords[inUMULInt] := iVMMulUnsignedInt;
  GTurboCoreWords[inMULInt] := iVMMulInt;

  //Memory Operation Instruction with Param Stack
  GTurboCoreWords[inFetchInt] := vFetchInt;
  GTurboCoreWords[inStoreInt] := vStore;
  GTurboCoreWords[inFetchByte] := vCFetchInt;
  GTurboCoreWords[inStoreByte] := vCStore;

  GTurboCoreWords[inPushInt] := iVMPushInt;
  GTurboCoreWords[inPopInt] := iVMPopInt;
  GTurboCoreWords[inPushQWord] := iVMPushInt64;
  GTurboCoreWords[inPopQWord] := iVMPopInt64;
  GTurboCoreWords[inEmit] := vEmitChar;
  GTurboCoreWords[inEmitString] := vEmitString;
  GTurboCoreWords[inEmitLString] := vEmitLString;

end;

initialization
  InitTurboCoreWordList;
end.
