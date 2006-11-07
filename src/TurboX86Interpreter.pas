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
  MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
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
  MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
 
  MOV  [EAX].TTurboGlobalOptions.LastErrorCode, errBadInstruction
  JMP  iVMHalt
  //Bad OpCode: no procedure assigned to the OpCode.
{@@IsUserWord:
  ADD  EAX, EDI //指向用户定义的word入口
  
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
  ADD  EAX, EDI
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
end;

procedure iVMHalt;
asm
{  MOV DL, [EDI].TPreservedCodeMemory.States
  BTR EDX, psRunning //cTurboScriptIsRunningBit  //clear the cIsRunningBit to 0.
  MOV [EDI].TPreservedCodeMemory.States, DL
  JMP iVMNext
}
  MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
  CMP  ESP, [EAX].TTurboGlobalOptions.ReturnStackBottom
  MOV  DL, [EAX].TTurboGlobalOptions.States
  BTR  EDX, psRunning //cTurboScriptIsRunningBit  //clear the cIsRunningBit to 0.
  BTS  EDX, psHalt
  MOV  [EAX].TTurboGlobalOptions.States, DL
  JZ   @@byebye  //ok
@@HaltErr:
  MOV  [EAX].TTurboGlobalOptions.LastErrorCode, errHalt
  MOV  ECX, ESP
  MOV  ESP, [EAX].TTurboGlobalOptions.ReturnStackBottom
  MOV  [EAX].TTurboGlobalOptions.ReturnStackBottom, ECX
@@byebye:
end;

procedure iVMExit;
asm
  POP  ESI
  JMP  iVMNext
end;

procedure iVMExitFar;
asm
  //不能这样！这样的话如果是自己调用这个函数，退出的时候就会停止
  //另外，如果我正好这个时候发布停止，但是这里却重置了状态～～
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
  PUSH EDI //save the current MemoryBase.
  LODSD
  TEST EAX, EAX //CMP EAX, 0
  JZ @@LocalEnter 
  //MOV DL, [EDI].TPreservedCodeMemory.States
  MOV  EDI, EAX //load the new MemoryBase
  //MOV [EDI].TPreservedCodeMemory.States, DL

@@LocalEnter:
  //JMP iVMEnter
  LODSD
  ADD EAX, EDI
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
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
  TEST EAX, EAX //CMP EAX, 0
  JZ  @@DoLocalEnterFar
  ADD  EAX, EDI //PTurboModuleEntry real addr
  MOV  ECX, [EAX].TTurboModuleEntry.Module
  TEST ECX, ECX //CMP ECX, 0
  JZ   @@RequireModuleExecutor
  MOV  EDI, [ECX].TTurboExecutorAccess.FMemory
  JMP  @@exit

@@RequireModuleExecutor: //find and load the module into the memory.
  PUSH EAX  //keep the PTurboModuleEntry 
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
  POP ECX  //restore the PTurboModuleEntry

  TEST  EAX, EAX
  JZ   @@NotFoundError

  MOV [ECX].TTurboModuleEntry.Module, EAX
  //Copy CPU States to the New Module Memory.
  //MOV EDI, [ESP] //restore the old Module MemoryBase in TOS
  //MOV  CL, [EDI].TPreservedCodeMemory.States
  MOV  EDI, [EAX].TTurboExecutorAccess.FMemory
  //MOV  [EDI].TPreservedCodeMemory.States, CL
  JMP @@Exit

@@NotFoundError:
  POP  EDI
  MOV  EAX, [EDI].TPreservedCodeMemory.GlobalOptions
  MOV  [EAX].TTurboGlobalOptions.LastErrorCode, errModuleNotFound
  JMP  iVMHalt

@@DoLocalEnterFar:

@@Exit:
  //JMP iVMEnter
  LODSD
  ADD EAX, EDI
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext
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

  {XCHG ESP, EBP
  POP  EBX
  XCHG ESP, EBP}
  MOV EBX, [EBP]
  ADD EBP, Type(tsInt)

  JMP  iVMNext
end;

//this is a Push Integer(立即操作数) directive
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

//this is a Push Byte(立即操作数) directive
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

//this is a Push Word(立即操作数) directive
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
  GTurboCoreWords[inCallFar] := iVMCallFar;

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
