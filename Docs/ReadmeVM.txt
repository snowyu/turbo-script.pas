我的虚拟机(Virtual Machine)说明：

我将它命名为： TurboScriptEngine. 

设计目标：
  高速（既能够解释执行，也能够编译成机器码直接执行），小巧，高效；
  数据库执行功能：the run-time script types, constants, global variables and procedures can be stored in the memory or database.

首先由编译模块将程序全部翻译成Forth虚拟机器码,再由执行器将虚拟机器码翻译成不同平台的执行码并执行或者由执行器解释执行.

两大模块：编译器模块，执行器模块

编译器模块
抽象层: uTurboCompiler.pas(CustomTurboCompiler, CustomTurboScriptModule)
使用层: TurboForthCompiler.pas, TurboPascalCompiler.pas, TurboBasicCompiler.pas, TurboCCompiler.pas, etc
扩展层: 

执行器模块
抽象层: uTurboExecutor.pas(include abstract PEFormat, executor , debugger and TurboProgram classes)
使用层: TurboX86Executor.pas; TurboZ80Executor.pas, TurboJavaVMExecutor.pas
扩展层: 如, TurboInterpreter.pas; TurboDebugger.pas;  

执行器中只包括Codes, ImportModules(自己提供给脚本使用的以及通过LoadLibrary装入的), Resource, 其它信息(ImportTable)只在PEFormat中存在。

TurboInterpreter_S: Pure Pascal 实现，暂缓
TurboInterpreter: 基于x86指令优化。核心指令汇编实现，寄存器采用x86的寄存器，对应关系如下：
ESP,EBP: 返回堆栈.记住压入减少，弹出增加地址。
EDI（栈指针）: 数据栈，基址指针放在内存某个单元中。EAX 为数据栈栈顶。不采用 STOS EAX, 所以EDI总是指向栈顶。
ESI: 指向当前指令地址
EBX: 状态寄存器 (0Bit: 是否运行；1Bit:是否调试)
ECX: W Register 临时寄存器
EDX: 临时寄存器

可以用PUSHAD 将这些通用寄存器保存于堆栈，供调用其他系统的过程时采用。然后POPAD.
现在我的问题，这些核心过程是用方法实现还是函数过程实现？用无参数的过程实现。
规定：
EBP+4: 所指的是TTurboX86Interpreter实例所在地址。
EBP: 所指的是代码内存地址;
//EBP-4(SizeOf(Pointer)): 则是数据栈基址.

PUSH EAX      
PUSH FMemory 
MOV  EBP, ESP
//PUSH FParameterStack

采用什么形式 THREADING TECHNIQUE 来实现呢？基于核心虚拟指令采用查表字典的方式！用户自定义Word采用相对地址（由于我占用了代码区前面的至少1024个字节，所以地址不可能小于255）表示。
那么我的函数表放在哪里好呢？全局变量的形式。

用户自定义Word 实际上 THREADING TECHNIQUE 类似于DTC(Direct Threaded Code) 模式，不过我有办法区分是否是VM机器指令，还是相对地址。
最核心的，最重要的Forth指令，我作为VM机器指令实现了：Next, Enter, Exit

用户自定义word:序列： 没有Enter了！只有Exit.

vmNext
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
  JMP  vmEnter
@@Exit:

干脆不用CALL 全部 jmp 比较好！然后每一个最后都有一个JMP vmNext

vmEnter: push the current IP(ESI),set the new IP, and run the vmNext
  PUSH ESI        //push the current IP.
  MOV  ESI, ECX   //set the new IP
  JMP .vmNext


Exit: pop to the IP(ESI),and run the vmNext.
  POP  ESI
  JMP  vmNext

vmHalt
  BTR EBX, cIsRunningBit  //clear the cIsRunningBit to 0.
  JMP vmNext

TCoreForthWords = array [Byte] of TProcedure;

代码区内存镜像：
FParameterStackBase(Pointer:是数据栈基址) FParameterStackSize(Integer:是数据栈大小)  
ReturnStackBase(Pointer: 返回栈基址) ReturnStackSize(Integer: 返回栈大小)
TIBLength(Integer) ToIn(Integer) TIB(PChar: 1024) LastWordEntry(Pointer: 用户自定义单词链入口)
type //in TurboScriptConsts
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

如何区分指令和入口地址（为 CALL 入口地址）？
方式1：指令长度与地址指针的长度一样，由于我占用了代码区前面的至少1024个字节，所以地址不可能小于255，因此<256的为虚拟指令，否则为入口地址。
方式2：指令长度为1字节，专门增加一个指令CALL,后面紧跟用户定义单词的入口地址。

采用方式1，理由：
  1、节约内存，系统的扩建主要是在用户自定义单词上
  2、便于扩充系统指令，方式2最多只能有256个指令，而且无法扩充。

文件支持层: 
  uTurboScriptAccessor(模块装入保存机制); 
  实际的装载、卸载发生在这里，管理从文件或数据库加载模块，模块的唯一性。
  uTurboPEFormat.pas(俺的格式); uTurboWin32PEFormat(windows32的PE格式);
编译后的可执行文件格式, PE: Portable Executable File Format 
功能：根据格式，装入Import表中的模块(DLL或ForthDLL模块)，重定位地址，以及管理RES资源。以及需要重新计算的绝对地址，relocation表。
1、Import表(if any)
2、Export表(if any)
3、Relocation表(if any)：分为代码重定位表和数据重定位表。
4、Resource表(if any)
5、Code段表(if any)
7、symbol符号表(if any)用于调试

注意：根据俺正在学习的PE格式，也许应该将其组织成Section，Section的用涂由其属性决定。具体格式参见ExeCompile.txt。

这个Section应该称之为模块吧：TTurboScriptModule, 模块只应该在编译期存在。Section应该是比较简单的格式，这样运行效率才高。

ForthDLL：与DLL类似，不过里面的代码不是机器码，而是Forth的VM码。


类的说明:

{ Summary the module for the TurboScript.}
TCustomTurboScriptModule = Class
protected
pubic
end;

{ Summary The Abstract Portable Executable File Format Helper Class }
{ Description 
  Load the Executable File from the stream/file.
  See Also GTurboPEFormatFactory
}
TCustomTurboPEFormat = Class

end;

{ Summary : The Abstract Virtual Machine Processor }
{ Description :
  Chinese 
    抽象虚拟机处理器抽象类
    拥有一个IR(instruction register)指令寄存器, 一个SP堆栈指针寄存器, 一个BP堆栈基址指针寄存器, 一个PC程序指针寄存器，一个状态寄存器.
    指令存储器部分：在这里存放等待执行的指令序列，采用单向链表的形式。
    可以执行Execute和停止(Stop)。
  TODO
    是否需要加入调试功能？
}
TCustomVMProcessor = Class
end;

{ Summary : the Abstract stack-oriented Virtual Machine processor }
{ Description :
  Chinese 
    基于堆栈虚拟机处理器抽象类
}
TStackVMProcessor = Class(TCustomVMProcessor)
end;

{ Summary : the Forth Virtual Machine processor }
{ Description :
  Chinese 
    基于堆栈的Forth虚拟机处理器
}
TForthVMProcessor = Class(TStackVMProcessor)

{ Summary 抽象的编译器for Forth Language}
TCustomCompiler = Class

{ Summary 抽象的脚本执行器 for Forth Language}
{ Description
  1、加载，根据文件头映像格式，加载模块和重定位地址。
  2、翻译，翻译成本地机器码
  3、执行脚本。
}
  TCustomScriptExecutor = Class
  private
  // 代码区 CodeArea : 存放运行代码。
    FCodeArea: array of byte;
  // 数据区 DataArea : 存放全局变量。
    FDataArea: array of byte;
  // 返回栈： 运行前，保存栈指针信息，然后将系统栈指针指向该FReturnStack。 ESP := @FReturnStack[0] + Length(FReturnStack) - 1; EBP := ESP;
  // 
    FReturnStack: array of byte;
  // 数据栈
    FDataStack: array of byte;
  // 主程序的函数地址
    FFunctions: array of TFunctionRec;
    FVariables: array of TVariableRec;

  protected
    function InternalExecute: Integer;virtual;

  public
  //注册主程序的函数变量到执行器中
    procedure AddFunction(AName:string; aProc:Pointer);
    procedure AddVariable(AName:string; aVar:Pointer);

    function Execute: Integer;

    function LoadFromStream(const ResolveAddressReq: Boolean=True): Integer;virutal;
    function LoadFromFile(const ResolveAddressReq: Boolean=True): Integer;
  end;

TFunctionProc = Function():Integer;

{ Summary Intel x86 CPU 的执行器}
  TIntelx86Executor = Class(TCustomScriptExecutor)
  private
    // 编译后才能调用
    FDoExecute: TFunctionProc;


  end;

数据说明:
虚拟机处理的数据区:
 1. 程序数据区（指令存储器）
   首先，我要将指令按长度分为：
     1.简单指令: 没有任何参数的指令
     2.单参数指令: 带一个参数的指令

   TVMCode = record
     opCode      : TInstruction;
     vInteger    : integer;         // the first integer parameter
     vInteger1   : integer;         // the second integer parameter
     Next        : pSimplifiedCode; // Next instruction pointer
     ExecCode    : TGenericProc;    // instruction method
     vString     : string;          // the string parameter: element name
     vDebugInfo  : integer;         // the debug information: source code position
     // Extra parameters
     case byte of
        0: ( vDouble:double );               // 1 double parameter
        1: ( vProc:TGenericProc );           // 1 method parameter
        2: ( vInteger2,vInteger3:integer );  // 2 additional integer parameters
   end;
   TProgramDataArea = array of TVMCode
   
分析一个简单的指令流程： “2 3 +”，编译后生成如下的指令流：
  opPush 2, opPush 3, opAdd
使用 fixed length VMCode:


内部(Internal)函数总览
也就是内部关键字（过程）。ID为LongWord（$1-$FFFFFFFF）,可以分为系统关键字过程,ID从1-$FFFF和用户自定义关键字过程，ID($10000-$FFFFFFFF).
如果是在内部解释执行，反正都需要解释，那么就采用查表法(TTC)；只有当需要脱离的时候才换成执行地址(DTC or STC)。

TTC(查表)的表： 对于系统关键字，可以在细分下($1-$1FF)为核心系统关键字，采用数组的形式直接存放执行入口地址；
而对于其他的关键字则采用散列等其他形式来节约内存。嘿嘿，我有了更好的主意，直接采用地址。
对于内部关键过程全部采用数组形式，新加的添加在最后（当然这样的后果就是用户自定义的函数可能不会唯一，和别的用户定义相冲突）。


系统关键字过程:
  四则运算过程：
  逻辑运算过程：
  字符串处理：
  流程控制： if...else, while, for...next, repeat...until.


 2. 返回堆栈区
 3. 变量数据区
 4. 内部程序区(Procs): 就是那些固定的机器指令代码/

FORTH虚拟机处理的数据区除了上述的数据区外:
 4. 数据堆栈区

TTC(查表方式下)寄存器的使用：

TurboScript 机器指令(Forth)汇编：
基本指令以字节作为长度。换句话说，机器指令最多255个。在解释器中，VM机器指令是以查表的方式解释执行。在翻译器中将这些VM指令翻译成真正的机器语言然后在执行器（如：X86Executor）中执行。
指令可分单字节指令和多字节指令（指令＋操作数）

  { Summary the FORTH Virtual Mache Codes}
  TVMInstruction = (
    inNone,
    {## The FORTH CORE instructions }
    inHalt,
    inEnter,
    inExit,
    inNext,
    
    {## Memory Operation Instruction }
    inStoreInt, 
    inStoreByte, //CStore
    inFetchInt,
    inFetchByte, //CFetch

    {## Arithmatic instructions }
    {## for Integer}
    inAddInt, //Add
    inSubInt, //subtract
    inIncInt, //add 1
    inDecInt, //subtract 1
    inMULInt, //multiply 
    inDIVInt, //divide
    inIncNInt, //add N
    inDecNInt, //subtract N

    {## Logical instructions }
    {## for Integer}
    inEQUInt,
    inNEQInt, // not equ
    inLESInt, //less than
    inLEQInt, //less than and equ
    inGERInt, //greater than
    inGEQInt, //greater than and equ
    inNOTInt, //Negate(NOT)
    inANDInt,
    inORInt,
    inXORInt,

    {## Proc Operation Instruction }
    inJMP,
    inJZ,
    inJNZ,
    inCall,
    inReturn,
    inNoop,

    {## Stack Operation Instuction }
    inPushInt,
    inPopInt,
    inDropInt,
    inDUPInt,
    inSWAPInt,
    inOVERInt,
    inROTInt
  ); 



TurboScript 标准库文件 Lib 格式： 
我想将内部关键字（函数）全部弄成库的形式，这样便于自举。
System.tcu (TurboScript Compiled Unit)
对于内部核心关键字应该是如何办啊？必须有一套汇编指令。既然是Forth机器，那么汇编指令就是Forth核心指令。

tcu<VersionNo>