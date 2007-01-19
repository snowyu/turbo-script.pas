我的虚拟机(Virtual Machine)说明：

我将它命名为： TurboScriptEngine. 

设计目标：
  高度可重用性
      * 层次分明，高度部件（组件）化： 在框架中的各个部件高度独立，可拆可组（任意拆卸，任意组合），着力通用。
      * 部件细化，设计精巧，运行高效，内存占用低。
      * 耦合度低(可拆可组)
  延展：我们需要架构具有可拓展性，以适应未来可能的变化。
    据库执行功能：the run-time script types, constants, global variables and procedures can be stored in the memory or database.
  高度自由缩放性：即可作为脚本解释器，也可JIT运行，也可作为高级语言编译器。
  高速（既能够解释执行，也能够编译成机器码直接执行），小巧，高效；
  安全：运行安全稳定【可以通过完善TDD测试机制来保障】
  简明：一个复杂的架构不论是测试还是维护都是困难的。我们希望架构能够在满足目的的情况下尽可能的简单明了。

设计架构优秀的 Framework - 兼谈 ORM Framework
架构设计是一种权衡和取舍。一个Framework是为了解决某一个领域内的某些问题的代码复用而因运而生的，而问题总是有多种的解决方案的。而我们要确定唯一的架构设计的解决方案，就意味着我们要在不同的矛盾体之间做出一个取舍。我们在设计的过程总是可以看到很多的矛盾体：开放和整合，一致性和特殊化等等。任何一对矛盾体都源于我们对Framework的不同期望，需要我们在各种方案中作出不同的取舍。没有一个Framework能够满足所有的要求，只是架构的侧重不同。而一个设计优秀的 Framework 则是体现在其架构简单明了，层次分明，重用价值高；运行高效率，稳定（完善的TDD测试机制）。

首先由编译模块将程序全部翻译成Forth虚拟机器码,再由执行器将虚拟机器码翻译成不同平台的执行码并执行或者由执行器解释执行.

因此可以分为两大部件：编译器，执行器。

编译器
抽象层: uTurboCompiler.pas(CustomTurboCompiler, CustomTurboScriptModule)
使用层: TurboForthCompiler.pas, TurboPascalCompiler.pas, TurboBasicCompiler.pas, TurboCCompiler.pas, etc
扩展层: 

连接器模块：根据需要将编译好的VM单元静态引入主脚本程序模块，需要处理地址重定位，优化（如：其它unit的初始化过程是对某个单元变量赋值，但是该变量并没有被主程序使用，那么该初始化过程就不会被连接进入或执行）
抽象与使用层: uTurboLinker.pas

执行器
抽象层: 
  执行引擎核心：uTurboExecutor.pas(include abstract executor, TurboProgram classes)
    模块内存镜像类： TCustomTurboModule
    执行引擎抽象类： TCustomTurboExecutor
    AppDomain类： TTurboProgram 一个程序就是一个AppDomain.
  加载器： uTurboAccessor.pas (抽象的模块加载器，以及加载器的管理器)
    文件加载器： 加载文件模块
    数据库加载器： 加载存放于数据库种的模块
  
使用层: TurboInterpreter.pas, TurboJITExecutor.pas, TurboX86Executor.pas; TurboZ80Executor.pas, TurboJavaVMExecutor.pas
扩展层: 如 TurboDebugger.pas

JIT Translator 即时翻译模块：由执行器调用，将VM及时翻译成本族语言直接执行。

执行器中只包括Codes, ImportModules(自己提供给脚本使用的以及通过LoadLibrary装入的), Resource, 其它信息(ImportTable)只在PEFormat中存在。

TurboInterpreter_S: Pure Pascal 实现，暂缓
TurboInterpreter: 基于x86指令优化。核心指令汇编实现，寄存器采用x86的寄存器，对应关系如下：
ESP: 返回堆栈指针.记住压入减少，弹出增加地址。
EBP: 数据栈指针，基址指针放在内存某个单元中。所以EBP总是指向次栈顶。
EBX: 为数据栈栈顶。 
ESI: 指向当前指令地址
EDX: 状态寄存器(仅当条件编译指令：TurboScript_FullSpeed开启时使用，否则为临时寄存器)；(0Bit: 是否运行；1Bit:是否调试) TTurboForthProcessorStates = set of TTurboForthProcessorState; TTurboForthState = (psLoaded, psRunning, tfsDebugging, tfsCompiling)
     状态寄存器放于保留内存中了，为了能控制停止。不过这样一来执行性能下降了3。增加了条件编译指令：TurboScript_FullSpeed，开启时候启用 EDX 作为状态寄存器。
EAX: W Register 临时寄存器
ECX: 临时寄存器


EDI: FMemory基址

可以用PUSHAD 将这些通用寄存器保存于堆栈，供调用其他系统的过程时采用。然后POPAD.
现在我的问题，这些核心过程是用方法实现还是函数过程实现？用无参数的过程实现。


采用什么形式 THREADING TECHNIQUE 来实现呢？基于核心虚拟指令采用查表字典的方式！用户自定义Word采用相对地址（由于我占用了代码区前面的至少1024个字节，所以地址不可能小于255）表示。
那么我的函数表放在哪里好呢？全局变量的形式。

用户自定义Word 实际上 THREADING TECHNIQUE 类似于DTC(Direct Threaded Code) 模式，不过我有办法区分是否是VM机器指令，还是相对地址。
最核心的，最重要的Forth指令，我作为VM机器指令实现了：Next, Enter, Exit

用户自定义word:序列： 没有Enter了！只有Exit.

iVMNext
  TEST EBX, cIsRunningBit
  JZ @@Exit

  MOV EAX, [ESI]  //the current instruction in W register
  ADD ESI, Type(Pointer) //4 = INC PC INC PC INC PC INC PC
  
@@ExecInstruction:
  CMP  EAX, cMaxTurboVMDirectiveCount
  JAE   @@IsUserWord
@@IsVMCode:
  MOV  ECX, PTR GTurboCoreWords
  MOV  EAX, [ECX+EAX]
  JMP  [EAX]
@@IsUserWord:
  ADD  EAX, [EBP] //指向用户定义的word入口
  JMP  iVMEnter
@@Exit:

干脆不用CALL 全部 jmp 比较好！然后每一个最后都有一个JMP vmNext

iVMEnter: push the current IP(ESI),set the new IP, and run the vmNext
  PUSH ESI        //push the current IP.
  MOV  ESI, EAX   //set the new IP
  JMP iVMNext


iVMExit: pop to the IP(ESI),and run the vmNext.
  POP  ESI
  JMP  iVMNext

iVMHalt
  BTR EBX, cIsRunningBit  //clear the cIsRunningBit to 0.  = MOV EDX, cIsRunningBit; NOT EDX; AND EBX, EDX 
  JMP iVMNext

TCoreForthWords = array [Byte] of TProcedure;

Forth代码区内存镜像(TCustomTurboExecutor.Memory)：
FParameterStackBase(Pointer:是数据栈基址) FParameterStackSize(Integer:是数据栈大小)  
ReturnStackBase(Pointer: 返回栈基址) ReturnStackSize(Integer: 返回栈大小)
TIBLength(Integer) ToIn(Integer) TIB(PChar: 1024) LastWordEntry(Pointer: 用户自定义单词链入口)
type //in TurboScriptConsts
  PPreservedCodeMemory = ^ TPreservedCodeMemory;
  //the typecast for code memory area to get the parameters
  TPreservedCodeMemory = packed record
    States: TTurboProcessorStates;
    Executor: TCustomTurboModule;
    //##abondoned:this Module unique Index in this program, allocated by compiler.
    //##ModuleIndex: Integer;
    ModuleType: TTurboModuleType;
    ParamStackBase: Pointer;
    ParamStackSize: Integer; //bytes
    ParamStackBottom: Pointer;
    ReturnStackBase: Pointer;
    ReturnStackSize: Integer; //bytes
    ReturnStackBottom: Pointer;
    UsedMemory: Integer;
    MemorySize: Integer; 
    //ToIn: Integer; //>IN the text buffer current index
    //TIBLength: Integer; //#TIB the text buffer length
    //TIB: array [0..cMAXTIBCount-1] of char; //'TIB
    LastErrorCode: TTurboProcessorErrorCode;
    //如果ModuleType是模块，那么就是装载运行该模块前执行的初始化过程，入口地址
    //如果是函数，则是该函数的入口地址
    InitializeProc: Pointer; //it is the offset address of the FMemory
    FinalizeProc: Pointer; //如果是模块的话
    //last Used(import) module entry.
    LastModuleEntry: PTurboModuleEntry;
    //有名字的函数链表，指向最后一个函数入口。
    LastWordEntry: PTurboWordEntry;
    //有名字的变量链表
    LastVariableEntry: PTurboVariableEntry;
    //RTTI TypeInfo 链表
    LastTypeInfoEntry: PTurboTypeInfoEntry;
    //reserved: array [SizeOf() ] of byte; 
  end;

测试VM代码：
2 3 +

pushInt 2 pushInt 3 AddInt Halt 

运行性能 QueryPerformanceCounter 大概 6-8 QueryPerformanceCounter.
而其他性能则要 400-800.QueryPerformanceCounter. 我的是他们的性能 ((1/6) / (1/400)) * 100 % = 6666 %。
而delphi 的 a:= 2+3 运行需要的时间 是 4-7, 只比Delphi慢( (1- (1/16) / (1/9)) * 100 % = 43.75%)。
现在我用了内存作为状态寄存器（为了能停止），性能下降了. 比Delphi慢了 【 1- (1/18) / (1/9) = 50% ProcessorStates = 1 byte】 【 1 - (1/36)/(1/9) = 75% ProcessorStates = 1 word 】

Luna5: Delphi 1-((1/6.57)/(1/0.61))= 1-0.0928 只是Delphi 性能的 9.28%,比delphi慢了90.72%.


-- $Id$
-- http://www.bagley.org/~doug/shootout/
-- contributed by Roberto Ierusalimschy

local sum = 0
for line in io.lines() do
  sum = sum + line
end
print(sum)

Luna5 CPU Time 6.57

Delphi CPU Time: 0.61
program sumcol;
var
  num, tot: integer;
begin
  tot:=0;
  while not Eof(input) do begin
    readLn(input, num);
    tot := tot + num;
  end;
  WriteLn(tot);
end.

变量区，代码区，都在FMemory中。
可以看作是该executor的局部变量（或参数）。
里面的变量、函数可以有名字，也可以没有。有名字的以链表的形式聚集在一起。

如何区分指令和入口地址（为 CALL 入口地址）？
方式1：指令长度与地址指针的长度一样，由于我占用了代码区前面的至少1024个字节，所以地址不可能小于255，因此<256的为虚拟指令，否则为入口地址。
方式2：指令长度为1字节，专门增加一个指令CALL,后面紧跟用户定义单词的入口地址。

采用方式1，理由：
  1、节约内存，系统的扩建主要是在用户自定义单词上
  2、便于扩充系统指令，方式2最多只能有256个指令，而且无法扩充。

文件支持层: 
  uTurboModuleAccessor(模块装入保存机制); 
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


uTurboModuleAccessor(模块加载保存机制详述):

TTurboModuleAccessor: abstract class  <-- TTurboModuleFileAccessor, TTurboModuleDataSetAccessor
TTurboModuleAccessorList: register the accessor to here. 所有注册的加载器全部在这里。

TTurboModuleManager: manage the Turbo Module Accessores. 所有的在内存中的模块都在这里。
   //find and load the module into memory.
  function Require(const aModuleName: String; const aModuleClass: TTurboModuleClass; const aGlobal: PTurboGlobalOptions; const IsLoaded: Boolean): TCustomTurboModule;

函数说明：
function GTurboModuleManager.RegisterAccessor(const AccessorClass: TTurboModuleAccessorClass; const IsDefault: Boolean = False): Boolean;
注册加载器，所有的加载器都是singleton单件实例。

uTurboModuleFileAccessor

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

文件头格式：
  TTurboModuleStreamHeader = packed record
    Id: array [0..cFORTHHeaderMagicIdLen-1] of char; //MagicWord: 辨别是否为TurboScript文件格式。
    Version: LongWord;
    BuildDate: TTimeStamp;
  end;

紧接着的内容就是：Forth代码区内存镜像(TCustomTurboExecutor.Memory)。

