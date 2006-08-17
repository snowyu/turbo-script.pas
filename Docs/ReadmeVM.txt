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
抽象层: uTurboExecutor.pas(include abstract PEFormat, executor and debugger classes)
使用层: TurboX86Executor.pas; TurboZ80Executor.pas, TurboJavaVMExecutor.pas
扩展层: 如, TurboInterpreter.pas; TurboDebugger.pas;  

执行器中只包括Codes, ImportModules(自己提供给脚本使用的以及通过LoadLibrary装入的), Resource, 其它信息(ImportTable)只在PEFormat中存在。

文件支持层: uTurboPEFormat.pas(俺的格式); uTurboWin32PEFormat(windows32的PE格式);
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


 2. 返回堆栈区
 3. 变量数据区
 4. 内部程序区(Procs): 就是那些固定的机器指令代码/

FORTH虚拟机处理的数据区除了上述的数据区外:
 4. 数据堆栈区