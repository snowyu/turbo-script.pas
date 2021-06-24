# The Open Source TurboScript Infrastructure Framework

The execution speed of the script is close to the machine assembly language.

StaticClass Type -> Record Type -> Object Type -> Class Type

Purpose(Target):

* It can be a Interpreter.
  * You can write your Interpreter by yourself.
  * It's the fastest/smallest byte-code virtual machine.
* It can be a JITter like .Net.
  * You can write your CLR(.Net) implementation like mono by yourself.
* It can be an Embeddable/Independence Compiler to generate the real machine code for processors.
  * You can write your compiler by yourself.
  * You can add your new syntax by yourself.
* It can be fully configurable.
  * The Type System can be fully configurable and self-explained.
  * It can be type-safety(StrongType): the types can have the typeInfo at runtime or not. (decided by yourself!)
  * It can be named type.  the types have the name of the type at runtime or not. (decided by yourself!)

Features:

* Highly Reusable Objects Engine
  * Clearly demarcated layers
  * Highly Separable.Components(Classes)
* Highly Scalable Engine
  * Highly Configurable
    * StrongType Supports or not
    * The external function can Load On demand or PreLoad(Module).
    * Self-explained Common Type System.
    * distributed virtual machine.
  * the CLR can call the functions in DLL or Host.
  * the host can call any function in CLR.
  * Executor Class Configurable(choose freely)
    * Interpreter
    * JITter
* Highly Extensible Engine
  Highly Extensible Common Type System: all types(include system types) can be defined by developer themselves.
* High Speed with small footprint.
* Top Security

Roadmap:

I. The first Phase - Codename: TurboScript
  V 0.7[Done]: the simple execution efficiency test of the TurboScript engine is here:
    TurboCompiler: TurboForthCompiler
    TurboSymbol
    TurboModule
    TurboExecutor: TTurboInterpreter, TTurboX86Interpreter
    TurboAppDomain
    TurboModule Loader: TTurboModule File Loader

  V 0.8:
    the Mini Compile-time/Runtime Core
    TurboPascalCompiler: TurboPascal RTL Library.
    TTurboModule DB Loader


  V1.0:
    TurboCSharpCompiler, TurboBasicCompiler ...
    TurboTranslator
    TTurboJITExecutor
    TurboDebugger
    TTurboModule Web Loader

  V1.1:
    Re-write the Mini Compile-time/Runtime Core with another languages(eg, C) to boot anther OS.

II. The Second Phase- Codename: TurboCompiler
  V2.0:
    TurboExecuteFileFormat: TurboExecuteFileFormat

III. The Third Phase - Codename: TurboUML
  V3.0: Design your all source code by UML editor like the Model Maker Tools. And generates the any supported high level language source code or compiles into execute binary file directly from UML automatically.

Layers:

* TurboModule Compiler
  * TurboSymbol
  * TurboTranslator
  * TurboCompiler: compile the source into the TurboScript Virtual Machine code stream.
    * TurboASMCompiler, TurboForthCompiler, TurboPascalCompiler, TurboCSharpCompiler, TurboBasicCompiler, etc
  * TurboExecuteFileFormat: encode the translated native code stream to executable file formats which the OS supported.
    * TurboPEFileFormat, TurboELFFileFormat
* TurboModule Executor
  * TurboModule
  * TurboExecutor
    * TTurboInterpreter, TTurboX86Interpreter, TTurboJITExecutor
  * TurboAppDomain
  * TurboDebugger
* TurboModule Loader
  * TTurboModule File Loader: load TurboScript module from File.
  * TTurboModule DB Loader  : load TurboScript module from DB.
  * TTurboModule Web Loader : load TurboScript module from Web.

TurboModule Layer:
  the TurboScript Virtual Machine code in the memory. It's the smallest TurboScript Virtual Machine code unit in the memory.
  the TurboModule can be treated as a CLR Static Class.

TurboCompiler Layer:
   The Turbo Compiler compile the source into the TurboScript Virtual Machine code stream.
  Core Layer:
    TurboSymbol:
      * Redesign the TurboSymbol Architecture(Compiler)
  Application Layer:
    V1.0: USE THE COCO/R
      Tokenizer, Parser(the COCO/R implemented. TODO: Modify the COCO/R source to speedup.)
      TurboASMCompiler, TurboForthCompiler, TurboPascalCompiler, TurboCSharpCompiler, TurboBasicCompiler:
        1. Translate the source to the TurboSymbols.
        2. save to file stream


TurboTranslator Layer: translate the Virtual Machine Code into native code.
  TurboTranslator:    abstract Translator
  TurboX86Translator: translate the Virtual Machine Code into the native code of X86 processors.
  TurboJitTranslator: translate the Virtual Machine Code into the simple genernal JITter native code.

TurboExecuteFileFormat: encode the translated native code stream to executable file formats which the OS supported.
  TurboExecuteFileFormat: abstract Executable Binary File Format.
  TurboPEFileFormat: Encode the native code to Windows PE File Format;
  TurboELFFileFormat: Encode the native code to Linux ELF File Format;

TurboExecutor Layer:
  The execution mechanism of the TurboScript Virtual Machine code stream.

  TTurboInterpreter:
  TTurboX86Interpreter: the fastest interpreter(X86 optimal).
  TTurboJITExecutor: the Just In Time(JIT) executor.
    JITter can be security of the Virtual Machine Code.

  TurboAppDomain: the minimal run-able script unit.
   It manages a TurboModule and an TurboExecutor.

TurboAccessor Layer: load and manage TurboModules in the memory.
  TTurboAccessor: the pure abstract accessor class: the module accessor and ModuleManager are derived from it.
  TTurboModuleAccessor: Abstract TurboModule Accessor[singleton]
  TTurboModuleManager: The TurboScript Module Manager.
    the GTurboModuleManager global variable manages all the loaded(cached)
    modules. And register the new ModuleAccessor to GTurboModuleManager.

    Require method return the specified module. It first
    searches in the memory if not found, try load the module
    through registered module accessors.
  TTurboModuleFileAccessor: load TurboScript module from File.
  TTurboModuleDBAccessor: load TurboScript module from DB.
  TTurboModuleWebAccessor: load TurboScript module from Web.


Dependency Layers:

TurboCompiler --> TurboSymbol(symbol-tree) -->  TurboModule(Virtual Machine Code) --> TurboExecutor
                                                       ^
                                                       |
                                                       |
                                                   TurboAccessor

TurboExecutor [-->TurboTranslator(Real Machine Code) --> TurboExecuteFileFormat(executable binary file formt)]


TurboAppDomain__|--> TurboModule
                |--> TurboExecutor


TurboSymbol Class Architecture(Compiler)

The first phrase of the TurboSymbol has done.

TurboCustomSymbol

    * Public
    * + Line, Column property
    * + UnResolvedRefs Property: UnResolved symbol reference list of this symbol.
    * + RefCount Property  the count of this symbol is referenced.
    * + OnError Event: TTurboCompilerErrorEvent = procedure(const Sender: TurboSymbol; const aErrCode: Integer) of object;
    * + CompileError(const aErrCode: Integer);
    * + Compile Method Compile this symbol and ResolveRefs when IsCompileAllowed.
    * + ReferenceTo(const aSymbol: TurboSymbol) Method  Reference this symbol into another symbol(MUST BE MethodSymbol or ModuleSymbol) and increase the RefCount.  (该符号在aSymbol的当前位置被引用,同时引用计数+1)
    * + ResolveRefs Method  resolve all the address in the UnResolvedRefs list and clear the list when compile successful.
    * + IsCompileAllowed:Boolean Virtual Method  The symbol will be compiled only this returns true(RefCount > 0)
    * + Assign(const aSymbol: TTurboCustomSymbol) Virtual Method  Copy name and value of another symbol object.
    * Protected
    * + iCompile Virtual Method(Abstract)
    * + iReferenceTo Virtual Method(Abstract)  (如果不能解决地址,则将 UnResolvedRefRec 项加入未解决地址列表:UnResolvedRefs)
    * + ResolveAddr Virtual Method(Abstract)  (解决在引用的时候没能解决地址的项,将在编译后被调用,解决地址. 该过程由ResolveRefs 调用.)

TurboSymbol(TurboCustomSymbol)

    * + OwnerSymbol: TTurboModuleSymbol Property [^Todo: change type TTurboModuleSymbol  to TTurboSymbol ]
    * + CompileError(const aErrCode: Integer);
    * + IsPublic: Boolean
    * + IsTyped: Boolean
    * + IsNamed: Boolean
    * + override IsCompileAllowed:Boolean Virtual Method  Return true()
    * * Compile:  Compile the symbol into the OwnerSymbol. (将编译到OwnerSymbol中。)

TTurboConstSymbol(TurboSymbol)

    * + override iCompile Virtual Method   (检查有无类型,没有就报错,否则如果常量是字符串类型,那么将该字符串编入 Module.)
    * + override iReferenceTo Virtual Method  (限定只能被 Method 引用! 如果是字符串类型检测能否解决字符串地址,不能则将该项加入 UnResolvedRefs(未解决地址列表))
    * + override ResolveAddr Virtual Method (如果是字符串常量将编译后的字符串地址保存到原来的aSymbol中的地址.)

TTurboVariableSymbol(TTurboConstSymbol)

    * + override iCompile Virtual Method (根据是否Public变量,决定是否将该符号放到静态字段列表中, 在Module 的 DataMemory上开辟变量空间,并将其变量地址记下.)
    * + override iReferenceTo Virtual Method  (限定只能被 Method 引用! 检查变量地址是否解决,没有解决则将该项加入 UnResolvedRefs(未解决地址列表))
    * + override ResolveAddr Virtual Method (将编译后的变量地址保存到原来的aSymbol中的地址.)

TTurboMethodSymbol(TurboSymbol)

    * + override iCompile Virtual Method  (首先编译当前方法引用的其它符号(CompileRefSymbols),然后根据 CodeFieldStyle 类型分别编译.)
    * + override iReferenceTo Virtual Method  (限定只能被 Method 引用! 根据 CodeFieldStyle 类型分别处理,检查方法地址是否解决,没有解决则将该项加入 UnResolvedRefs(未解决地址列表))
    * + override ResolveAddr Virtual Method (根据 CodeFieldStyle 类型, 将编译后的方法地址保存到原来的aSymbol中的地址.)
    * + BodyInit[^]:  在进入代码体前调用,初始化工作: 如果该方法没有被添加到 OwnerSymbol 则添加, 然后如果是函数不是引用,则清空代码体内存.
    * + BodyFinal[^]: 在代码体最后调用,收尾工作! 如果是主入口则需要清理所有是字符串的静态字段.如果是引用函数则需要搜索引用的哪一个模块中的函数

TTurboModuleSymbol

如果没有OwnerSymbol的,就认为是主Module: 正在编译的Module, 否则就是 Module 的引用.[^]

    * + override iCompile Virtual Method (根据是否有OwnerSymbol, 分别编译. 有则被认为是引用module,)
    * + override iReferenceTo Virtual Method  None (无,简单的返回OK)
    * + override ResolveAddr Virtual Method   None (无)

TurboTypeSymbol

    * + override iCompile Virtual Method (如果该类型不是内部类型,那么则将该类型添加到 OwnerSymbol 模块上的 RegisteredTypes 中.)
    * + override iReferenceTo Virtual Method  None (无,简单的返回OK)
    * + override ResolveAddr Virtual Method   None (无)


