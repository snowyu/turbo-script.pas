== 编译开关 ==
所有的脚本语言都支持如下的编译开关:

+ Compiler Switch: 
  + Assert directives
    是否启用断言。
    Syntax	{$C+} or {$C-}
            {$ASSERTIONS ON} or {$ASSERTIONS OFF}
    Default	{$C+}
            {$ASSERTIONS ON}
    Scope	  Local
    和Delphi类似，如果关闭，则不会编译Assert过程。
    要想使用断言，必须使用参数，可以这样编译，增加过程参数，然后翻译，比如: 将语句Assert(False, 'helo'); 自动翻译成：
      Push False; Push 'Helo'; CALL Assert
  + MESSAGE directive
    显示各种用户定义的消息，包括语法错误消息（当发生语法或语意错误消息，编译器将不会编译成功）。
    Syntax    {$MESSAGE  HINT|WARN|SYNTAX|SYMANTIC 'text string' }
    Examples: {$MESSAGE 'Boo!'}                   emits a hint 
  + MaxCodeSize directive
    设置最大的代码体占用内存大小，如果小于实际编译后的代码大小，则以实际代码大小为准。
    Syntax    {$MaxCodeSize  Size[KB|MB] }
    注意：必须放在Program|Unit之后，当 MaxCodeSize为-1表示以以实际代码体大小为准。
  + MaxDataSize directive
    设置最大的数据占用内存大小，如果小于实际编译后的代数据大小，则以实际数据大小为准。
    Syntax    {$MaxDataSize  Size[KB|MB] }
    注意：当 MaxDataSize为-1表示以以实际数据大小为准。
  + LoadOnDemand directive
    是否预先加载（引用的）模块，还是按需要加载（仅当脚本调用到模块的过程的时候才去加载）
    Syntax    {$L+|-} or {$LoadOnDemand ON|OFF}
    Default:  {$LoadOnDemand OFF}
    注意：打开按需加载的好处是如果脚本没有执行到某个模块的过程时不会该模块不会在内存中（甚至该模块文件可以不存在），能节省内存，但是影响了效率。



== TurboForth Grammar ==

注释支持
* 单行注释: //
* 多行注释: {}

TurboForth支持两种文件: 程序文件和单元库文件,它们在声明中只是略有不同.不过含义却大不一样,一个表示可以执行的程序,另一个则是供调用的单元库,不能被直接执行.
begin...end.块的含义夜不一样,在程序文件中是主程序的执行入口,而单元文件中则是单元的初始化过程.

我将 TurboForth 作为 TurboScript 的低级汇编语言.

程序文件大致形式如下:
<pre>
program aName;
  //[UseModulesBlock] 可选的.
  uses test; 
  uses Another;
  //[DeclarationBlock]: 包括常量声明,变量声明,类型声明,过程声明
  type
    cc = string;
    bb = integer;
  const
    a = 123;
    b = 344;
  var
    s:string [= 33];
  //定义过程
  : Add(a,b: integer)  + ;
  
begin
end.
</pre>

单元库文件类似于Delphi的:
<pre>
Unit aName;
 
  Const aConstName = Value;
  [PUBLIC [NAMED] [TYPED]] Var aVarName: Type [= InitValue];
  : [PUBLIC NAMED] DefinedWord
     [标号] 操作码 [操作数]
  ;
  
End.
</pre>

=== 

=== 文法产生式 ===
CHARACTERS
 LETTER="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_".
 DIGIT =  "0123456789".
 DOT = ".".
 IDENTCHARS = LETTER + DIGIT.
 CTRL=CHR(1)..CHR(31).
 SpecialChar = "*,()%:".
 TAB=CHR(9).
 EOL=CHR(13).
 LF=CHR(10).
 noQuote = ANY - "'" .
 noDblQuote = ANY - '"' .               
 noEOL = ANY - EOL .
 HexDigit = "0123456789ABCDEF" .

TOKENS
 Identifier = LETTER {IDENTCHARS|DOT IDENTCHARS}.
 IntegerToken = DIGIT {DIGIT} | DIGIT {DIGIT} CONTEXT ( ".." ).
 HexNumberToken = "$" HexDigit {HexDigit}.
 FloatToken = DIGIT {DIGIT} "." DIGIT {DIGIT} ["E" ["+" |"-"] DIGIT {DIGIT}].
 StringToken   = "'" { noQuote | "'" "'" } "'" .

NAMES

IGNORE TAB+EOL+LF

COMMENTS FROM "//" TO EOL
COMMENTS FROM "{" TO "}"

PRODUCTIONS

  TurboForthCompiler DESCRIPTION "TurboScript Assembler Compiler"
  = 
    ( "PROGRAM"|"UNIT") Identifier
    [UseModulesBlock]
    [DeclarationBlock]
    "BEGIN"
      [ASMStatementBlock]
    "END."

  UseModulesBlock = UseModule {UseModule}.

  UseModule = "USES" Identifier ";" .

  DeclarationBlock =     Declaration {Declaration}.

  Declaration = DefineConstPart | DefineVarPart | DefineTypePart | DefineWord .

  DefineTypePart =    "TYPE" DefineType {DefineType}   .

  DefineType =    Identifier  "=" (TypeKind|RestrictedType) ";"  .

  TypeId =    Identifier  .
  
  TypeKind =    TypeId  .

  RestrictedType = ClassType.

  ClassType = "CLASS" //[ClassHeritage]  .

  DefineConstPart = "CONST" DefineConstant {DefineConstant}  .

  DefineConstant  = Identifier [":" TypeKind] "=" Value ";"  .

  DefineVarPart  = "VAR" DefineVariable {DefineVariable} .

  DefineVariable = [WordVsibility] Identifier ":" TypeKind ["=" Value] .

  Value = (StringToken|IntegerToken|HexNumberToken|Identifier)  .

  DefineWord = ":" [WordVsibility] Identifier [FormalParameters] [":" ResultParamType] [ExternalWordOption]
     [ASMStatementBlock] ";" .

  FormalParameters = "(" [FormalParms] ")"  .
  FormalParms = FormalParm {";" FormalParm} .

  FormalParm =  [ "VAR"|"CONST"|"OUT"] Parameter<vParam> .

  Parameter  = Identifier ":" TypeKind .

  ResultParamType = TypeKind .

  ExternalWordOption = "EXTERNAL" StringToken
      [ModuleType] 
      [("STDCALL"|"REGISTER"|"PASCAL"|"FORTH")
        ["Name" StringToken]
        ["Index" IntegerToken]
      ].

  ModuleType  = "LIB"|"DLL"|"HOST".

  WordVsibility = "PRIVATE" |("PUBLIC" ["NAMED"]["TYPED"])
  .

  ASMStatementBlock = ASMStatement {ASMStatement}
    .

  ASMStatement
   = 
    [LabelDeclaration]
     PushIntAsmStatement
    | PushStringAsmStatement
    | AddIntAsmStatement
    | "i8+"
    | SubIntAsmStatement
    | "i8-" 
    | MulIntAsmStatement
    | "!" 
    | "!i8"
    | "!i2"
    | "!i1"
    | "@" 
    | "@i8"
    | "@i2"
    | "@i1"
    | "DROP"
    | EmitAsmStatement
    | "@TICK"
    | "!TICK"
    | "NOOP" 
    | AssertStatement
    | WordAsmStatement
  .

  LabelDeclaration  = "Label" Identifier .

  PushIntAsmStatement = ( IntegerToken|HexNumberToken ) .
  AddIntAsmStatement = "+"  .
  SubIntAsmStatement = "-"  .
  MulIntAsmStatement = "*"  .
  EmitAsmStatement = "EMIT" | ".S" | ".LS".

  PushStringAsmStatement = "#" StringToken  .

  WordAsmStatement  = Identifier [(. vParams := TStringList.Create; .) WordParams<aWord, vParams>  ] .
  WordParams  =  "("[ WordParam< vParam > {"," WordParam} ] ")".
  WordParam =( IntegerToken|HexNumberToken|Identifier|StringToken ) .
  AssertStatement = "ASSERT(" WordParam "," WordParam ")" .


== TurboPascal Grammar ==

Pascal like language compiler for TurboScript Engine.

program xxx;

uses
  xxx, xx;

type
  ;

var
  xx: ttt;

begin

end.

