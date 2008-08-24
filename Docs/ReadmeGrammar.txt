== ���뿪�� ==
���еĽű����Զ�֧�����µı��뿪��:

+ Compiler Switch: 
  + Assert directives
    �Ƿ����ö��ԡ�
    Syntax	{$C+} or {$C-}
            {$ASSERTIONS ON} or {$ASSERTIONS OFF}
    Default	{$C+}
            {$ASSERTIONS ON}
    Scope	  Local
    ��Delphi���ƣ�����رգ��򲻻����Assert���̡�
    Ҫ��ʹ�ö��ԣ�����ʹ�ò����������������룬���ӹ��̲�����Ȼ���룬����: �����Assert(False, 'helo'); �Զ�����ɣ�
      Push False; Push 'Helo'; CALL Assert
  + MESSAGE directive
    ��ʾ�����û��������Ϣ�������﷨������Ϣ���������﷨�����������Ϣ�����������������ɹ�����
    Syntax    {$MESSAGE  HINT|WARN|SYNTAX|SYMANTIC 'text string' }
    Examples: {$MESSAGE 'Boo!'}                   emits a hint 
  + MaxCodeSize directive
    �������Ĵ�����ռ���ڴ��С�����С��ʵ�ʱ����Ĵ����С������ʵ�ʴ����СΪ׼��
    Syntax    {$MaxCodeSize  Size[KB|MB] }
    ע�⣺�������Program|Unit֮�󣬵� MaxCodeSizeΪ-1��ʾ����ʵ�ʴ������СΪ׼��
  + MaxDataSize directive
    ������������ռ���ڴ��С�����С��ʵ�ʱ����Ĵ����ݴ�С������ʵ�����ݴ�СΪ׼��
    Syntax    {$MaxDataSize  Size[KB|MB] }
    ע�⣺�� MaxDataSizeΪ-1��ʾ����ʵ�����ݴ�СΪ׼��
  + LoadOnDemand directive
    �Ƿ�Ԥ�ȼ��أ����õģ�ģ�飬���ǰ���Ҫ���أ������ű����õ�ģ��Ĺ��̵�ʱ���ȥ���أ�
    Syntax    {$L+|-} or {$LoadOnDemand ON|OFF}
    Default:  {$LoadOnDemand OFF}
    ע�⣺�򿪰�����صĺô�������ű�û��ִ�е�ĳ��ģ��Ĺ���ʱ�����ģ�鲻�����ڴ��У�������ģ���ļ����Բ����ڣ����ܽ�ʡ�ڴ棬����Ӱ����Ч�ʡ�



== TurboForth Grammar ==

ע��֧��
* ����ע��: //
* ����ע��: {}

TurboForth֧�������ļ�: �����ļ��͵�Ԫ���ļ�,������������ֻ�����в�ͬ.��������ȴ��һ��,һ����ʾ����ִ�еĳ���,��һ�����ǹ����õĵ�Ԫ��,���ܱ�ֱ��ִ��.
begin...end.��ĺ���ҹ��һ��,�ڳ����ļ������������ִ�����,����Ԫ�ļ������ǵ�Ԫ�ĳ�ʼ������.

�ҽ� TurboForth ��Ϊ TurboScript �ĵͼ��������.

�����ļ�������ʽ����:
<pre>
program aName;
  //[UseModulesBlock] ��ѡ��.
  uses test; 
  uses Another;
  //[DeclarationBlock]: ������������,��������,��������,��������
  type
    cc = string;
    bb = integer;
  const
    a = 123;
    b = 344;
  var
    s:string [= 33];
  //�������
  : Add(a,b: integer)  + ;
  
begin
end.
</pre>

��Ԫ���ļ�������Delphi��:
<pre>
Unit aName;
 
  Const aConstName = Value;
  [PUBLIC [NAMED] [TYPED]] Var aVarName: Type [= InitValue];
  : [PUBLIC NAMED] DefinedWord
     [���] ������ [������]
  ;
  
End.
</pre>

=== 

=== �ķ�����ʽ ===
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

