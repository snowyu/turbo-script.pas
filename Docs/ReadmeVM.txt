�ҵ������(Virtual Machine)˵����

�ҽ�������Ϊ�� TurboScriptEngine. 

���Ŀ�꣺
  ���٣����ܹ�����ִ�У�Ҳ�ܹ�����ɻ�����ֱ��ִ�У���С�ɣ���Ч��
  ���ݿ�ִ�й��ܣ�the run-time script types, constants, global variables and procedures can be stored in the memory or database.

�����ɱ���ģ�齫����ȫ�������Forth���������,����ִ��������������뷭��ɲ�ͬƽ̨��ִ���벢ִ�л�����ִ��������ִ��.

����ģ�飺������ģ�飬ִ����ģ��

������ģ��
�����: uTurboCompiler.pas(CustomTurboCompiler, CustomTurboScriptModule)
ʹ�ò�: TurboForthCompiler.pas, TurboPascalCompiler.pas, TurboBasicCompiler.pas, TurboCCompiler.pas, etc
��չ��: 

ִ����ģ��
�����: uTurboExecutor.pas(include abstract PEFormat, executor , debugger and TurboProgram classes)
ʹ�ò�: TurboX86Executor.pas; TurboZ80Executor.pas, TurboJavaVMExecutor.pas
��չ��: ��, TurboInterpreter.pas; TurboDebugger.pas;  

ִ������ֻ����Codes, ImportModules(�Լ��ṩ���ű�ʹ�õ��Լ�ͨ��LoadLibraryװ���), Resource, ������Ϣ(ImportTable)ֻ��PEFormat�д��ڡ�

TurboInterpreter_S: Pure Pascal ʵ�֣��ݻ�
TurboInterpreter: ����x86ָ���Ż�������ָ����ʵ�֣��Ĵ�������x86�ļĴ�������Ӧ��ϵ���£�
ESP,EBP: ���ض�ջ.
EDI��ջָ�룩: ����ջ����ַָ������ڴ�ĳ����Ԫ�С�EAX Ϊ��ջջ������ջ����
ESI: ָ��ǰָ���ַ
EBX: ״̬�Ĵ��� (0Bit: �Ƿ����У�1Bit:�Ƿ����)
ECX: W Register ��ʱ�Ĵ���

������PUSHAD ����Щͨ�üĴ��������ڶ�ջ������������ϵͳ�Ĺ���ʱ���á�Ȼ��POPAD.
�����ҵ����⣬��Щ���Ĺ������÷���ʵ�ֻ��Ǻ�������ʵ�֣����޲����Ĺ���ʵ�֡�
�涨��
EBP+4: ��ָ����TTurboX86Interpreterʵ�����ڵ�ַ��
EBP: ��ָ���Ǵ����ڴ��ַ;
EBP-4(SizeOf(Pointer)): ��������ջ��ַ.

PUSH EAX      
PUSH FMemory 
MOV  EBP, ESP
PUSH FParameterStack

����ʲô��ʽ THREADING TECHNIQUE ��ʵ���أ����ڲ���ֵ�ķ�ʽô��
��ô�ҵĺ��������������أ�ȫ�ֱ�������ʽ��

TCoreForthWords = array [Byte] of TProcedure;

�������ڴ澵��
TIBLength(Integer) ToIn(Integer) TIB(PChar: 1024) LastWordEntry

�������ָ�����ڵ�ַ��Ϊ CALL ��ڵ�ַ����

�ļ�֧�ֲ�: 
  uTurboScriptAccessor(ģ��װ�뱣�����); 
  ʵ�ʵ�װ�ء�ж�ط��������������ļ������ݿ����ģ�飬ģ���Ψһ�ԡ�
  uTurboPEFormat.pas(���ĸ�ʽ); uTurboWin32PEFormat(windows32��PE��ʽ);
�����Ŀ�ִ���ļ���ʽ, PE: Portable Executable File Format 
���ܣ����ݸ�ʽ��װ��Import���е�ģ��(DLL��ForthDLLģ��)���ض�λ��ַ���Լ�����RES��Դ���Լ���Ҫ���¼���ľ��Ե�ַ��relocation��
1��Import��(if any)
2��Export��(if any)
3��Relocation��(if any)����Ϊ�����ض�λ��������ض�λ��
4��Resource��(if any)
5��Code�α�(if any)
7��symbol���ű�(if any)���ڵ���

ע�⣺���ݰ�����ѧϰ��PE��ʽ��Ҳ��Ӧ�ý�����֯��Section��Section����Ϳ�������Ծ����������ʽ�μ�ExeCompile.txt��

���SectionӦ�ó�֮Ϊģ��ɣ�TTurboScriptModule, ģ��ֻӦ���ڱ����ڴ��ڡ�SectionӦ���ǱȽϼ򵥵ĸ�ʽ����������Ч�ʲŸߡ�

ForthDLL����DLL���ƣ���������Ĵ��벻�ǻ����룬����Forth��VM�롣


���˵��:

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
    ���������������������
    ӵ��һ��IR(instruction register)ָ��Ĵ���, һ��SP��ջָ��Ĵ���, һ��BP��ջ��ַָ��Ĵ���, һ��PC����ָ��Ĵ�����һ��״̬�Ĵ���.
    ָ��洢�����֣��������ŵȴ�ִ�е�ָ�����У����õ����������ʽ��
    ����ִ��Execute��ֹͣ(Stop)��
  TODO
    �Ƿ���Ҫ������Թ��ܣ�
}
TCustomVMProcessor = Class
end;

{ Summary : the Abstract stack-oriented Virtual Machine processor }
{ Description :
  Chinese 
    ���ڶ�ջ�����������������
}
TStackVMProcessor = Class(TCustomVMProcessor)
end;

{ Summary : the Forth Virtual Machine processor }
{ Description :
  Chinese 
    ���ڶ�ջ��Forth�����������
}
TForthVMProcessor = Class(TStackVMProcessor)

{ Summary ����ı�����for Forth Language}
TCustomCompiler = Class

{ Summary ����Ľű�ִ���� for Forth Language}
{ Description
  1�����أ������ļ�ͷӳ���ʽ������ģ����ض�λ��ַ��
  2�����룬����ɱ��ػ�����
  3��ִ�нű���
}
  TCustomScriptExecutor = Class
  private
  // ������ CodeArea : ������д��롣
    FCodeArea: array of byte;
  // ������ DataArea : ���ȫ�ֱ�����
    FDataArea: array of byte;
  // ����ջ�� ����ǰ������ջָ����Ϣ��Ȼ��ϵͳջָ��ָ���FReturnStack�� ESP := @FReturnStack[0] + Length(FReturnStack) - 1; EBP := ESP;
  // 
    FReturnStack: array of byte;
  // ����ջ
    FDataStack: array of byte;
  // ������ĺ�����ַ
    FFunctions: array of TFunctionRec;
    FVariables: array of TVariableRec;

  protected
    function InternalExecute: Integer;virtual;

  public
  //ע��������ĺ���������ִ������
    procedure AddFunction(AName:string; aProc:Pointer);
    procedure AddVariable(AName:string; aVar:Pointer);

    function Execute: Integer;

    function LoadFromStream(const ResolveAddressReq: Boolean=True): Integer;virutal;
    function LoadFromFile(const ResolveAddressReq: Boolean=True): Integer;
  end;

TFunctionProc = Function():Integer;

{ Summary Intel x86 CPU ��ִ����}
  TIntelx86Executor = Class(TCustomScriptExecutor)
  private
    // �������ܵ���
    FDoExecute: TFunctionProc;


  end;

����˵��:
����������������:
 1. ������������ָ��洢����
   ���ȣ���Ҫ��ָ����ȷ�Ϊ��
     1.��ָ��: û���κβ�����ָ��
     2.������ָ��: ��һ��������ָ��

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
   
����һ���򵥵�ָ�����̣� ��2 3 +����������������µ�ָ������
  opPush 2, opPush 3, opAdd
ʹ�� fixed length VMCode:


�ڲ�(Internal)��������
Ҳ�����ڲ��ؼ��֣����̣���IDΪLongWord��$1-$FFFFFFFF��,���Է�Ϊϵͳ�ؼ��ֹ���,ID��1-$FFFF���û��Զ���ؼ��ֹ��̣�ID($10000-$FFFFFFFF).
��������ڲ�����ִ�У���������Ҫ���ͣ���ô�Ͳ��ò��(TTC)��ֻ�е���Ҫ�����ʱ��Ż���ִ�е�ַ(DTC or STC)��

TTC(���)�ı� ����ϵͳ�ؼ��֣�������ϸ����($1-$1FF)Ϊ����ϵͳ�ؼ��֣������������ʽֱ�Ӵ��ִ����ڵ�ַ��
�����������Ĺؼ��������ɢ�е�������ʽ����Լ�ڴ档�ٺ٣������˸��õ����⣬ֱ�Ӳ��õ�ַ��
�����ڲ��ؼ�����ȫ������������ʽ���¼ӵ��������󣨵�Ȼ�����ĺ�������û��Զ���ĺ������ܲ���Ψһ���ͱ���û��������ͻ����


ϵͳ�ؼ��ֹ���:
  ����������̣�
  �߼�������̣�
  �ַ�������
  ���̿��ƣ� if...else, while, for...next, repeat...until.


 2. ���ض�ջ��
 3. ����������
 4. �ڲ�������(Procs): ������Щ�̶��Ļ���ָ�����/

FORTH����������������������������������:
 4. ���ݶ�ջ��

TTC(���ʽ��)�Ĵ�����ʹ�ã�

TurboScript ����ָ��(Forth)��ࣺ
����ָ�����ֽ���Ϊ���ȡ����仰˵������ָ�����255�����ڽ������У�VM����ָ�����Բ��ķ�ʽ����ִ�С��ڷ������н���ЩVMָ���������Ļ�������Ȼ����ִ�������磺X86Executor����ִ�С�
ָ��ɷֵ��ֽ�ָ��Ͷ��ֽ�ָ�ָ���������

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



TurboScript ��׼���ļ� Lib ��ʽ�� 
���뽫�ڲ��ؼ��֣�������ȫ��Ū�ɿ����ʽ�����������Ծ١�
System.tcu (TurboScript Compiled Unit)
�����ڲ����Ĺؼ���Ӧ������ΰ찡��������һ�׻��ָ���Ȼ��Forth��������ô���ָ�����Forth����ָ�

tcu<VersionNo>