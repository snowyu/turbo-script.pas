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

������ģ�飺������Ҫ������õ�VM��Ԫ��̬�������ű�����ģ�飬��Ҫ�����ַ�ض�λ���Ż����磺����unit�ĳ�ʼ�������Ƕ�ĳ����Ԫ������ֵ�����Ǹñ�����û�б�������ʹ�ã���ô�ó�ʼ�����̾Ͳ��ᱻ���ӽ����ִ�У�
������ʹ�ò�: uTurboLinker.pas

ִ����ģ��
�����: uTurboExecutor.pas(include abstract PEFormat, executor , debugger and TurboProgram classes)
ʹ�ò�: TurboX86Executor.pas; TurboZ80Executor.pas, TurboJavaVMExecutor.pas
��չ��: ��, TurboInterpreter.pas; TurboDebugger.pas;  

JIT Translator ��ʱ����ģ�飺��ִ�������ã���VM��ʱ����ɱ�������ֱ��ִ�С�

ִ������ֻ����Codes, ImportModules(�Լ��ṩ���ű�ʹ�õ��Լ�ͨ��LoadLibraryװ���), Resource, ������Ϣ(ImportTable)ֻ��PEFormat�д��ڡ�

TurboInterpreter_S: Pure Pascal ʵ�֣��ݻ�
TurboInterpreter: ����x86ָ���Ż�������ָ����ʵ�֣��Ĵ�������x86�ļĴ�������Ӧ��ϵ���£�
ESP: ���ض�ջָ��.��סѹ����٣��������ӵ�ַ��
EBP: ����ջָ�룬��ַָ������ڴ�ĳ����Ԫ�С�����EBP����ָ���ջ����
EBX: Ϊ����ջջ���� 
ESI: ָ��ǰָ���ַ
EDX: ״̬�Ĵ���(������������ָ�TurboScript_FullSpeed����ʱʹ�ã�����Ϊ��ʱ�Ĵ���)��(0Bit: �Ƿ����У�1Bit:�Ƿ����) TTurboForthProcessorStates = set of TTurboForthProcessorState; TTurboForthState = (psLoaded, psRunning, tfsDebugging, tfsCompiling)
     ״̬�Ĵ������ڱ����ڴ����ˣ�Ϊ���ܿ���ֹͣ����������һ��ִ�������½���3����������������ָ�TurboScript_FullSpeed������ʱ������ EDX ��Ϊ״̬�Ĵ�����
EAX: W Register ��ʱ�Ĵ���
ECX: ��ʱ�Ĵ���


EDI: FMemory��ַ

������PUSHAD ����Щͨ�üĴ��������ڶ�ջ������������ϵͳ�Ĺ���ʱ���á�Ȼ��POPAD.
�����ҵ����⣬��Щ���Ĺ������÷���ʵ�ֻ��Ǻ�������ʵ�֣����޲����Ĺ���ʵ�֡�


����ʲô��ʽ THREADING TECHNIQUE ��ʵ���أ����ں�������ָ����ò���ֵ�ķ�ʽ���û��Զ���Word������Ե�ַ��������ռ���˴�����ǰ�������1024���ֽڣ����Ե�ַ������С��255����ʾ��
��ô�ҵĺ��������������أ�ȫ�ֱ�������ʽ��

�û��Զ���Word ʵ���� THREADING TECHNIQUE ������DTC(Direct Threaded Code) ģʽ���������а취�����Ƿ���VM����ָ�������Ե�ַ��
����ĵģ�����Ҫ��Forthָ�����ΪVM����ָ��ʵ���ˣ�Next, Enter, Exit

�û��Զ���word:���У� û��Enter�ˣ�ֻ��Exit.

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
  ADD  EAX, [EBP] //ָ���û������word���
  JMP  iVMEnter
@@Exit:

�ɴ಻��CALL ȫ�� jmp �ȽϺã�Ȼ��ÿһ�������һ��JMP vmNext

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

Forth�������ڴ澵��(TCustomTurboExecutor.Memory)��
FParameterStackBase(Pointer:������ջ��ַ) FParameterStackSize(Integer:������ջ��С)  
ReturnStackBase(Pointer: ����ջ��ַ) ReturnStackSize(Integer: ����ջ��С)
TIBLength(Integer) ToIn(Integer) TIB(PChar: 1024) LastWordEntry(Pointer: �û��Զ��嵥�������)
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
    //���ModuleType��ģ�飬��ô����װ�����и�ģ��ǰִ�еĳ�ʼ�����̣���ڵ�ַ
    //����Ǻ��������Ǹú�������ڵ�ַ
    InitializeProc: Pointer; //it is the offset address of the FMemory
    FinalizeProc: Pointer; //�����ģ��Ļ�
    //last Used(import) module entry.
    LastModuleEntry: PTurboModuleEntry;
    //�����ֵĺ�������ָ�����һ��������ڡ�
    LastWordEntry: PTurboWordEntry;
    //�����ֵı�������
    LastVariableEntry: PTurboVariableEntry;
    //RTTI TypeInfo ����
    LastTypeInfoEntry: PTurboTypeInfoEntry;
    //reserved: array [SizeOf() ] of byte; 
  end;

����VM���룺
2 3 +

pushInt 2 pushInt 3 AddInt Halt 

�������� QueryPerformanceCounter ��� 6-8 QueryPerformanceCounter.
������������Ҫ 400-800.QueryPerformanceCounter. �ҵ������ǵ����� ((1/6) / (1/400)) * 100 % = 6666 %��
��delphi �� a:= 2+3 ������Ҫ��ʱ�� �� 4-7, ֻ��Delphi��( (1- (1/16) / (1/9)) * 100 % = 43.75%)��
�����������ڴ���Ϊ״̬�Ĵ�����Ϊ����ֹͣ���������½���. ��Delphi���� �� 1- (1/18) / (1/9) = 50% ProcessorStates = 1 byte�� �� 1 - (1/36)/(1/9) = 75% ProcessorStates = 1 word ��

Luna5: Delphi 1-((1/6.57)/(1/0.61))= 1-0.0928 ֻ��Delphi ���ܵ� 9.28%,��delphi����90.72%.


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

��������������������FMemory�С�
���Կ����Ǹ�executor�ľֲ����������������
����ı������������������֣�Ҳ����û�С������ֵ����������ʽ�ۼ���һ��

�������ָ�����ڵ�ַ��Ϊ CALL ��ڵ�ַ����
��ʽ1��ָ������ַָ��ĳ���һ����������ռ���˴�����ǰ�������1024���ֽڣ����Ե�ַ������С��255�����<256��Ϊ����ָ�����Ϊ��ڵ�ַ��
��ʽ2��ָ���Ϊ1�ֽڣ�ר������һ��ָ��CALL,��������û����嵥�ʵ���ڵ�ַ��

���÷�ʽ1�����ɣ�
  1����Լ�ڴ棬ϵͳ��������Ҫ�����û��Զ��嵥����
  2����������ϵͳָ���ʽ2���ֻ����256��ָ������޷����䡣

�ļ�֧�ֲ�: 
  uTurboModuleAccessor(ģ��װ�뱣�����); 
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


uTurboModuleAccessor(ģ��װ�뱣���������):

TTurboModuleAccessor: abstract class  <-- TTurboModuleFileAccessor, TTurboModuleDataSetAccessor
TTurboModuleAccessorClasses: register the accessor class to here.

TTurboModuleManager: manage the Turbo Module Accessores.
  Require(aModuleName: string): TCustomTurboExecutor; //find and load the module into memory.

����˵����
RegisterTurboModuleAccessor(aClass: TTurboModuleAccessorClass);

uTurboModuleFileAccessor

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

�ļ�ͷ��ʽ��
  TTurboModuleStreamHeader = packed record
    Id: array [0..cFORTHHeaderMagicIdLen-1] of char; //MagicWord: ����Ƿ�ΪTurboScript�ļ���ʽ��
    Version: LongWord;
    BuildDate: TTimeStamp;
  end;

�����ŵ����ݾ��ǣ�Forth�������ڴ澵��(TCustomTurboExecutor.Memory)��

