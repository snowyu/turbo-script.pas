�ҵ������(Virtual Machine)˵����

���˵��:

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
TCustomProcessor = Class
end;

{ Summary : the Abstract stack-oriented Virtual Machine processor }
{ Description :
  Chinese 
    ���ڶ�ջ�����������������
}
TStackProcessor = Class(TCustomProcessor)
end;

{ Summary : the Forth Virtual Machine processor }
{ Description :
  Chinese 
    ���ڶ�ջ��Forth�����������
}
TForthProcessor = Class(TStackProcessor)

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


 2. ���ض�ջ��
 3. ����������
 4. �ڲ�������(Procs): ������Щ�̶��Ļ���ָ�����/

FORTH����������������������������������:
 4. ���ݶ�ջ��