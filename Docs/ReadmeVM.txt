�ҵ������(Virtual Machine)˵����

���˵��:

{ Summary : The Abstract Virtual Machine Processor }
{ Description :
  Chinese 
    ���������������������
    ӵ��һ��IR(instruction register)ָ��Ĵ���, һ��SP��ջָ��Ĵ���, һ��BP��ջ��ַָ��Ĵ���, һ��PC����ָ��Ĵ�����һ��״̬�Ĵ���.
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
 1. ����������
   ���ȣ���Ҫ��ָ����ȷ�Ϊ��
     1.��ָ��: û���κβ�����ָ��
     2.������ָ��: ��һ��������ָ��

   TProgramDataArea = array of TVMCode
   
 2. ���ض�ջ��
 3. ����������
 4. �ڲ�������(Procs): ������Щ�̶��Ļ���ָ�����/

FORTH����������������������������������:
 4. ���ݶ�ջ��