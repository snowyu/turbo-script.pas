�ҵ������(Virtual Machine)˵����

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