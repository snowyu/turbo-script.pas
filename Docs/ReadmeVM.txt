我的虚拟机(Virtual Machine)说明：

{ Summary : The Abstract Virtual Machine Processor }
{ Description :
  Chinese 
    抽象虚拟机处理器抽象类
    拥有一个IR(instruction register)指令寄存器, 一个SP堆栈指针寄存器, 一个BP堆栈基址指针寄存器, 一个PC程序指针寄存器，一个状态寄存器.
    可以执行Execute和停止(Stop)。
  TODO
    是否需要加入调试功能？
}
TCustomProcessor = Class
end;

{ Summary : the Abstract stack-oriented Virtual Machine processor }
{ Description :
  Chinese 
    基于堆栈虚拟机处理器抽象类
}
TStackProcessor = Class(TCustomProcessor)
end;

{ Summary : the Forth Virtual Machine processor }
{ Description :
  Chinese 
    基于堆栈的Forth虚拟机处理器
}
TForthProcessor = Class(TStackProcessor)