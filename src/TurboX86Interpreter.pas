{1 the fastest script
        基于x86指令优化。核心指令汇编实现，寄存器采用x86的寄存器.对应关系如下：
        }
{{
对应关系如下：
ESP,EBP: 返回堆栈： EAX, EDX, ECX 为返回堆栈栈顶，次栈顶，...
ESI（基址指针）, EBX（栈指针）: 数据栈
EDI: 指向当前指令地址
}
unit TurboInterpreter;

interface

{$I Setting.inc}

uses
  SysUtils, Classes
  , uTurboScriptConsts
  , uTurboExecutor
  ;

type
  TTurboX86Interpreter = class(TCustomTurboExecutor)
  protected
    function ExecuteCFA(const aCFA: Integer): Integer; override;
    procedure Init; override;
  public
    destructor Destroy; override;
  end;
  

implementation

{----Helper functions ----}

{
***************************** TTurboX86Interpreter *****************************
}
destructor TTurboX86Interpreter.Destroy;
begin
  inherited Destroy;
end;

function TTurboX86Interpreter.ExecuteCFA(const aCFA: Integer): Integer;
begin
  Result := inherited ExecuteCFA(aCFA);
end;

procedure TTurboX86Interpreter.Init;
begin
  inherited Init;
end;


end.
