{1 the fastest script
        ����x86ָ���Ż�������ָ����ʵ�֣��Ĵ�������x86�ļĴ���.��Ӧ��ϵ���£�
        }
{{
��Ӧ��ϵ���£�
ESP,EBP: ���ض�ջ�� EAX, EDX, ECX Ϊ���ض�ջջ������ջ����...
ESI����ַָ�룩, EBX��ջָ�룩: ����ջ
EDI: ָ��ǰָ���ַ
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
