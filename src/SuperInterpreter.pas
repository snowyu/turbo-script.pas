unit SuperInterpreter;

interface

uses
  SysUtils
  , uSuperScriptConsts
  , uSuperExecutor 
  ;

type
  TSuperInterpreter = class(TCustomSuperExecutor)
  protected
    FIR: TInstruction;
    FPC: Integer;
    FSP: Integer;
    FStatus: TProcessorStates;
  public
    procedure ExecuteInstruction(const aInstruction: TInstruction); virtual;
    property IR: TInstruction read FIR;
    property PC: Integer read FPC write FPC;
    property SP: Integer read FSP write FSP;
    property Status: TProcessorStates read FStatus;
  end;
  

implementation

{
****************************** TSuperInterpreter *******************************
}
procedure TSuperInterpreter.ExecuteInstruction(const aInstruction:
        TInstruction);
begin
end;


end.
