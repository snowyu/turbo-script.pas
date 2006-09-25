program test;

{$APPTYPE CONSOLE}

uses
  Windows,  SysUtils
  , uTurboScriptConsts
  , uTurboExecutor
  , TurboInterpreter
  ;
const
  cStackMaxSize = 1024 * 10;
var
  GTurboExecutor: TTurboX86Interpreter;
  p: Pointer;
  CFA: Integer;
  tBegin, tEnd: Int64;
  i: integer;
  c: int64;
  r: integer;
  lastErr: TTurboForthProcessorErrorCode;
begin
  c := 0;
    GTurboExecutor := TTurboX86Interpreter.Create;
    try
      GTurboExecutor.ParameterStackSize := cStackMaxSize;
      GetMem(p, cStackMaxSize);
      GTurboExecutor.ParameterStack := p;
      GTurboExecutor.ReturnStackSize := cStackMaxSize;
      GetMem(p, cStackMaxSize);
      GTurboExecutor.ReturnStack := p;
      //GTurboExecutor.InitExecution;
      with GTurboExecutor do
      begin
        CFA := UsedMemory;
        IsLoaded := True;
        Reset;
        //AddIntToMem(Integer(inNone));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(12);
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        AddIntToMem(Integer(inPushInt));
        AddIntToMem(300);
        AddIntToMem(Integer(inAddInt));
        //}
        AddIntToMem(Integer(inHalt));
        //AddIntToMem(Integer(inMULUnsignedInt));
        QueryPerformanceCounter(tBegin);
        ExecuteCFA(CFA);
        QueryPerformanceCounter(tEnd);
        c := c + tEnd - tBegin;
        lastErr := LastErrorCode;
        r := PInteger(SP)^;
        FreeMem(ParameterStack);
        ParameterStack := nil;
        FreeMem(ReturnStack);
        ReturnStack := nil;
      end;
    finally
      FreeAndNil(GTurboExecutor);
    end;

  if lastErr <> errNone then
    writeln('lasterr=', Integer(lasterr));
  writeln('ScriptExecTime:',c,'; Result=', r);
  c := 0;
    CFA := 12;
    QueryPerformanceCounter(tBegin);
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    CFA:= CFA+300;
    QueryPerformanceCounter(tEnd);
    c := c + tEnd - tBegin;

  writeln('DelphiExecuteTime:',c, '; Result=', CFA);
end.
