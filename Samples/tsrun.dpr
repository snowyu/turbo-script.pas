{: Turbo Script command line intercepter}
program tsrun;

{$APPTYPE CONSOLE}

uses
  Windows,  SysUtils
  , uTurboConsts
  , uTurboExecutor
  , TurboInterpreter
  ;

const
  Copyright = 'Turbo Script command line intercepter 1.0'#13#10'    Copyright(c) by Riceball<riceballl@hotmail.com>';


procedure Help;
begin
  Writeln('');
  Writeln('Usage:   tsrun <afile.'+cTurboCompiledProgramFileExt+'>');
  Writeln('         run the compiled turbo-script program.');
  Writeln('Example:');
  Writeln('         tsrun test.tpc');
end;

procedure AddSwitch(const aSwitch: string);
begin
end;

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
  lastErr: TTurboProcessorErrorCode;
  aFileName: string;
  s: string;
  CountFreq: Int64;
begin
	QueryPerformanceFrequency(CountFreq);
  Writeln(Copyright);
  if (ParamCount < 1) then
  begin
    Help;
    exit;
  end;
  for i := 1 to ParamCount do
  begin
  	s := ParamStr(i);
    if (s[1] = '-') or (s[1] = '/') then AddSwitch(s) else aFileName := Trim(s);
  end;
  if aFileName = '' then
  begin
    Help;
    exit;
  end;
  if ExtractFileExt(aFileName) = '' then aFileName := aFileName + cTurboCompiledProgramFileExt;
  if not FileExists(aFileName) then
  begin
  	Writeln(aFileName + ' is not exists.');
  	exit;
  end;

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
        //CFA := UsedMemory;
        //IsLoaded := True;
        writeln(aFileName + ' loading...');
        LoadFromFile(aFileName);
        Reset;
        CFA := InitializeProc;
        //AddIntToMem(Integer(inMULUnsignedInt));
        QueryPerformanceCounter(tBegin);
        ExecuteCFA(CFA);
        QueryPerformanceCounter(tEnd);
        c := c + tEnd - tBegin;
        lastErr := LastErrorCode;

        Integer(P) := SP;
        if Integer(P) < (Integer(ParameterStack)+cStackMaxSize-SizeOf(Integer))  then
        begin
        WriteLn('The ParameterStack Data :');
        i := 0;
        Write('  ');
        while Integer(P) < (Integer(ParameterStack)+cStackMaxSize-SizeOf(Integer)) do
        begin
          r := PInteger(P)^;
          Inc(Integer(P), SizeOf(Integer));
          Write(r:8,'($', IntToHex(r,4), ') ');
          inc(i);
          if i mod 4 = 0 then 
          begin
            writeln('');
            Write('  ');
          end;
        end;
        WriteLn('');
        end;

        Integer(P) := RP;
        if Integer(P) < (Integer(ReturnStack)+cStackMaxSize) then
        begin
        WriteLn('The ReturnStack Data :');
        i := 0;
        Write('  ');
        while Integer(P) < (Integer(ReturnStack)+cStackMaxSize) do
        begin
          r := PInteger(P)^;
          Inc(Integer(P), SizeOf(Integer));
          Write(r:8,'($', IntToHex(r,4), ') ');
          inc(i);
          if i mod 4 = 0 then 
          begin
            writeln('');
            Write('  ');
          end;
        end;
        WriteLn('');
        end;
      end;
    finally
        FreeMem(GTurboExecutor.ParameterStack);
        //ParameterStack := nil;
        FreeMem(GTurboExecutor.ReturnStack);
        //ReturnStack := nil;
      FreeAndNil(GTurboExecutor);
    end;

  if lastErr <> errNone then
    writeln('lasterr=', Integer(lasterr));
  writeln('ScriptExecTime:',c/CountFreq*1000, ' (ms)');
end.
