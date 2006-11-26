{: Turbo Script command line intercepter}
{ Description
  开关：
  -d: 显示调试信息（显示数据栈和返回栈数据）
}
program tsrun;

{$APPTYPE CONSOLE}

uses
  //FastMM4,
  Windows,  SysUtils
  , uTurboConsts
  , uTurboExecutor
  , uTurboModuleFileAccessor
  , TurboInterpreter
  ;

type
  TMyInterpreter = Class(TTurboX86Interpreter)
  protected
    procedure DoPrintChar(aChar: Char); override;
    procedure DoPrintString(const aStr: String); override;
  end;

procedure TMyInterpreter.DoPrintChar(aChar: Char);
begin
	 write(aChar);
end;

procedure TMyInterpreter.DoPrintString(const aStr: String);
begin
	 //writeLN('DOSting:');
	 write(aStr);
end;

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
  GGlobalOptions: TTurboGlobalOptions;
  GTurboExecutor: TMyInterpreter;
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
  vShowDebugInfo: Boolean;
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
  vShowDebugInfo := FindCmdLineSwitch('d');

  c := 0;
    GTurboExecutor := TMyInterpreter.Create;
    try
      GTurboExecutor.GlobalOptions := @GGlobalOptions;
      with GGlobalOptions do
      begin
        ParamStackSize := cStackMaxSize;
      //GTurboExecutor.ParameterStackSize := cStackMaxSize;
        GetMem(p, cStackMaxSize);
        ParamStackBase := p;
      //GTurboExecutor.ParameterStack := p;
      //GTurboExecutor.ReturnStackSize := cStackMaxSize;
        GetMem(p, cStackMaxSize);
      //GTurboExecutor.ReturnStack := p;
        ReturnStackBase := p;
        ReturnStackSize := cStackMaxSize;
      end;
      //GTurboExecutor.InitExecution;
      with GTurboExecutor do
      begin
        //CFA := UsedMemory;
        //IsLoaded := True;
        if vShowDebugInfo then writeln(aFileName + ' loading...');
        writeln('');
        LoadFromFile(aFileName);
        //Reset;
        CFA := InitializeProc;
        //AddIntToMem(Integer(inMULUnsignedInt));
        QueryPerformanceCounter(tBegin);
        ExecuteCFA(CFA);
        QueryPerformanceCounter(tEnd);
        c := c + tEnd - tBegin;
        lastErr := LastErrorCode;

        Integer(P) := SP;
        if vShowDebugInfo and (Integer(P) < (Integer(ParameterStack)+cStackMaxSize-SizeOf(Integer)))  then
        begin
        WriteLn('');
        WriteLn('______________________________');
        WriteLn('The ParameterStack Data :');
        i := 0;
        //Write('':2);
        while Integer(P) < (Integer(ParameterStack)+cStackMaxSize-SizeOf(Integer)) do
        begin
          r := PInteger(P)^;
          Inc(Integer(P), SizeOf(Integer));
          Write(r:8,'($', IntToHex(r,4), ') ');
          inc(i);
          if i mod 3 = 0 then 
          begin
            writeln('');
            //Write('':2);
          end;
        end;
        WriteLn('');
        end;

        Integer(P) := RP;
        if vShowDebugInfo and (Integer(P) < (Integer(ReturnStack)+cStackMaxSize)) then
        begin
        WriteLn('______________________________');
        WriteLn('The ReturnStack Data :');
        i := 0;
        //Write('  ');
        while Integer(P) < (Integer(ReturnStack)+cStackMaxSize) do
        begin
          r := PInteger(P)^;
          Inc(Integer(P), SizeOf(Integer));
          Write(r:8,'($', IntToHex(r,4), ') ');
          inc(i);
          if i mod 3 = 0 then 
          begin
            writeln('');
            //Write('  ');
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

  WriteLn('');
  if lastErr <> errNone then
  begin
    WriteLn('');
    writeln('lasterr=', Integer(lasterr));
  end;
  if vShowDebugInfo then 
  begin
    WriteLn('');
    writeln('ScriptExecTime(',c,'):',c/CountFreq*1000, ' (ms)');
  end;
  aFileName := '';
end.
