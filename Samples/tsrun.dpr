{: Turbo Script command line Console intercepter}
{ Description
  开关：
  -d: 显示调试信息（显示数据栈和返回栈数据）
    首先试图装入资源（'SCRIPT', 'MAIN'）中装入
    不行则查找参数行，看有没有文件名。
}
program tsrun;

{$APPTYPE CONSOLE}

uses
  //FastMM4,
  Windows,  SysUtils, Classes
  , uTurboPE
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

const
  cRunTimeErrors : array [TTurboProcessorErrorCode] of string =
  (
    ''
    , 'Bad Instruction'
    , 'DizZero'
    , 'ModuleNotFound'
    , '代码区内存无可用的空间'
    ,'MetaData区已无可用的空间'
    , 'OutOfDataStack'
    , 'errOutOfReturnStack'
    , 'Assertion Failed'
  );

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
  cScriptResType = 'SCRIPT';
  cScriptResName = 'MAIN';



type
  TTurboExecutorOption  = (eoShowDebugInfo, eoShowHelp, eoInternalRun, eoDisableInternalRun);
  TTurboExecutorOptions = set of TTurboExecutorOption;

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
  LastAddr: tsInt;
  aFileName: string;
  s: string;
  CountFreq: Int64;
  vExeOptions: TTurboExecutorOptions;
  vStream: TStream;
  vSize: LongWord;


procedure Help;
begin
  Writeln('');
  Write('Usage:   ', ExtractFileName(ParamStr(0)));
  if not (eoInternalRun in vExeOptions) then
  else
    Writeln('<afile.'+cTurboCompiledProgramFileExt+'>');
  Writeln('         run the compiled turbo-script program.');
  Writeln('-d  show debug info: the param/return stack data if any.');
  Writeln('-?  show help.:');
  if not (eoInternalRun in vExeOptions) then
  begin
    Writeln('Example:');
    Writeln('       ',ExtractFileName(ParamStr(0)),' test.tpc');
  end;
end;

procedure TryLoadFromRes;
begin
  p := ReadResourceToPointer(HInstance, cScriptResName, vSize, cScriptResType);
  if Assigned(p) then
  begin
    //vStream := TStaticMemoryStream.Create(p, vSize);
    Include(vExeOptions, eoInternalRun);
  end;
end;

procedure AddSwitch(const aSwitch: string);
begin
  if aSwitch = 'D' then 
    Include(vExeOptions, eoShowDebugInfo)
  else if (aSwitch = 'H') or (aSwitch = '?') then 
    Include(vExeOptions, eoShowHelp)
  else if (aSwitch = 'I') then 
    Include(vExeOptions, eoDisableInternalRun)
  ;
end;

procedure ExecuteScript;
begin
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
        if (eoShowDebugInfo in vExeOptions) and not (eoInternalRun in vExeOptions) then
        begin 
          writeln(aFileName + ' loading...');
          writeln('');
        end;
        LoadFromStream(vStream);
        FreeAndNil(vStream);
        //Reset;
        CFA := InitializeProc;
        //AddIntToMem(Integer(inMULUnsignedInt));
        QueryPerformanceCounter(tBegin);
        ExecuteCFA(CFA);
        QueryPerformanceCounter(tEnd);
        c := c + tEnd - tBegin;
        lastErr := LastErrorCode;
        LastAddr := tsInt(GGlobalOptions.ErrorAddr);// - tsInt(Memory);

        Integer(P) := SP;
        if (eoShowDebugInfo in vExeOptions) and (Integer(P) < (Integer(ParameterStack)+cStackMaxSize-SizeOf(Integer)))  then
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
        if (eoShowDebugInfo in vExeOptions) and (Integer(P) < (Integer(ReturnStack)+cStackMaxSize)) then
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
end;

begin
	QueryPerformanceFrequency(CountFreq);
	vStream := nil;
  if eoShowDebugInfo in vExeOptions then
    Writeln(Copyright);
  for i := 1 to ParamCount do
  begin
  	s := ParamStr(i);
    if (s[1] = '-') or (s[1] = '/') then
    begin
      Delete(s, 1, 1); 
      AddSwitch(UpperCase(s));
    end 
    else 
      aFileName := Trim(s);
  end;

  if eoShowHelp in vExeOptions then
  begin
    Help;
    exit;
  end;

  if not (eoDisableInternalRun in vExeOptions) then TryLoadFromRes;

  if eoInternalRun in vExeOptions then
  begin
    vStream := TStaticMemoryStream.Create(p, vSize);
  end
  else
  begin
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
    vStream := TFileStream.Create(aFileName, fmOpenRead);
  end;
  try
    ExecuteScript();
  finally
    FreeAndNil(vStream);
  end;

  if lastErr <> errNone then
  begin
    WriteLn('');
    WriteLn('');
    write('lasterr(', Integer(lasterr) ,'):', cRunTimeErrors[lasterr], ' at address:'+ IntToHex(LastAddr, 4));
    WriteLn('');
  end;
  if eoShowDebugInfo in vExeOptions then 
  begin
    WriteLn('');
    writeln('ScriptExecTime(',c,'):',c/CountFreq*1000, ' (ms)');
  end;
  aFileName := '';
  s := '';
end.
