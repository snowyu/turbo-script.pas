{: Turbo Script command line Console intercepter}
{ Description
  开关：
  -d: 显示调试信息（显示数据栈和返回栈数据）
    首先试图装入资源（'SCRIPT', 'MAIN'）中装入
    不行则查找参数行，看有没有文件名。
}
program tsrun;

{$I TurboScript.inc}
{$DEFINE PUREPASCAL}

{$APPTYPE CONSOLE}

uses
  {$IFNDEF FPC}
  FastMM4,
  {$ENDIF}
  Windows,  SysUtils, Classes
  , uTurboPE
  , uTurboConsts
  , uTurboExecutor
  , uTurboModuleFileAccessor
  {$IFDEF PUREPASCAL}
  , TurboInterpreter
  {$ELSE}
  , TurboX86Interpreter
  {$ENDIF}
  ;

type
  TConsoleInterpreter = Class({$IFDEF PUREPASCAL}TTurboInterpreter{$ELSE}TTurboX86Interpreter{$ENDIF})
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

procedure TConsoleInterpreter.DoPrintChar(aChar: Char);
begin
	 Write(aChar);
   //System.Write('1');
end;

procedure TConsoleInterpreter.DoPrintString(const aStr: String);
begin
	 Write(aStr);
end;

const
  Copyright = 'Turbo Script command line intercepter 1.0'{$IFDEF FPC}+'(FPC Edit)'{$ENDIF}+#13#10'    Copyright(c) by Riceball<riceballl@hotmail.com>';
  cScriptResType = 'SCRIPT';
  cScriptResName = 'MAIN';



type
  TTurboExecutorOption  = (eoShowDebugInfo, eoShowHelp, eoInternalRun, eoDisableInternalRun);
  TTurboExecutorOptions = set of TTurboExecutorOption;

const
  cStackMaxSize = 1024 * 10;
var
  GTurboAppDomain: TTurboAppDomain;
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
  Writeln(Copyright);
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
    GTurboAppDomain := TTurboAppDomain.Create;
    try
      GTurboAppDomain.ExecutorClass := TConsoleInterpreter;
      with GTurboAppDomain do
      begin
        if (eoShowDebugInfo in vExeOptions) and not (eoInternalRun in vExeOptions) then
        begin
          writeln(aFileName + ' loading...');
          writeln('');
        end;
        Memory.LoadFromStream(vStream);
        FreeAndNil(vStream);
        QueryPerformanceCounter(tBegin);
        Execute();
        QueryPerformanceCounter(tEnd);
        c := c + tEnd - tBegin;
        lastErr := LastErrorCode;
        LastAddr := tsInt(GlobalOptions.ErrorAddr);// - tsInt(Memory);

        P := Executor.SP;
        if (eoShowDebugInfo in vExeOptions) and (Integer(P) < GlobalOptions.ParamStackBottom)  then
        begin
        WriteLn('');
        WriteLn('______________________________');
        WriteLn('The ParameterStack Data :');
        i := 0;
        //Write('':2);
        while Integer(P) < GlobalOptions.ParamStackBottom do
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

        P := Executor.RP;
        if (eoShowDebugInfo in vExeOptions) and (Integer(P) < GlobalOptions.ReturnStackBottom) then
        begin
        WriteLn('______________________________');
        WriteLn('The ReturnStack Data :');
        i := 0;
        //Write('  ');
        while Integer(P) < GlobalOptions.ReturnStackBottom do
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
      FreeAndNil(GTurboAppDomain);
    end;
end;

begin
	QueryPerformanceFrequency(CountFreq);
	vStream := nil;
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
  s := ''; //free string

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

  if eoShowDebugInfo in vExeOptions then
    Writeln(Copyright);

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
    writeln('ScriptExecTime(',c,'):',c/CountFreq*1000*1000 :8:4, ' (us)');
  end;
  aFileName := '';
  s := '';
end.
