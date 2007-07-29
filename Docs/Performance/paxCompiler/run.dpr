program run;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils
  , PaxCompiler, PaxProgram
  ;

function Int64ToStr(Value: Int64): string;
begin
  Result := IntToStr(Value);
end;

function GetCount: Int64;
begin
  QueryPerformanceCounter(Result);
end;

procedure WriteOut(const aStr: string);
begin
  Writeln(aStr);
end;

var
  CountFreq: Int64;


procedure srun(const aFileName: string);
var
  PaxCompiler1: TPaxCompiler;
  PaxPascalLanguage1: TPaxPascalLanguage;
  PaxProgram1: TPaxProgram;
  vBegin, vEnd: Int64;
  i: integer;
begin
  if FileExists(aFileName) then
  begin
    PaxCompiler1 := TPaxCompiler.Create(nil);
    PaxPascalLanguage1:= TPaxPascalLanguage.Create(nil);
    PaxProgram1:= TPaxProgram.Create(nil);
    try
      PaxCompiler1.Reset;
      PaxCompiler1.RegisterLanguage(PaxPascalLanguage1);
      PaxCompiler1.RegisterHeader(0, 'function IntToStr(Value: Int64): string;', @Int64ToStr);
      PaxCompiler1.RegisterHeader(0, 'function StrToInt(const S: string): Integer;', @StrToInt);
      PaxCompiler1.RegisterHeader(0, 'function GetCount: Int64;', @GetCount);
      PaxCompiler1.RegisterHeader(0, 'procedure ShowMessage(const Msg: string);', @WriteOut);
      PaxCompiler1.AddModule('1', 'Pascal');
      PaxCompiler1.AddCodeFromFile('1', aFileName);
      if PaxCompiler1.Compile(PaxProgram1) then
      begin
        QueryPerformanceCounter(vBegin);
        PaxProgram1.Run;
        QueryPerformanceCounter(vEnd);
        vEnd := vEnd - vBegin;
        Writeln('ScriptExecTime(',vEnd,'):',vEnd/CountFreq*1000*1000 :8:4, ' (us)');
      end
      else begin
        for I:=0 to PaxCompiler1.ErrorCount - 1 do
          with PaxCompiler1 do Writeln(IntToStr(ErrorLineNumber[I]) + ' Line:'+ ErrorMessage[I]);
      end;
    finally
      PaxCompiler1.Free;
      PaxPascalLanguage1.Free;
      PaxProgram1.Free;
    end;
  end
  else 
    Writeln('Invalid filename:', aFileName);
end;

begin
  if ParamCount = 0 then Writeln('need source filename to run.');
	QueryPerformanceFrequency(CountFreq);
  
  sRun(paramStr(1));
end.
