program tmscc;

uses
  Windows,
  //Forms,
  //Dialogs,
  SysUtils,
  atScript,
  atPascal,
  {$IFNDEF VER130}
  Variants,
  {$ENDIF}
  uScriptLib;

{$APPTYPE CONSOLE}
  
var
  AFile: string;
  ARoutine: string;
  AScr: TatPascalScripter;
  AResult: Variant;
  tBegin, tEnd: Int64;
  CountFreq: Int64;

{$IFDEF VER130}
function AnsiDequotedStr(S: string; Quote: Char): string;
var
  TempS: PChar;
begin
  TempS := PChar(S);
  result := AnsiExtractQuotedStr(TempS, Quote);
end;
{$ENDIF}

begin
	QueryPerformanceFrequency(CountFreq);

  AScr := TatPascalScripter.Create(nil);
  try
    try
      AResult := 0;
      AddScripterLibraries(AScr);
      if ParamCount >= 1 then
      begin
        AFile := ParamStr(1);
        AFile := AnsiDequotedStr(AFile, '"');
        if FileExists(AFile) then
        begin
          AScr.SourceCode.LoadFromFile(AFile);
    QueryPerformanceCounter(tBegin);
          AScr.Compile;
    QueryPerformanceCounter(tEnd);
    tEnd := tEnd - tBegin;
    writeln('ScriptCompileTime(',tEnd,'):',tEnd/CountFreq*1000*1000 :8:4, ' (us)');
          if ParamCount >= 2 then
            ARoutine := ParamStr(2)
          else
            ARoutine := '';
          ARoutine := AnsiDequotedstr(ARoutine, '"');
    QueryPerformanceCounter(tBegin);
          if ARoutine = '' then
            AResult := AScr.Execute
          else
            AResult := AScr.ExecuteSubRoutine(ARoutine);
    QueryPerformanceCounter(tEnd);
    tEnd := tEnd - tBegin;
    writeln('ScriptExecTime(',tEnd,'):',tEnd/CountFreq*1000*1000 :8:4, ' (us)');
        end else
          WriteLn('Script file not found.');
      end else
      begin
        WriteLn('No script specified on the command line.');
      end;
      if not VarIsEmpty(AResult) and not VarIsNull(AResult)
        {$IFNDEF VER130}
        and not VarIsClear(AResult)
        {$ENDIF}
        then
          writeln('the result:', AResult);
    except
      On E:Exception do
        writeln('Error:', E.Message);
    end;
  finally
    AScr.Free;
  end;
end.
