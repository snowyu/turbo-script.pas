{: Turbo Script command line linker }
{ Description
}
program tslink;

{$APPTYPE CONSOLE}

uses
  //FastMM4,
  Windows,  SysUtils, Classes
  , uTurboPE
  , uTurboConsts
  ;

const 
  Copyright = 'Turbo Script command line linker 1.0'#13#10'    Copyright(c) by Riceball<riceballl@hotmail.com>';
  cScriptResType = 'SCRIPT';
  cScriptResName = 'MAIN';

type
  TTurboExecutorOption  = (eoShowHelp);
  TTurboExecutorOptions = set of TTurboExecutorOption;

var
  vExeOptions: TTurboExecutorOptions;

procedure Help;
begin
  Writeln('');
  Write('Usage:   ', ExtractFileName(ParamStr(0)));
  Writeln('<afile.'+cTurboCompiledProgramFileExt+'>');
  Writeln('         link the compiled turbo-script program.to exe file');
  Writeln('-?  show help.:');
end;

procedure AddSwitch(const aSwitch: string);
begin
  if (aSwitch = 'H') or (aSwitch = '?') then 
    Include(vExeOptions, eoShowHelp)
end;

var
  vExeStream: TMemoryStream;
  vData: TMemoryStream;
  p: Pointer;
  size, handle: dword;
  i: integer;
  s, aFileName: string;
begin
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
  if (aFileName = '') then
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
  vExeStream := TMemoryStream.Create;
  vData := TMemoryStream.Create;
  try
  vData.LoadFromFile(aFileName);
  vData.Position := 0;
  vExeStream.LoadFromFile('tsrun.exe');
  //资源名称必须大写
  WriteResourceInStream(vExeStream, cScriptResName, vData.Memory, vData.Size, cScriptResType);
  vExeStream.SavetoFile(ChangeFileExt(aFileName, '.exe'));
  //vExeStream.LoadFromFile('ts1.exe');
  finally
    vExeStream.Free;
    vData.Free;
    aFileName := '';
    s := '';
  end;
end.
