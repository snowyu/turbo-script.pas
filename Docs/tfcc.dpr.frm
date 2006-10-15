$M"tfcc" /* the module file base name */
{: Turbo Forth Command Line Complier.}
program -->MODULENAME<--;
{$APPTYPE CONSOLE}

uses
  SysUtils,
  uTurboConsts,
  -->Grammar<--;

const
  ResultStr = 'Results can be found in ';
  cProgCopyright = '-->MODULENAME<-- 1.0 Writen by Riceball<riceballl@hotmail.com>';
  cProgTitle     = 'Turbo Forth Command Line Complier';

type
  TDisplayObj = class(TObject)
  private
    function CustomErrorEvent(Sender : TObject; const ErrorCode : integer; 
      const Data : string) : string;
    procedure OnSuccess(Sender : TObject);
    procedure OnFailure(Sender : TObject; NumErrors : integer);
  end; // DisplayObj

var
  -->Grammar<--1 : T-->Grammar<--;
  DisplayObj : TDisplayObj;

{ TDisplayObj }

function TDisplayObj.CustomErrorEvent(Sender: TObject;
  const ErrorCode: integer; const Data : string): string;
begin
  Result := 'Error: ' + IntToStr(ErrorCode);
end;

procedure TDisplayObj.OnSuccess(Sender : TObject);
begin
  Writeln('Compile sucessful');
  Writeln(ResultStr + ChangeFileExt(ParamStr(1),'.lst'));
end;

procedure TDisplayObj.OnFailure(Sender : TObject; NumErrors : integer);
begin
  Write('Compile completed with ' + IntToStr(NumErrors) + ' error');
  if NumErrors <> 1 then
    Writeln('s')
  else
    Writeln;
  Writeln(ResultStr + ChangeFileExt(ParamStr(1),'.lst'));
end;

procedure ShowVersion;
begin
  //Writeln('');
  //Write('-->Grammar<--');
  Write(cProgTitle);
  -->Console_Version<--
  Writeln;
end;

procedure ShowHelp;
begin
  //Writeln(cProgTitle);
  //Writeln(cProgCopyright);
  //Writeln('');
  Writeln('Usage: -->MODULENAME<-- [filename]');
  Writeln('Example: -->MODULENAME<-- Test.tf');
end;

Var
  aFilename: String;
begin
  ShowVersion;
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;
  -->Grammar<--1 := T-->Grammar<--.Create(nil);
  try
    DisplayObj := TDisplayObj.Create;
    try
      aFilename := ParamStr(1);
      if ExtractFileExt(aFileName) = '' then 
        aFileName := aFileName + cTurboForthFileExt;
      if NOT FileExists(aFilename) then
      begin
        Writeln('File: ' + aFileName + ' not found.');
        Exit;
      end;
      -->Grammar<--1.OnCustomError := DisplayObj.CustomErrorEvent;
      -->Grammar<--1.OnSuccess := DisplayObj.OnSuccess;
      -->Grammar<--1.OnFailure := DisplayObj.OnFailure;

      -->Grammar<--1.SourceFileName := aFileName;
      -->Grammar<--1.Execute;
      -->Grammar<--1.ListStream.SaveToFile(ChangeFileExt(ParamStr(1),'.lst'));
    finally
      DisplayObj.Free;
    end;
  finally
    -->Grammar<--1.Free;
  end;

end.

