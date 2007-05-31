$M"tfcc" /* the module file base name */
{: Turbo Forth Command Line Complier.}
program -->MODULENAME<--;
{$APPTYPE CONSOLE}

uses
  {$IFNDEF FPC}
  FastMM4,
  {$ENDIF}
  SysUtils,
  uTurboConsts,
  Cocobase,
  -->Grammar<--;

const
  sErrorDetail = 'Error Detail can be found in ';
  cProgCopyright = '-->MODULENAME<-- 1.0 Copyright(c) 2006 Riceball LEE<riceballl@hotmail.com>';
  cProgTitle     = 'Turbo Forth Command Line Complier';

type
  TDisplayObj = class(TObject)
  private
    function CustomErrorEvent(Sender : TObject; const ErrorCode : integer; 
      const Data : string) : string;
    procedure OnSuccess(Sender : TObject);
    procedure OnError(Sender : TObject; Error : TCocoError);
    procedure OnFailure(Sender : TObject; NumErrors : integer);
  end; // DisplayObj
  TCocoRScannerAccess = class(TCocoRScanner);

var
  -->Grammar<--1 : T-->Grammar<--;
  DisplayObj : TDisplayObj;
  GFileName: String;
  ReqMakeErrListFile: Boolean;

{ TDisplayObj }

function TDisplayObj.CustomErrorEvent(Sender: TObject;
  const ErrorCode: integer; const Data : string): string;
begin
  Result := 'Error(' + IntToStr(ErrorCode) + ') ';
  If Data <> '' then Result := Result + ':'+ Data;
end;

procedure TDisplayObj.OnSuccess(Sender : TObject);
begin
  Writeln('Compile sucessful');
  //Writeln(sErrorDetail + ChangeFileExt(GFileName,'.lst'));
end;

procedure TDisplayObj.OnError(Sender : TObject; Error : TCocoError);
Var
  s: String;
  errStr: String;
begin
  With Error Do
  Begin
    s := T-->Grammar<--(Sender).ErrorStr(ErrorCode, Data);
    If Data <> '' then  s := s + ' --- '+ Data;
    errStr := Format('at line %d, position %d: %s',  [Line, Col - 1, S]);
    s := GFileName + '(' + IntToStr(Line) + ')';
    Case ErrorType of
      etSyntax: s := s + ' Syntax Error(E'+ IntToStr(ErrorCode) + ')';
      etSymantic:s := s + ' Symantic Error(E'+ IntToStr(ErrorCode) + ')';
      etWarn: s := s + ' Warn  ';
      etHint: s := s + ' Hint  ';
    End;
    s := s + ' ' + errStr;
  End;
  Writeln(s);
End;

procedure TDisplayObj.OnFailure(Sender : TObject; NumErrors : integer);
begin
  Write(GFileName+'(', TCocoRScannerAccess(T-->Grammar<--(Sender).Scanner).CurrLine, ') Fatal Error: Compile fatal with ' + IntToStr(NumErrors) + ' error');
  if NumErrors <> 1 then
    Writeln('s')
  else
    Writeln;
  If ReqMakeErrListFile Then
  Begin
    Writeln(sErrorDetail + ChangeFileExt(GFileName,'.lst'));
    T-->Grammar<--(Sender).ListStream.SaveToFile(ChangeFileExt(GFileName,'.lst'));
  End;
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
  Writeln('Usage: -->MODULENAME<-- filename[.tf] [-ge]');
  Writeln('Options:');
  Writeln('-ge generate error detail list file if nay.');
  Writeln('Example: -->MODULENAME<-- Test.tf');
end;

begin
  ShowVersion;
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;
  GFileName := ParamStr(1);
  If ParamCount >= 2 Then
  	ReqMakeErrListFile := AnsiSameText(ParamStr(2), '-ge');
  -->Grammar<--1 := T-->Grammar<--.Create(nil);
  try
    DisplayObj := TDisplayObj.Create;
    try
      if ExtractFileExt(GFileName) = '' then 
        GFileName := GFileName + cTurboForthFileExt;
      if NOT FileExists(GFileName) then
      begin
        Writeln('File: ' + GFileName + ' not found.');
        Exit;
      end;
      -->Grammar<--1.OnCustomError := DisplayObj.CustomErrorEvent;
      -->Grammar<--1.OnSuccess := DisplayObj.OnSuccess;
      -->Grammar<--1.OnFailure := DisplayObj.OnFailure;
      -->Grammar<--1.OnError   := DisplayObj.OnError;

      -->Grammar<--1.SourceFileName := GFileName;
      try
        -->Grammar<--1.Execute;
      except
      End;
    finally
      DisplayObj.Free;
    end;
  finally
    -->Grammar<--1.Free;
  end;

end.

