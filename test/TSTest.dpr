{
  ##Project Name: TSTest
  ##Initial Date: 2006-5-4
  Summary
    TurboScript Test Suite Project.

  Description
    the TurboScript Library Test Suite 

  See Also
    ����

  Bugs
    ��֪���⡣

  Internal
    �ڲ�������Ա���ģ�������⡣

  TODO
    �������

  Author
    Riceball LEE(riceball@cq118.com)
    Riceball LEE(riceballl@hotmail.com)

  Copyright
    Copyright(C) 2006 by Riceball LEE

  Current Version
    $Revision$

  History
    �汾��ʷ��
}
program TSTest;

{$I jedi.inc}

{$DEFINE FASTMM}

uses
{$IFDEF FASTMM}    // From Project | Options | Directories/Conditionals
  {$IFNDEF COMPILER10_UP}
    {$IFNDEF CLR}
      FastMM4,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  TestFramework,
  TestExtensions,
{$IFDEF LINUX}
  QGUITestRunner,
{$ELSE}
  GUITestRunner,
{$ENDIF}
  TextTestRunner
  , uTurboModuleTest
  ;

{ NOTE:
  This program uses the test registration system.
  Units containing test cases register their test suites calling one of:
    TestFramework.RegisterTest
    TestFramework.RegisterTests
    TestFramework.RegisterTestSuites
}

{.$R *.RES}

{$APPTYPE CONSOLE}

const
  rcs_id :string = '#(@)$Id$';
  SwitchChars = ['-','/'];

procedure RunInConsoleMode;
begin
  try
    {$IFDEF MSWINDOWS}
    if not IsConsole then
      Windows.AllocConsole;
    {$ENDIF}
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  except
    on e:Exception do
      Writeln(Format('%s: %s', [e.ClassName, e.Message]));
  end;
end;

begin
  if FindCmdLineSwitch('c', SwitchChars, true) then
    RunInConsoleMode
  else begin
    {$IFDEF LINUX}
    TGUITestRunner.RunRegisteredTests;
    {$ELSE}
    GUITestRunner.RunRegisteredTests;
    {$ENDIF}
  end;
end.
