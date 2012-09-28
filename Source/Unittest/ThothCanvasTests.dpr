program ThothCanvasTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  TestThCanvasEditor in 'TestThCanvasEditor.pas',
  ThContainer in '..\src\Libraries\ThContainer.pas',
  ThCanvasEditor in '..\src\Components\ThCanvasEditor.pas',
  UnitTestForm in 'lib\UnitTestForm.pas' {frmUnitTest},
  FMX.TestLib in 'lib\FMX.TestLib.pas',
  FMX.TestLib.Mac in 'lib\FMX.TestLib.Mac.pas',
  FMX.TestLib.Win in 'lib\FMX.TestLib.Win.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

