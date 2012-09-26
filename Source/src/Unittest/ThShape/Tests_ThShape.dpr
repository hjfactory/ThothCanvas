program Tests_ThShape;
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
  FMX.Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  FMX.TestLib in '..\lib\FMX.TestLib.pas',
  FMX.TestLib.Win in '..\lib\FMX.TestLib.Win.pas',
  FMX.TestLib.Mac in '..\lib\FMX.TestLib.Mac.pas',
  ThShape in '..\..\Libraries\ThShape.pas',
  TestThLine in '..\TestThLine.pas';

{R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

