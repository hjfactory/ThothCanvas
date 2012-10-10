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
  ThItem in '..\src\Libraries\ThItem.pas',
  ThShape in '..\src\Components\ThShape.pas',
  ThItemFactory in '..\src\Components\ThItemFactory.pas',
  TestThRectangle in 'TestThRectangle.pas',
  BaseTestUnit in 'lib\BaseTestUnit.pas',
  CommonUtils in '..\src\Utils\CommonUtils.pas',
  ThItemHighlighterIF in '..\src\Interfaces\ThItemHighlighterIF.pas',
  ThItemHighlighter in '..\src\Components\ThItemHighlighter.pas',
  ThConsts in '..\src\Libraries\ThConsts.pas',
  ThItemResizablerIF in '..\src\Interfaces\ThItemResizablerIF.pas',
  ThItemResizabler in '..\src\Components\ThItemResizabler.pas',
  TestThItemResizabler in 'TestThItemResizabler.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

