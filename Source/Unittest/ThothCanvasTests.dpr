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
  ThCanvas in '..\src\Libraries\ThCanvas.pas',
  ThCanvasEditor in '..\src\Components\ThCanvasEditor.pas',
  UnitTestForm in 'lib\UnitTestForm.pas' {frmUnitTest},
  FMX.TestLib in 'lib\FMX.TestLib.pas',
  ThItem in '..\src\Libraries\ThItem.pas',
  ThShape in '..\src\Components\ThShape.pas',
  ThItemFactory in '..\src\Components\ThItemFactory.pas',
  TestThItemRectangle in 'TestThItemRectangle.pas',
  BaseTestUnit in 'lib\BaseTestUnit.pas',
  DebugUtils in '..\src\Utils\DebugUtils.pas',
  ThItemHighlighter in '..\src\Components\ThItemHighlighter.pas',
  ThConsts in '..\src\Libraries\ThConsts.pas',
  ThItemResizer in '..\src\Components\ThItemResizer.pas',
  TestThItemRectangleResizer in 'TestThItemRectangleResizer.pas',
  TestThItemLine in 'TestThItemLine.pas',
  ThTypes in '..\src\Libraries\ThTypes.pas',
  ResizeUtils in '..\src\Utils\ResizeUtils.pas',
  TestThItemCircle in 'TestThItemCircle.pas',
  TestThItemControl in 'TestThItemControl.pas',
  TestThCommandHistory in 'TestThCommandHistory.pas',
  ThClasses in '..\src\Libraries\ThClasses.pas',
  ThothController in '..\src\Controllers\ThothController.pas',
  ThCanvasController in '..\src\Controllers\ThCanvasController.pas',
  ThItemCommand in '..\src\Commands\ThItemCommand.pas',
  ThCommandManager in '..\src\Commands\ThCommandManager.pas',
  ThItemStorage in '..\src\Controllers\ThItemStorage.pas',
  ThSystemCommand in '..\src\Commands\ThSystemCommand.pas',
  TestThCanvasZoom in 'TestThCanvasZoom.pas',
  ThZoomAnimation in '..\src\Components\ThZoomAnimation.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

